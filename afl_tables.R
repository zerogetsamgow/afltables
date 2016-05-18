library(xml2)
library(dplyr)
library(tidyr)
library(rvest)


# Get team, venue and state data ----
library(googlesheets)
# Run gs_auth() to set this up

gs <- gs_key("17041tChNHzRNYmi1nCCJvacOqbk19MJUzW8UVX91b_A")

teams <-  gs_read(gs, sheet = "AFL_data", ws = "Teams",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df() %>%
    mutate(is_vic = team_state=="Vic") %>%
    arrange(desc(is_vic), team)


venues <- gs_read(gs, sheet = "AFL_data", ws = "Venues",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

states <- gs_read(gs, sheet = "AFL_data", ws = "States",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

## create variable for URL of season 2016 on afltables
afltables <- read_html("http://afltables.com/afl/seas/2016.html")

# Tables are poorly arranged and not named(?)
# so create data frame of required rows using ugly loop
round_tables <-
    rbind(
        ## Rounds 1 to 12 have eight matches = eight rows
        data_frame(start_row = seq(3, 12*12, by=12),
                   end_row = start_row + 8),
        ## Rounds 13 to 15 are bye rounds, so have six matches
        data_frame(start_row = seq(147, 147 + 2*9, by=9),
                                 end_row = start_row + 5),
        ## Rounds 16 to 22 are bye rounds, so have six matches
        data_frame(start_row = seq(174, 174 + 12*12, by=12),
                                 end_row = start_row + 8))

get_table_row <- function(round_no, round_tbl) {
    ## read match data to data frame called the_table
    the_table <-
        afltables %>%
        html_nodes("table") %>%
        .[[round_tbl]] %>%
        html_table(fill = TRUE, head = FALSE)

    ## put all match data in one row and return
    cbind(round_no, the_table[1,], the_table[2,])
}

get_row_nums <- function(round_no) {
    seq(round_tables[round_no, ] %>% .[["start_row"]],
        round_tables[round_no, ] %>% .[["end_row"]])
}

get_table_rows <- function(round_no) {
    temp <- lapply(get_row_nums(round_no),
                   get_table_row, round_no = round_no)
    do.call("rbind", temp)
}

get_full_table <- function() {
    temp <- lapply(1:23, get_table_rows)
    do.call("rbind", temp)
}

season_2016 <- as_data_frame(get_full_table())

## Name rows
names(season_2016) <-
    c("round", "team_home", "quarters_home",
      "score_home", "date_venue",
      "team_away", "quarters_away", "score_away", "result")

season_2016$game_id <- 1:nrow(season_2016)

## Remove rows not required for this analysis, probably shouldn't
## do this but makes gathering easier later
games_2016 <-
    season_2016 %>%
    select(game_id, round, date_venue, result, team_home, team_away) %>%
    mutate(local_time = gsub("\\s+(\\(|Att).*$", "", date_venue),
           venue = sub(".*Venue: ", "", date_venue),
           attendance = as.integer(sub(",", "",
                            gsub("^.*Att:([0-9,]+).*$", "\\1", date_venue))),
           winner = sub(" won.*", "", result),
           loser = ifelse(winner==team_home, team_away, team_home)) %>%
    select(-result, -date_venue)

## Create data on scores
## This is ugly. There should be a better way.
scores_2016 <-
    season_2016 %>%
    select(game_id, team_home, quarters_home, score_home) %>%
    rename(team=team_home, quarters=quarters_home, score=score_home) %>%
    mutate(playing_at="home")

scores_2016 <-
    rbind(scores_2016,
    season_2016 %>%
    select(game_id, team_away, quarters_away, score_away) %>%
    rename(team=team_away, quarters=quarters_away, score=score_away) %>%
    mutate(playing_at="away"))

## Obtain the top eight teams based on latest afltables ladder
top_eight <-
    "http://afltables.com/afl/seas/ladders/laddersyby.html#2016" %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE, head = FALSE) %>%
    data.frame() %>%
    .[-c(1,2),] %>%
    .[1:8,1] %>%
    data_frame(team=.)

# Save data
save(scores_2016, games_2016, teams, venues, top_eight, file="afltables.Rdata")

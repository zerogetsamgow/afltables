library(rvest)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(readr)


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
    return(cbind(round_no, the_table[1,], the_table[2,]))

}

get_table_rows <- function(round_no) {
    temp <- lapply(seq(round_tables[round_no, 1],
               round_tables[round_no, 2]),
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
    select(game_id, round, date_venue, result) %>%
    mutate(local_time = gsub("\\s+(\\(|Att).*$", "", date_venue),
           venue = sub(".*Venue: ", "", date_venue),
           attendance = as.integer(sub(",", "",
                            gsub("^.*Att:([0-9,]+).*$", "\\1", date_venue))),
           winner = sub(" won.*", "", result)) %>%
    select(-result, -date_venue)

## Separate out quarter scores
scores_2016 <-
    season_temp %>%
    separate(quarters_home, into = paste0("q", 1:4, "_home"), sep = " ") %>%
    separate(quarters_away, into = paste0("q", 1:4, "_home"), sep = " ") %>%
    gather(playing, team, home:away) %>%

## Create data on scores
## This is ugly. There should be a better way.
scores_2016 <-
    season_2016 %>%
    select(game_id, team_home, quarters_home, score_home) %>%
    rename(team=team_home, quarters=quarters_home, score=score_home) %>%
    mutate(role="home")

scores_2016 <-
    rbind(scores_2016,
    season_2016 %>%
    select(game_id, team_away, quarters_away, score_away) %>%
    rename(team=team_away, quarters=quarters_away, score=score_away) %>%
    mutate(role="away"))

## merge teams data frame with season data frame
played_where <-
    scores_2016 %>%
    inner_join(games_2016) %>%
    ggplot(aes(x=role, fill=venue)) +
	geom_bar() +
    facet_wrap(~ team) +
	## coord_flip() +
	labs(title="Where has each team played matches this year?", x="", y="",fill="venue") +
	## scale_fill_manual(labels=c("away"="Away","home"="Home"),values=c("dark blue","red")) +
	scale_x_discrete(breaks= rev(levels(teams$team)), drop = FALSE) +
	scale_y_continuous()

## create chart of where teams have played by state
played_state <-
    scores_2016 %>%
    inner_join(games_2016) %>%
    inner_join(venues) %>%
    ggplot(aes(x=team, fill=venue_state)) +
	geom_bar() +
    # geom_bar(data=subset.data.frame(season_2016,(season_2016$winner %in% levels(teams$team)))) +
	## facet_wrap() +
	coord_flip() +
	labs(title="Where has each team played matches this year?", x="", y="",fill="State") +
	scale_x_discrete(breaks= rev(levels(teams$team)), drop = FALSE) +
	scale_y_continuous()

## create chart showing where teams have won
wins_by_venue <- ggplot(season_2016, aes(x=winner, fill=venue)) +
									 geom_bar(data=subset.data.frame(season_2016,(season_2016$winner %in% levels(teams$team)))) +
									 ## facet_wrap() +
	               	 coord_flip() +
	                 labs(title="Where has each team won matches this year?", x="", y="",fill="Venue") +
									 scale_x_discrete(drop = FALSE) +
									 scale_y_continuous()

wins_by_state <- ggplot(season_2016, aes(x=winner, fill=venue_state)) +
                	geom_bar(data=subset.data.frame(season_2016,(season_2016$winner %in% levels(teams$team)))) +
	                ## facet_wrap() +
									coord_flip() +
									labs(title="Where has each team won this year?", x="", y="",fill="State") +
									scale_x_discrete(drop = FALSE) +
									scale_y_continuous()

## Obtain the top eight teams based on latest afltables ladder
afl_ladder_url <- read_html("http://afltables.com/afl/seas/ladders/laddersyby.html#2016")
afl_ladder_url %>% html_nodes("table")  %>%
									 .[[1]] %>% html_table(fill = TRUE, head = FALSE) %>%
	                 data.frame() %>% .[-c(1,2),] %>%  .[1:8,1] ->
	                 top_eight
## create chart showing wins against top eight teams away from home.
wins_away_topeight <- ggplot(season_2016, aes(x=winner, fill=team)) +
	                    geom_bar(data=subset.data.frame(season_2016,((season_2016$team %in% top_eight & season_2016$playing=="home" & !(season_2016$team==season_2016$winner))))) +
	                    ## facet_wrap() +
	                    coord_flip() +
	                    labs(title="Wins against top eight teams away from home this year?", x="", y="",fill="Opposition") +
	                    scale_x_discrete(drop = FALSE) +
	                    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))

## create chart showing wins against top eight teams at home.
wins_home_topeight <- ggplot(season_2016, aes(x=winner, fill=team)) +
	geom_bar(data=subset.data.frame(season_2016,((season_2016$team %in% top_eight & season_2016$playing=="away" & !(season_2016$team==season_2016$winner))))) +
	## facet_wrap() +
	coord_flip() +
	labs(title="Wins against top eight teams at home this year?", x="", y="",fill="Opposition") +
	scale_x_discrete(drop = FALSE) +
	scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))

losses_topeight <- ggplot(season_2016, aes(x=team, fill=winner)) +
	geom_bar(data=subset.data.frame(season_2016,((season_2016$winner %in% top_eight & !(season_2016$team==season_2016$winner))))) +
	## facet_wrap() +
	coord_flip() +
	labs(title="Who has lost against top eight teams this year?", x="", y="",fill="Opposition") +
	scale_x_discrete(drop = FALSE) +
	scale_y_continuous(breaks = c(seq(0,8, by=1)), limits=c(0,8))

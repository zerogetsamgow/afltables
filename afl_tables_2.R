library(xml2)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)

# Get team, venue and state data ----
library(googlesheets)
# Run gs_auth() to set this up

gs <- gs_key("17041tChNHzRNYmi1nCCJvacOqbk19MJUzW8UVX91b_A")

teams <-  gs_read(gs, sheet = "AFL_data", ws = "Teams",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df() %>%
    mutate(is_vic = team_state=="Vic") %>%
    arrange(desc(is_vic), team)

teams$team %>% factor() -> teams$team

venues <- gs_read(gs, sheet = "AFL_data", ws = "Venues",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

states <- gs_read(gs, sheet = "AFL_data", ws = "States",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

seasons <- c(1990:2017)

matches_df <- data.frame()
ladders_df <- data.frame()
match_id <- 0L

match_or_ladder <- function(node_table) {
      node_table %>% nrow() %>% as.character() %>% switch(., "1"="Bye",
                                                             "2"="Match",
      																										   "9"="Ladder", 
      																										   "14"="Ladder",
      																										   "15"="Ladder",
                                                             "16"="Ladder",
      																										   "17"="Ladder",
      																										   "18"="Ladder",
      																										   "19"="Ladder") %>% return()
}

for (season in seasons) {
    ## create variable for URL of season 2016 on afltables
    afltables <- read_html(paste0("http://afltables.com/afl/seas/",as.character(season),".html"))

    ## Count number of nodes of type td_table
    node_type <- "center table"
    no_of_tables <-  afltables %>% html_nodes(node_type)  %>% length()
    match_id <- 0L

    for (table_count in c(seq(1,no_of_tables,by=1))) {
        node_count <- table_count
        node_table <- afltables %>%
            html_nodes(node_type)  %>%
            .[[table_count]] %>%
            html_table(fill = TRUE, head = FALSE) %>%
            data.frame()

        if(node_table %>% ncol()==4) {
                switch(node_table %>% match_or_ladder(),
                        "Match"={## Spread the data to create single row for each match
                                match_id <- match_id+1L
                                this_match <- cbind(season, match_id, node_table[1,], node_table[2,])
                                ## Add match to matches_df
                                matches_df<-rbind(matches_df,this_match)},
                        "Ladder"={ladders_df<-rbind(ladders_df, node_table[2:nrow(node_table),] %>% mutate(season=season))})
        }
    }
}
## Rename variables
names(matches_df) <- c("season", "match_id",
                       "team_home", "quarters_home", "score_home",
                       "date_venue",
                       "team_away", "quarters_away", "score_away",
                       "result")

names(ladders_df) <- c("team_letters", "round_id","team_points", "team_per_cent", "season")

## Extract data from date_venue and result variable into separate variables
matches_df <- matches_df %>% mutate(team_home = gsub("(Footscray,Kangaroos)","(Western Bulldogs,North Melbourne)",team_home),
                                    team_away = gsub("(Footscray,Kangaroos)","(Western Bulldogs,North Melbourne)",team_away),
                                    result = gsub("(Footscray,Kangaroos)","(Western Bulldogs,North Melbourne)",result),
                                    local_time = as.POSIXct(
                                                 strptime(sub("([0-9]){1}:","0\\1:", ## Add leading zero to hour
                                                          sub("[A-Za-z]{3}\\s","",   ## Remove Day string
                                                          gsub("\\s+(\\(|Att).*$", "", date_venue))) ## Remove text after date
                                                                , "%d-%b-%Y %I:%M %p")), ## Format of character string to be converted to date.
                                    venue = sub(".*Venue: ", "", date_venue),
                                    attendance = as.integer(sub(",", "",gsub("^.*Att: ([0-9,]+).*$", "\\1", date_venue))),
                                    winner = ifelse(!grepl("Match drawn",result),sub(" won.*", "", result),""),
                                    loser = ifelse(winner==team_home, team_away, ifelse(winner=="","",team_home)),
                                    win_margin = ifelse(grepl("Match drawn",result),0,as.integer(gsub(".*by\\s([0-9]+)\\spt.*","\\1",result)))) %>%
                                    separate(quarters_home, into = paste0("q", 1:4, "_home"), sep = " ") %>%
                                    separate(quarters_away, into = paste0("q", 1:4, "_away"), sep = " ")## %>%
                                    ## select(-result, -date_venue)

## Create finals variable NEEDS fixing
finals_df <- matches_df %>% dplyr::group_by(season) %>% filter((season %in% c(1990:1993) & match_id %in% c((n()-6):n()))|
																															 (season %in% c(1994:2009) & match_id %in% c((n()-8):n()))|
																															 (season %in% c(2010:2010) & match_id %in% c((n()-9):n()))| ## drawn grand final
																															 (season %in% c(2011:2016) & match_id %in% c((n()-8):n()))) %>%
																												 mutate(finals=TRUE)

regular_df <- matches_df %>% dplyr::group_by(season) %>% filter((season %in% c(1990:1993) & match_id %in% c(1:(n()-7)))|
																																	(season %in% c(1994:2009) & match_id %in% c(1:(n()-9)))|
																																	(season %in% c(2010:2010) & match_id %in% c(1:(n()-10)))| ## drawn grand final
																																	(season %in% c(2011:2016) & match_id %in% c(1:(n()-9)))) %>%
																												 mutate(finals=FALSE)

this_season <- matches_df %>% filter(season==2017) %>%
	mutate(finals=FALSE)


matches_df <- rbind(finals_df, regular_df)  %>% arrange(season, match_id)
##remove(finals_df, regular_df)

close_finals_df <- finals_df %>% filter(win_margin<7)
close_finals_eagles_df <- finals_df %>% filter(win_margin<7 & (team_home=="West Coast"|team_away=="West Coast"))

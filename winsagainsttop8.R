library(rvest)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(tidyr)

## factorise team variables with level order chosen to group Victoria and interstate teams
season_2016$team %>% factor(levels=c("Carlton","Collingwood","Essendon","Geelong","Hawthorn",
																 "Melbourne","North Melbourne","Richmond","St Kilda","Western Bulldogs",
																 "Adelaide","Brisbane Lions","Fremantle","Gold Coast","Greater Western Sydney",
																 "Port Adelaide","Sydney","West Coast")) -> season_2016$team

season_2016$winner %>% factor(levels=c("Carlton","Collingwood","Essendon","Geelong","Hawthorn",
																		 "Melbourne","North Melbourne","Richmond","St Kilda","Western Bulldogs",
																		 "Adelaide","Brisbane Lions","Fremantle","Gold Coast","Greater Western Sydney",
																		 "Port Adelaide","Sydney","West Coast")) -> season_2016$winner

## create teams data frame with team_state variable
teams <- data.frame(team = levels(season_2016$team),team_state = c(rep("Victoria", 10),
																																			 "South Australia",
																																			 "Queensland",
																																			 "Western Australia",
																																			 "Queensland",
																																			 "New South Wales",
																																	     "South Australia",
																																	     "New South Wales",
																																     "Western Australia"))
## merge teams data frame with season data frame
season_2016 %>% merge(teams) -> season_2016

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

## create chart of all teams losses against top eight.
losses_topeight <- ggplot(season_2016, aes(x=team, fill=winner)) +
	geom_bar(data=subset.data.frame(season_2016,((season_2016$winner %in% top_eight & !(season_2016$team==season_2016$winner))))) +
	## facet_wrap() +
	coord_flip() +
	labs(title="Who has lost against top eight teams this year?", x="", y="",fill="Opposition") +
	scale_x_discrete(drop = FALSE) +
	scale_y_continuous(breaks = c(seq(0,8, by=1)), limits=c(0,8))

library(rvest)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(tidyr)

games_per_venue <- games_2016 %>%
    inner_join(venues) %>%
    ggplot(aes(x=venue)) +
    geom_bar()+
    coord_flip() +
    scale_x_discrete() +
    labs(title="Nearly 100 games will be played at the MCG and Docklands this year", x="", y="") +
    scale_y_continuous(breaks=c(seq(0,48,by=6)))

played_where <-
    games_2016 %>% select(team_home,team_away,venue) %>%
    gather(home_away, team, team_home:team_away) %>%
    inner_join(venues) %>%
    ggplot(aes(x=venue_state, fill=venue)) +
	geom_bar() +
    facet_wrap(~ team) +
	## coord_flip() +
	labs(title="Where has each team played matches this year?", x="", y="",fill="Venue") +
	scale_fill_manual(labels=venues$venue,values=venues$venue_colour) +
	scale_x_discrete(drop = FALSE) +
	scale_y_continuous()

## create chart of where teams have played by state
played_vic_or_not <-  games_2016 %>% select(team_home,team_away,venue) %>%
    gather(home_away, team, team_home:team_away) %>%
    inner_join(teams) %>%
    inner_join(venues) %>%
    ggplot(aes(x=venue_binary, fill=venue_state)) +
    theme_economist_white(gray_bg = FALSE) +
    theme(legend.position="right") +
    geom_bar() +
    facet_wrap(~ team_abb, nrow = 3) +
    ## coord_flip() +
    labs(title="Where has each team played matches this year?", x="", y="",fill="State") +
    scale_fill_manual(breaks=venues$venue_state, labels=venues$venue_state, values=c("royalblue","skyblue","darkorange2","maroon","red2","forestgreen","navyblue","gold")) +
    scale_x_discrete(breaks=c("Vic","Other"), labels=c("Vic","Other"),drop = FALSE) +
    scale_y_continuous(breaks=c(5,10,15,20))


## create chart showing where teams have won
wins_by_venue <- games_2016 %>%
                 filter(winner %in% teams$team) %>%
                 ggplot(aes(x=winner, fill=venue)) +
				 geom_bar() +
				 ## facet_wrap() +
	             coord_flip() +
	             labs(title="Where has each team won matches this year?", x="", y="",fill="Venue") +
				 scale_x_discrete(drop = FALSE) +
				 scale_y_continuous()

wins_by_state <- games_2016 %>%
                 filter(winner %in% teams$team) %>%
                 left_join(venues) %>%
                 left_join(states, by=c("venue_state"="state_abb")) %>%
                 ggplot(aes(x=winner, fill=state_comb)) +
                 geom_bar() +
	             ## facet_wrap() +
				 coord_flip() +
				 labs(title="Where has each team won this year?", x="", y="",fill="State") +
				 scale_x_discrete(drop = FALSE) +
				 scale_y_continuous()


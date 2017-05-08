library(rvest)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(tidyr)

## Obtain the top eight teams based on latest afltables ladder
afl_ladder_url <- read_html("http://afltables.com/afl/seas/ladders/laddersyby.html#2016")
afl_ladder_url %>% html_nodes("table")  %>%
									 .[[1]] %>% html_table(fill = TRUE, head = FALSE) %>%
	                 data.frame() %>% .[-c(1,2),] %>%  .[1:8,1] ->
	                 top_eight


## create chart showing wins against top eight teams away from home.
wins_away_topeight <- games_2016 %>%
                      inner_join(teams, by=c("team_away"="team")) %>%
                      filter(team_home %in% top_eight & team_away==winner) %>%
                      ggplot(aes(x=winner, fill=team_home)) +
	                  geom_bar() +
	                  ## facet_wrap() +
	                  coord_flip() +
	                  labs(title="Wins against top eight teams away from home this year?", x="", y="",fill="Opposition") +
	                  ##scale_x_discrete(drop = FALSE) +
	                  scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))

## create chart showing wins against top eight teams at home.
wins_home_topeight <- games_2016 %>%
    inner_join(teams, by=c("team_away"="team")) %>%
    filter(team_away %in% top_eight & team_home==winner) %>%
    ggplot(aes(x=winner, fill=team_away)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Wins against top eight teams at home this year?", x="", y="",fill="Opposition") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))+
    scale_fill_tableau()


## create chart of all teams losses against top eight.
wins_topeight <- games_2016 %>%
    inner_join(teams, by=c("team_away"="team")) %>%
    filter(loser %in% top_eight) %>%
    ggplot(aes(x=winner, fill=loser)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Wins against top eight teams this year?", x="", y="",fill="Opposition") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))+
    scale_fill_tableau()

losses_topeight <- games_2016 %>%
    inner_join(teams, by=c("team_away"="team")) %>%
    filter(winner %in% top_eight) %>%
    ggplot(aes(x=loser, fill=winner)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Losses against top eight teams this year?", x="", y="",fill="Opposition") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = c(seq(0,10, by=1)), limits=c(0,10))+
    scale_fill_tableau()

against_topeight <- games_2016 %>%
    inner_join(teams, by=c("team_away"="team")) %>%
    filter(winner %in% top_eight) %>%
    ggplot(aes(x=loser, fill=winner)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Losses against top eight teams this year?", x="", y="",fill="Opposition") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = c(seq(0,10, by=1)), limits=c(0,10))+
    scale_fill_tableau()

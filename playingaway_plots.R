library(dplyr)
library(ggplot2)
library(ggthemes)
load("afltables.Rdata")

## merge teams data frame with season data frame
played_where <-
    scores_2016 %>%
    left_join(games_2016) %>%
    ggplot(aes(x=playing_at, fill=venue)) +
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
    inner_join(states, by = c("venue_state" = "state_abb")) %>%
    subset.data.frame(,!is.na(scores_2016$score)) %>%
    ggplot(aes(x=team, fill=state_comb)) +
	geom_bar() +
    ## scale_fill_manual(values=teams$team_fill) +
    # geom_bar(data=subset.data.frame(season_2016,(season_2016$winner %in% levels(teams$team)))) +
	## facet_wrap() +
	coord_flip() +
	labs(title="Where has each team played matches this year?", x="", y="",fill="State") +
	scale_x_discrete(breaks= teams$team, drop = FALSE) +
	scale_y_continuous()

## create chart showing where teams have won
wins_by_venue <-
    games_2016 %>%
    ggplot(aes(x=winner, fill=venue)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Where has each team won matches this year?", x="", y="",fill="Venue") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous()

wins_by_state <-
    games_2016 %>%
    subset(winner !='') %>%
    inner_join(venues) %>%
    ggplot(aes(x=winner, fill=venue_state)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Where has each team won this year?", x="", y="",fill="State") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous()

## create chart showing wins against top eight teams away from home.

wins_against_topeight <-
    games_2016 %>%
    right_join(top_eight, by=c("loser"="team")) %>%
    ggplot(aes(x=winner, fill=loser)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Wins against top eight teams away this year",
         x="", y="", fill="Opposition") +
    scale_x_discrete(breaks = teams$team, labels = teams$team, drop = FALSE) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5)) +
    scale_fill_manual(breaks = teams$team, labels = teams$team, values=teams$team_fill)

games_2016 %>%
    right_join(top_eight, by=c("loser"="team")) -> test


wins_away_topeight <-
    games_2016 %>%
    inner_join(teams, by=c("team_away"="team")) %>%
    inner_join(top_eight, by=c("loser"="team")) %>%
    filter(team_away==winner) %>%
    ggplot(aes(x=winner, fill=team_home)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Wins against top eight teams away from home this year?",
         x="", y="", fill="Opposition") +
    scale_x_discrete(breaks = teams$team, labels = teams$team, drop = FALSE) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5)) +
    scale_fill_manual(values=teams$team_fill, drop = FALSE)

## create chart showing wins against top eight teams at home.
wins_home_topeight <-
    games_2016 %>%
    semi_join(top_eight, by=c("loser"="team")) %>%
    filter(team_home==winner) %>%
    ggplot(aes(x=winner, fill=loser)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
    labs(title="Wins against top eight teams at home this year?",
         x="", y="",fill="Opposition") +
    scale_x_discrete(breaks = levels(factor(teams$team)), drop = FALSE) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5), limits=c(0,5))

losses_topeight <-
    games_2016 %>%
    semi_join(top_eight, by=c("winner"="team")) %>%
    ggplot(aes(x=loser, fill=winner)) +
    geom_bar() +
    ## facet_wrap() +
    coord_flip() +
	labs(title="Who has lost against top eight teams this year?",
	     x="", y="",fill="Opposition") +
	scale_x_discrete(drop = FALSE) +
	scale_y_continuous(breaks = c(seq(0,8, by=1)), limits=c(0,8))

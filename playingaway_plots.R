library(dplyr)
library(ggplot2)
library(ggthemes)
load("afltables.Rdata")

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

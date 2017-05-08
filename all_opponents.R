games_2016 %>% select(game_id, team_home, team_away) %>%
           gather(where, team, team_home:team_away) -> blurgh

blurgh %>% inner_join(blurgh, by=c('game_id'='game_id')) %>%
           filter(!(team.x==team.y))  %>%
           inner_join(teams, by=c("team.x"="team")) -> blurgh

blurgh_colours <- teams %>%
                  filter(team %in% top_eight) %>%
                  arrange(team) %>%
                  select(team_fill, team_colour)

against_topeight <- blurgh %>%
    filter(team.y %in% top_eight & team.x %in% top_eight) %>%
    ggplot(aes(x=team.x, fill=team.y, colour=team.y)) +
    geom_bar() +
    facet_grid(. ~ where.x) +
    coord_flip() +
    labs(title="Games against top eight teams this year?", x="", y="") +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = c(seq(0,10, by=1)), limits=c(0,10))+
    scale_fill_manual(values=blurgh_colours$team_colour) +
    scale_colour_manual(values=blurgh_colours$team_fill)

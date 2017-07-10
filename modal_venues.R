## source('~/afltables/afl_tables_2.R')

modal_venue <- matches_df %>%
               filter(!finals & season > 2009) %>%
                      select(team_home, team_away, venue, season, match_id) %>%
                      gather(playing, team, team_home, team_away) %>%
                       mutate(playing=sub("team_","",playing)) %>%
                       select(team, season, venue, playing) %>%
                       group_by(team, season, venue, playing) %>%
                       summarise(games_at_venue=n()) %>%
                       group_by(team, season, venue) %>%
                       mutate(total_at_venue=sum(games_at_venue)) %>%
                       group_by(team, season) %>%
                       arrange(team, season, desc(total_at_venue)) %>%
                       filter(venue==venue[1] &
                                     row_number() %in% c(1,2))

library(ggplot2)
modal_venue_plot <- modal_venue %>%
                    ggplot(aes(x=season,y=games_at_venue,fill=playing, label=venue))+
                    geom_bar(stat="identity")+
                    ## geom_text(x=2012,y=17,label=venue)+
                    facet_wrap(~team) +
                    scale_y_continuous(limit=c(0,18),expand=c(0,0),breaks=c(seq(0,16,by=4)))

ggsave("modal_venue_plot.png",plot= modal_venue_plot, path= "~/afltables/plots/")

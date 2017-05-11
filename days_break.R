library(tidyr)
library(lubridate)
season_2016 %>% select(game_id, team_home, team_away) %>%
    gather(where, team, team_home:team_away) %>%
    mutate(where=gsub("team_","",where)) -> temp_teams

season_2016 %>% select(game_id, score_home, score_away) %>%
    mutate(margin_home=score_home-score_away,
           margin_away=score_away-score_home) %>%
    select(game_id, margin_home, margin_away) %>%
    gather(whose, margin, margin_home:margin_away) %>%
    mutate(whose=gsub("margin_","",whose)) -> temp_scores

games_2016 %>% select(game_id, local_time, winner) %>%
                mutate(game_date = gsub("[A-Z][a-z][a-z]\\s*.PM", "*.", local_time),
                       game_date = gsub("\\s\\d+(:)+\\d\\d\\s+(PM).*$", "", game_date),
                       game_date = gsub("[A-Z][a-z][a-z]\\s","",game_date),
                       game_date = dmy(game_date)) %>%
                select(game_id,game_date, winner) -> temp_dates

days_break_df <- inner_join(temp_teams, temp_scores, by=c('where'='whose', 'game_id'='game_id')) %>%
                 inner_join(temp_dates) %>%
                 arrange(team, game_id) %>%
                 mutate(days_break = as.factor(game_date-lag(game_date)),
                        winner=as.logical(team==winner)) %>%
                 filter(days_break %in% c(6,7,8))

remove(temp_dates,temp_scores,temp_teams)

breaks_df <- days_break_df %>%
             select(game_id, where, days_break) %>%
             spread(where, days_break) %>%
             rename("home_break"=home, "away_break"=away)

margins_df <- days_break_df %>%
              select(game_id, where, margin) %>%
              spread(where, margin) %>%
              rename("home_margin"=home, "away_margin"=away)

library(ggplot2)
days_break_chart <- inner_join(breaks_df,margins_df) %>%
                    ggplot(aes(x=away_break, y=home_margin, colour=home_break)) +
                    geom_point()

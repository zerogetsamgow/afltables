

score_distribution_df <- matches_df %>%
                         filter(season>2011) %>%
                         mutate(score_winner=ifelse(winner==team_home,score_home,score_away),
                                score_loser=ifelse(loser==team_home,score_home,score_away)) %>%
                         select(season, score_winner, score_loser, win_margin, team_home, winner) %>%
                         gather(winner_loser, score, score_winner:score_loser) %>%
                         group_by(season, win_margin, winner_loser) %>%
                         mutate(max_score=max(score),
                                min_score=min(score))

score_distribution_plot <- score_distribution_df %>%
                           ggplot(aes(x=win_margin,ymin=min_score,ymax=max_score,colour=winner_loser)) +
                           geom_linerange() +
                           facet_wrap(~season)


max(matches_df$win_margin, na.rm = TRUE)
matches_df %>% filter(win_margin==186)

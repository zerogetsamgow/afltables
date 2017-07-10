team_1 <- "Geelong"
team_2 <- "Western Bulldogs"

teams_to_compare <- c(team_1,team_2)

head_to_head <- matches_df %>%
                    filter(team_home %in% teams_to_compare &
                               team_away %in% teams_to_compare)


head_to_head_plot <- head_to_head %>%
                     mutate(win_margin=ifelse(winner==team_1,win_margin,-1*win_margin))%>%
                     ggplot(aes(x=local_time,y=win_margin)) +
                     geom_line()

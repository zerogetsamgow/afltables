library(reldist)
library(ggplot2)
library(ggrepel)


gini_values_df <- ladders_df %>% 
	                mutate(round_id=as.integer(round_id),
	                			 season=as.factor(season)) %>%
	                group_by(season, round_id) %>%
	                summarise(round_gini=gini(as.integer(team_points)))

gini_labels <- gini_values_df %>% filter(round_id==2 & season %in% c(1997,1998,2017))

gini_colours <- c(rep("black",2),"orange","red",rep("black",18),"blue")

gini_plot <- gini_values_df %>% filter(!round_id==1) %>%
	           ggplot(aes(x=round_id,y=round_gini,colour=season, label=season))+
	           geom_line() + 
	           geom_label(data=gini_labels, aes(x=round_id-1,y=round_gini, label = (season)))+
             geom_label_repel(data=gini_labels, aes(x=round_id,y=round_gini, label = (season)))+
	           scale_y_continuous(limits=c(0,0.5),expand=c(0,0.1), breaks = seq(0,0.5, by=.1))+
             scale_x_continuous(limits=c(0,25),expand=c(0,1), breaks=c(seq(2,22,by=4)))+
	           scale_colour_manual(values=gini_colours, guide=FALSE) +
	           labs(title="2017 isn't the most even season, yet",
	           		  subtitle="", y="Gini coefficient", x="Round number")

print(gini_plot)
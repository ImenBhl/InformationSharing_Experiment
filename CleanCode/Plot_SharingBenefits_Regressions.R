#house cleaning
rm(list=ls())


#load packages
packages <- c('plyr',  'dplyr',  'ggplot2',  'RColorBrewer','pals', 'vioplot', 'aplpack', 'moments', 'nortest', 'plotly', 'ggsignif', 'ggpubr', 'cowplot')
#lapply(packages, install.packages, character.only=TRUE)
lapply(packages, require, character.only=TRUE)

#raincloud plots:
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#install.packages("remotes")
#remotes::install_github("tpepler/nonpar")

#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")
colorsPalette_sharing <- c("#56B4E9","#E69F00")
colorsPalette_visibility <- c("#af8dc3","#7fbf7b")

raincloud_theme = theme(
  text = element_text(size =12,  family="sans"),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  #axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 14),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
  strip.background=element_blank())


#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}



#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))



#Scores depending on sharing decision 2 rounds ago
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2  ), ~visibility_radius+treatments_order+part+games+round_num_in_game+share_lag2, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
levels(dt$treatments_order) <- c('Visibility second','Visibility first')
dt$treatments_order <- relevel(dt$treatments_order, 'Visibility first')

dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")
dt_subset$part_visibility <- ifelse (dt_subset$part =="Part 1", "Visibility, Part 1", "Visibility, Part 2")


learning_curve_round_payoff_by_shareDecision_subset <- ggplot(dt_subset, aes(x = round_num_in_game, y = m, color = share_lag2)) +
  geom_point(size=0.75)+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = c(0,5,10,15), limits = c(1, 15))+
  #scale_y_continuous(limits = c(0,30))+
  guides(color=guide_legend(title=expression(paste(Sharing[t-2]))) , fill=guide_legend(title=expression(paste(Sharing[t-2])))) +
  facet_grid( ~part_visibility+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  xlab("Round") +
  ylab("Score")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12, face="bold"), axis.text.x = element_text(size = 10, face="bold"), axis.title.y = element_text( size = 10, face="bold"),
        legend.position = c(.1, .65),
        legend.title=element_text(size=10, face="plain"), 
        legend.text=element_text(size=8, face="plain"))+ #, legend.position="bottom")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
#ggsave(filename = "Results/LearningCurve_RoundPayoff_byShareDecision_subset.pdf", plot = learning_curve_round_payoff_by_shareDecision, height =5, width = 6, units = "in")







#Shared and non shared rewards across the game
dt <- ddply(realGame_data, ~visibility_radius+treatments_order+part+games+round_num_in_game+share_factor, summarize, m = mean(normalized_round_payoff*num_splitters), error = se(normalized_round_payoff*num_splitters))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
levels(dt$treatments_order) <- c('Visibility second','Visibility first')
dt$treatments_order <- relevel(dt$treatments_order, 'Visibility first')


dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")
#dt_subset$treatments_order2 <- factor(dt_subset$treatments_order, levels=c('Visibility first','Visibility second'))



learning_curve_observed_round_payoff_by_shareDecision_subset <- ggplot(dt_subset, aes(x = round_num_in_game, y = m, color = share_factor)) +
  geom_point(size=0.75)+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = c(0,5,10,15))+
  #scale_y_continuous(limits = c(0,30))+
  guides(color=guide_legend(title="Sharing ") , fill=guide_legend(title="Sharing ")) +
  facet_grid( ~treatments_order+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  xlab("Round") +
  ylab("Payoff")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12, face="bold"), axis.text.x = element_text(size = 10, face="bold"), axis.title.y = element_text( size = 10, face="bold"),
        legend.position = c(.1, .65),
        legend.title=element_text(size=10, face="plain"), 
        legend.text=element_text(size=8, face="plain"),
        strip.text.x = element_blank())+ #, legend.position="bottom")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
#ggsave(filename = "Results/LearningCurve_ObservedRoundPayoff_byShareDecision_Subset.pdf", plot = learning_curve_observed_round_payoff_by_shareDecision_subset, height =5, width = 6, units = "in")





#Additional information across game depending on the sharing decision
realGame_data$mean_additional_info <- apply(realGame_data, 1, function(x) (sum(x$mean_additional_info_from_my_visibilibity_rad_did_not_share, x$mean_additional_info_shared_with_me, na.rm=TRUE)/sum(c(!is.na(x$mean_additional_info_from_my_visibilibity_rad_did_not_share),!is.na(x$mean_additional_info_shared_with_me)))))
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2), ~round_num_in_game+share_lag2+visibility_radius+treatments_order+part+games, summarize, 
            m = mean(mean_additional_info, na.rm=TRUE), error = se(mean_additional_info))
#m = mean(max_additional_info, na.rm=TRUE), error = se(max_additional_info))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
dt$Share <- dt$share_lag2
levels(dt$treatments_order) <- c('Visibility second','Visibility first')
dt$treatments_order <- relevel(dt$treatments_order, 'Visibility first')

dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")
#dt_subset$treatments_order2 <- factor(dt_subset$treatments_order, levels=c('Visibility first','No visibility first'))

plot_additional_info_aross_time_subset <- ggplot(dt_subset, aes( x = round_num_in_game, y = m, color = Share, group = Share)) +
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = seq(0,15,5), limits = c(1, 15))+
  facet_grid(~treatments_order+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  guides(color=guide_legend(title=expression(paste(Sharing[t-2]))) , fill=guide_legend(title=expression(paste(Sharing[t-2]))) ) +
  xlab("Round") +
  ylab("Social information") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12, face="bold"), axis.text.x = element_text(size = 10, face="bold"), axis.title.y = element_text( size = 10, face="bold"),
        legend.position = c(.1, .65),
        legend.title=element_text(size=10, face="plain"), 
        legend.text=element_text(size=8, face="plain"),
        strip.text.x = element_blank())+ #, legend.position="bottom", legend.box = "vertical")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
#ggsave(filename = "Results/Additional_info_acrossTime_by_treatment_and_setOfGames_Subset.pdf", plot = plot_additional_info_aross_time_subset, height =5, width = 8, units = "in")




plot <- ggdraw() +
  draw_plot(learning_curve_round_payoff_by_shareDecision_subset, x = 0, y = 0.6, width = 1, height = .4) +
  draw_plot(learning_curve_observed_round_payoff_by_shareDecision_subset, x = 0, y = .3, width = 0.99, height = .3) +
  draw_plot(plot_additional_info_aross_time_subset, x = 0, y = 0.0, width = 1, height = 0.3) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0, 0), y = c(0.95, .65, .35))
ggsave(filename = "Results/SharingBenefits_subsets_combined2.pdf", plot = plot, height =8, width = 5, units = "in")




## Run regression models from DataRegression_M2-3-4 before
Effects_size <- plot_models(MixedModel1, MixedModel2, MixedModel3, 
                            show.p = TRUE,
                            show.values = TRUE,
                            #p.shape = TRUE,
                            vline.color = "grey75", 
                            line.size = 0.5,
                            dot.size = 1,
                            #geom.colors = "Dark2", 
                            wrap.labels = 48,
                            grid = TRUE,
                            axis.lim = c(-2,4),
                            axis.labels=c( expression(paste(Log(Payoff[t-2]),"*",Sharing[t-2])),  expression(paste(Sharing[t-2])),expression(paste(Log(Payoff[t-2]))),"Part 2", "Visibility",  "Round" ),
                            m.labels = c( "Sharing\n benefits", "Social information\n benefits", "Log(Score)"))+
  facet_grid(~group, scales = "free_x") + 
  scale_y_continuous(limits = c(NA, NA), expand = c(0.5, 0)) +              
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=14, face="bold"), axis.text.y = element_text(size = 10, face="plain"), axis.title.x = element_text( size = 10, face="bold"), legend.position="none")




plot2 <- ggdraw() +
  draw_plot(plot, x = 0, y = 0, width = 0.35, height =1) +
  draw_plot(Effects_size, x = 0.35, y = 0, width = 0.65, height = 1) +
  draw_plot_label(label = c("d"), size = 12,
                  x = c(0.45), y = c(0.95))
ggsave(filename = "Results/SharingBenefits_subsets_combined2_Effect_size.pdf", plot = plot2, height =5.25, width = 13, units = "in")


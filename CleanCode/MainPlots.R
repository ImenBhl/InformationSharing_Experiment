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
#utils::View(realGame_data)


### To combine two plots next to each other :
require(gridExtra)
grid.arrange(plot1, plot2, ncol=2)
ggsave("Results/Payoff_and_Visibility.pdf", arrangeGrob(learning_curve_round_payoff, plot_payoff, ncol=2), height =4.5, width = 8, units = "in")

####################
#Treatments level
####################

# The average round payoff per round (learning curve) per visibiliy condition (the 4 games are pooled)
dt <- ddply(realGame_data, ~visibility_radius+round_num_in_game, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
dt$visibility_radius <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

learning_curve_round_payoff <- ggplot(dt, aes(x = round_num_in_game, y = m, color = visibility_radius)) +
  geom_line( linetype = "dashed")+
  geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = visibility_radius), alpha=0.4)+
  scale_x_continuous(breaks = c(0,5,10,15))+
  scale_y_continuous(limits = c(0,31))+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  #guides(color=FALSE , fill=FALSE) +
  xlab("Round") +
  ylab("Score")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/LearningCurve_RoundPayoff.pdf", plot = learning_curve_round_payoff, height =4.5, width = 6, units = "in")


#Frequency of use of the different search strategies across the game

histogram_search_strategy_family_across_game <- ggplot(realGame_data, aes(x=round_num_in_game ,  color=search_strategy_family, fill=search_strategy_family )) + 
  geom_bar(position="fill", width=0.5)+
  scale_x_continuous(breaks = c(0,5,10,15))+
  xlab("Round") +
  ylab("Frequency") +
  labs(title = "",  color = "Search Strategy\n", fill = "Search Strategy\n") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  facet_grid(  ~Visibility , labeller = label_both)+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Search_strategies_Evolution.pdf", plot = histogram_search_strategy_family_across_game, height =4, width = 6, units = "in")


realGame_data$search_strategy_family2 <- ifelse((realGame_data$search_strategy_family=="Local search around best info"),"Local search around best information", "Other")
realGame_data$search_strategy_family2 <- as.factor(realGame_data$search_strategy_family2)
realGame_data$search_strategy_family2 <- relevel(realGame_data$search_strategy_family2,"Other")

histogram_search_strategy_family_across_game_2 <- ggplot(realGame_data, aes(x=round_num_in_game ,  color=search_strategy_family2, fill=search_strategy_family2 )) + 
  geom_bar(position="fill", width=0.5)+
  scale_x_continuous(breaks = c(0,5,10,15))+
  xlab("Round") +
  ylab("Frequency") +
  labs(title = "",  color = "", fill = "") + #color = "Search Strategy\n", fill = "Search Strategy\n"
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE)) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=16,  family="sans"), legend.text=element_text(size=16), legend.position="top",legend.direction='vertical', legend.box = "vertical")+
  #facet_grid(  ~Visibility , labeller = label_both)+
  scale_fill_manual(values=c("#999999", "#D6604D"))+
  scale_color_manual(values=c("#999999", "#D6604D"))
ggsave(filename = "Results/Search_strategies_Evolution_V2.pdf", plot = histogram_search_strategy_family_across_game_2, height =4, width = 5.5, units = "in")

dt <- ddply(subset(realGame_data,realGame_data$search_strategy_family2=="Local search around best information"), ~round_num_in_game+search_strategy_family2, summarize, m = mean(innovation_distance_after_copying), error = se(innovation_distance_after_copying))
my.labels  <- c("Local search\naround best information"  )
dt$search_strategy_family_labels <- factor(dt$search_strategy_family2, levels=c("Local search around best information"),labels = my.labels)


learning_curve_innovation_distance <- ggplot(subset(dt, round_num_in_game>1), aes(x = round_num_in_game, y = m)) +
  #geom_quasirandom(aes(color=search_strategy_family2, fill=search_strategy_family2), position=position_dodge(1), alpha=0.4, size=1, width=0.3) +
  geom_point(aes(color=search_strategy_family2), size=2)+
  #geom_line(aes(color=search_strategy_family2), size=0.75)+
  geom_errorbar(aes(color=search_strategy_family2, ymin=m - 2*error, ymax=m + 2*error), size=1, width=.1, alpha=0.45) +
  geom_smooth( aes(color=search_strategy_family2, fill=search_strategy_family2),method="loess")+
  scale_x_continuous(breaks = c(0,5,10,15))+
  scale_y_continuous(breaks = seq(0,2,0.1))+
  #facet_grid(.~search_strategy_family_labels, labeller=label_value)+
  labs(fill = NULL) +
  labs(title="")+
  #guides(color=guide_legend(title="Search strategy"), fill=guide_legend(title="Search strategy")) +
  guides(fill=FALSE) + guides(color=FALSE) +
  xlab("Round") +
  ylab("Innovation distance") +
  scale_fill_manual(values=c("#D6604D"))+
  scale_color_manual(values=c("#D6604D"))+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=16,  family="sans"))
ggsave(filename = "Results/InnovationEvolution_V2.pdf", plot = learning_curve_innovation_distance, height =5, width = 8, units = "in")


####################
#Participants level
####################

#Boxplot of the average round payoff by participant by visibility condition and treamtnets order
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+part, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
levels(d$treatments_order) <- c('Visibility second','Visibility first')
d$treatments_order <- relevel(d$treatments_order,'Visibility first')

plot_payoff <- ggplot(d, aes(x = Visibility, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="Visibility", fill="Visibility"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(fill=Visibility, group=Visibility), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Visibility") +
  ylab("Payoff")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  facet_grid(.~treatments_order, labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/Payoff_comparison_treatmentsOrder.pdf", plot = plot_payoff, height =4, width = 6, units = "in")

plot_payoff_no_stat_comparison <- ggplot(d, aes(x = treatments_order, y=m, fill=Visibility)) +
  geom_flat_violin(aes(fill=Visibility), position = position_nudge(x = 0, y = 0), alpha = .6, trim=TRUE) +
  xlab("Treatments order") +
  ylab("Score")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  guides(color=FALSE , fill=FALSE) +
  scale_y_continuous( limits=c(0, 30))+
  #facet_grid(.~treatments_order, labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/Payoff_treatmentsOrder_violinPlots.pdf", plot = plot_payoff_no_stat_comparison, height =4, width = 6, units = "in")


#Boxplot of participants payoff by sharing decision, by visibility condition and treatments order
d <- ddply(subset(realGame_data, round_num_in_game>2 ), ~participant.code+visibility_radius+treatments_order+share_lag2, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_payoff_sharers_vs_non_sharers <- ggplot(d, aes(x = share_lag2, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="share_lag2", fill="share_lag2"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(fill=share_lag2, group=share_lag2), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Sharing decision two rounds ago") +
  ylab("Payoff")+
  guides(color=guide_legend(title="Sharing decision two rounds ago"), fill=guide_legend(title="Sharing decision two rounds ago")) +
  facet_grid(treatments_order~Visibility, labeller=labeller(Visibility=label_both, treatments_order=label_value))+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/Payoff_comparison_by_sharing_decision_by_treatment_and_treatmentsorder.pdf", plot = plot_payoff_sharers_vs_non_sharers, height =4, width = 6, units = "in")
#ggsave(filename = "Results/Payoff_comparison_by_sharing_decision.pdf", plot = plot_payoff_sharers_vs_non_sharers, height =4, width = 6, units = "in")



#Boxplot of participants sharing rate by visibility condition and treatments order
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order, summarize, m = mean(share), error = se(share))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_sharing_rate <- ggplot(d, aes(x = Visibility, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="Visibility", fill="Visibility"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(fill=Visibility, group=Visibility), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Visibility") +
  ylab("Average individual sharing rate")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  facet_grid(.~treatments_order, labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  #scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharingRate.pdf", plot = plot_sharing_rate, height =4, width = 6, units = "in")



#performance across the game depending on the share decision two rounds ago
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2  ), ~visibility_radius+treatments_order+part+games+round_num_in_game+share_lag2, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
levels(dt$treatments_order) <- c('Visibility second','Visibility first')
dt$treatments_order <- relevel(dt$treatments_order, 'Visibility first')

dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")

learning_curve_round_payoff_by_shareDecision <- ggplot(dt, aes(x = round_num_in_game, y = m, color = share_lag2)) +
  geom_point(size=0.75)+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = c(0,5,10,15), limits = c(1, 15))+
  #scale_y_continuous(limits = c(0,30))+
  guides(color=guide_legend(title=expression(paste(Sharing[t-2]))) , fill=guide_legend(title=expression(paste(Sharing[t-2])))) +
  facet_grid(treatments_order ~part+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  geom_rect(data = subset(dt, treatments_order=="Visibility second" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  xlab("Round") +
  ylab("Score")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+ #, legend.position="bottom")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/LearningCurve_RoundPayoff_byShareDecision.pdf", plot = learning_curve_round_payoff_by_shareDecision, height =5, width = 6, units = "in")


boxplot_round_payoff_by_shareDecision_subset <- ggplot(dt_subset, aes(x = share_lag2, y=m)) +
  geom_boxplot(notch=FALSE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="share_lag2", fill="share_lag2"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(aes(group=share_lag2), fun=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  #geom_flat_violin(aes(fill=share_lag2, group=share_lag2), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Sharing decision") +
  ylab("Payoff")+
  guides(color=guide_legend(title="Sharing decision"), fill=guide_legend(title="Sharing decision")) +
  facet_grid( ~treatments_order+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility second" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)


#Shared and non shared rewards across the game
dt <- ddply(realGame_data, ~visibility_radius+treatments_order+part+games+round_num_in_game+share_factor, summarize, m = mean(normalized_round_payoff*num_splitters), error = se(normalized_round_payoff*num_splitters))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
levels(dt$treatments_order) <- c('Visibility second','Visibility first')
dt$treatments_order <- relevel(dt$treatments_order, 'Visibility first')


dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")
dt_subset$treatments_order2 <- factor(dt_subset$treatments_order, levels=c('Visibility first','Visibility second'))

learning_curve_observed_round_payoff_by_shareDecision <- ggplot(dt, aes(x = round_num_in_game, y = m, color = share_factor)) +
  geom_point(size=0.75)+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = c(0,5,10,15))+
  #scale_y_continuous(limits = c(0,30))+
  guides(color=guide_legend(title="Sharing") , fill=guide_legend(title="Sharing")) +
  facet_grid(treatments_order ~part+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  xlab("Round") +
  ylab("Payoff")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+ #, legend.position="bottom")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/LearningCurve_ObservedRoundPayoff_byShareDecision.pdf", plot = learning_curve_observed_round_payoff_by_shareDecision, height =5, width = 6, units = "in")


learning_curve_observed_round_payoff_by_shareDecision_subset <- ggplot(dt_subset, aes(x = round_num_in_game, y = m, color = share_factor)) +
  geom_point(size=0.75)+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = c(0,5,10,15))+
  #scale_y_continuous(limits = c(0,30))+
  guides(color=guide_legend(title="Sharing ") , fill=guide_legend(title="Sharing ")) +
  facet_grid( ~treatments_order+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility second" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  xlab("Round") +
  ylab("Payoff")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+ #, legend.position="bottom")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/LearningCurve_ObservedRoundPayoff_byShareDecision_Subset.pdf", plot = learning_curve_observed_round_payoff_by_shareDecision_subset, height =5, width = 6, units = "in")

boxplot_observed_round_payoff_by_shareDecision_subset <- ggplot(dt_subset, aes(x = share_factor, y=m)) +
  geom_boxplot(notch=FALSE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="share_factor", fill="share_factor"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(aes(group=share_factor), fun=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  #geom_flat_violin(aes(fill=share_factor, group=share_factor), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Sharing decision") +
  ylab("Observed payoff")+
  guides(color=guide_legend(title="Sharing decision"), fill=guide_legend(title="Sharing decision")) +
  facet_grid( ~treatments_order2+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  geom_rect(data = subset(dt_subset, treatments_order=="No visibility first" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)



#Shared (observed) payoff between the highlighted cases
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+games+share_factor, summarize, m = mean(normalized_round_payoff_before_splitting), error = se(normalized_round_payoff_before_splitting))
subset1 <- subset(d, share_factor =="Yes" & games == "Last 2 games" & visibility_radius==0 & treatments_order=="No visibility first")
subset2 <- subset(d, share_factor =="Yes" & games == "Last 2 games" & visibility_radius==2 & treatments_order=="Visibility first")
t.test(subset1$m,subset2$m, paired= FALSE) 



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
dt_subset$treatments_order2 <- factor(dt_subset$treatments_order, levels=c('Visibility first','No visibility first'))


plot_additional_info_aross_time <- ggplot(dt, aes( x = round_num_in_game, color = Share, group = Share)) +
  geom_line(mapping=aes( y = m), size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = seq(0,15,5), limits = c(1, 15))+
  facet_grid(treatments_order~part+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  guides(color=guide_legend(title=expression(paste(Sharing[t-2]))) , fill=guide_legend(title=expression(paste(Sharing[t-2]))) ) +
  xlab("Round") +
  ylab("Average social information") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12.5,  family="sans"))+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/Additional_info_acrossTime_by_treatment_and_setOfGames.pdf", plot = plot_additional_info_aross_time, height =5, width = 8, units = "in")


plot_additional_info_aross_time_subset <- ggplot(dt_subset, aes( x = round_num_in_game, y = m, color = Share, group = Share)) +
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = seq(0,15,5), limits = c(1, 15))+
  facet_grid(~treatments_order+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility second" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  guides(color=guide_legend(title=expression(paste(Sharing[t-2]))) , fill=guide_legend(title=expression(paste(Sharing[t-2]))) ) +
  xlab("Round") +
  ylab("Average social information") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12.5,  family="sans"))+ #, legend.position="bottom", legend.box = "vertical")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/Additional_info_acrossTime_by_treatment_and_setOfGames_Subset.pdf", plot = plot_additional_info_aross_time_subset, height =5, width = 8, units = "in")


boxplot_additional_info_subset <- ggplot(dt_subset, aes(x = share_lag2, y=m)) +
  geom_boxplot(notch=FALSE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="share_lag2", fill="share_lag2"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(aes(group=share_lag2), fun=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  #geom_flat_violin(aes(fill=share_lag2, group=share_lag2), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Sharing decision") +
  ylab("Average social information")+
  guides(color=guide_legend(title="Sharing decision"), fill=guide_legend(title="Sharing decision")) +
  facet_grid( ~treatments_order2+games, labeller= labeller( Visibility=label_both, games=label_value, part=label_value))+
  geom_rect(data = subset(dt_subset, treatments_order=="No visibility first" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)


#Additional information across game depending on the sharing decision, and per information source
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2), ~round_num_in_game+share_lag2+visibility_radius+treatments_order+part+games, summarize, 
            mean_additional_info_shared = mean(mean_additional_info_shared_with_me, na.rm=TRUE), error_additional_info_shared = se(mean_additional_info_shared_with_me), max_additional_info_shared = mean(max_additional_info_shared_with_me, na.rm=TRUE),
            mean_additional_info_visible_not_shared = mean(mean_additional_info_from_my_visibilibity_rad_did_not_share, na.rm=TRUE), error_additional_info_visible_not_shared = se(mean_additional_info_from_my_visibilibity_rad_did_not_share), max_additional_info_visible_not_shared = mean(max_additional_info_from_my_visibilibity_rad_did_not_share, na.rm=TRUE)
)
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
dt$Share <- dt$share_lag2

dt_subset <- subset(dt, dt$Visibility=="Yes" & dt$games == "Last 2 games")
dt_subset$treatments_order2 <- factor(dt_subset$treatments_order, levels=c('Visibility first','No visibility first'))

plot_additional_info_aross_time <- ggplot(dt, aes( x = round_num_in_game, color = Share, group = Share)) +
  geom_line(mapping=aes( y = mean_additional_info_shared, linetype="Shared"), size=0.75)+
  geom_line(mapping=aes(y = mean_additional_info_visible_not_shared, linetype="Non shared"), size=0.75)+
  #geom_line(mapping=aes(y = mean_additional_info_visible_and_shared, linetype="dotted"), size=0.75)+
  scale_x_continuous(breaks = seq(0,15,5), limits = c(1, 15))+
  facet_grid(treatments_order~part+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  guides(color=guide_legend(title=" Sharing decision\n (two rounds backward)") , fill=guide_legend(title=" Sharing decision\n (two rounds ago)") ) +
  xlab("Round") +
  ylab("Average social information") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12.5,  family="sans"))+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)+
  scale_linetype_manual(name = "Social information source", values = c(2, 1))


plot_additional_info_aross_time_subset <- ggplot(dt_subset, aes( x = round_num_in_game, color = Share, group = Share)) +
  geom_line(mapping=aes( y = mean_additional_info_shared, linetype="Shared"), size=0.75)+
  geom_line(mapping=aes(y = mean_additional_info_visible_not_shared, linetype="Non shared"), size=0.75)+
  scale_x_continuous(breaks = seq(0,15,5), limits = c(1, 15))+
  facet_grid(~treatments_order2+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  geom_rect(data = subset(dt_subset, treatments_order=="No visibility first" &  games=="Last 2 games" & part=="Part 2"), 
            fill = NA, colour = "black",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = subset(dt_subset, treatments_order=="Visibility first" &  games=="Last 2 games" & part=="Part 1"), 
            fill = NA, colour = "grey80",linetype = 2, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  guides(color=guide_legend(title=" Sharing decision\n (two rounds backward)") , fill=guide_legend(title=" Sharing decision\n (two rounds ago)") ) +
  xlab("Round") +
  ylab("Average social information") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12.5,  family="sans"))+ #, legend.position="bottom", legend.box = "vertical")+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)+
  scale_linetype_manual(name = "Social information source", values = c(2, 1))


####################
#Groups level
####################

#Boxplot of the number of (other) sharers in the group
realGame_data$num_sharers_in_group <- as.numeric(realGame_data$num_sharers_in_group)
d <- ddply(realGame_data, ~group.id+visibility_radius+treatments_order, summarize, m = mean(num_sharers_in_group), error = se(num_sharers_in_group))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_num_sharers <- ggplot(d, aes(x = Visibility, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="Visibility", fill="Visibility"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(fill=Visibility, group=Visibility), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Visibility") +
  ylab("Number of other sharers in the group")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  facet_grid(.~treatments_order, labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_y_continuous(limits = c(0, 5))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
#ggsave(filename = "Results/Num_sharers_in_group.pdf", plot = plot_num_sharers, height =4, width = 6, units = "in")
ggsave(filename = "Results/Num_sharers_in_group_by_treatmentsorder.pdf", plot = plot_num_sharers, height =4, width = 6, units = "in")


#Evolution of the number of sharers in the group
dt <- ddply(subset(realGame_data, round_num_in_game>1), ~visibility_radius+treatments_order+round_num_in_game, summarize, m = mean(total_num_sharers_in_group), error = se(total_num_sharers_in_group))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_numSharers_aross_time <- ggplot(dt, aes(x = round_num_in_game, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  scale_x_continuous(breaks = seq(0,15,5))+
  facet_grid( .~treatments_order, labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Round") +
  ylab("Total number of shares in the group") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/NUmSharersEvolution.pdf", plot = plot_numSharers_aross_time, height =4, width = 6, units = "in")


#Sharing decision by distance to the best observation
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+share_factor, summarize, m = mean(distance_to_max), error = se(distance_to_max))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_shared_payoff <- ggplot(d, aes(x = share_factor, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="share_factor", fill="share_factor"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(color=share_factor, fill=share_factor), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=4.5, vjust=2)+
  xlab("Sharing decision") +
  ylab("Distance to the maximum")+
  guides(color=guide_legend(title=" Sharing\n decision"), fill=guide_legend(title=" Sharing\n decision")) +
  facet_grid(treatments_order~Visibility, labeller=labeller(Visibility=label_both, treatments_order=label_value))+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=16,  family="sans"))+
  scale_fill_manual(values=colorsPalette_sharing)+
  scale_color_manual(values=colorsPalette_sharing)
ggsave(filename = "Results/plot_shared_payoff.pdf", plot = plot_shared_payoff, height =7, width = 9, units = "in")



#Boxplot of the number of visible neighbours by treatment and treatments order
realGame_data$num_all_visibile_neighbours <- as.numeric(realGame_data$num_all_visibile_neighbours)
d <- ddply(realGame_data, ~group.id+visibility_radius+treatments_order, summarize, m = mean(num_all_visibile_neighbours), error = se(num_all_visibile_neighbours))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))


plot_num_visible_neighbours <- ggplot(d, aes(x = Visibility, y=m)) +
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="Visibility", fill="Visibility"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  geom_flat_violin(aes(fill=Visibility, group=Visibility), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Yes", "No")), map_signif_level=FALSE, textsize=3)+
  xlab("Visibility") +
  ylab("Total number of visible others")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  facet_grid(.~treatments_order, labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_y_continuous(limits = c(0, 5))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/Num_visible_neighbours_by_treatment_and_order.pdf", plot = plot_num_visible_neighbours, height =4, width = 6, units = "in")


#Shared (observed) payoff by treatment
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+game_in_treatment+share_factor, summarize, m = mean(normalized_round_payoff_before_splitting), error = se(normalized_round_payoff_before_splitting))
d$games <- ifelse (d$game_in_treatment>2, "Last 2 games", "First 2 games")
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
d <- subset(d, share_factor=="Yes")
d$treatments_order <- relevel(d$treatments_order, ref = "No visibility first")

plot_shared_payoff <- ggplot(d, aes(x = Visibility, y=m)) +
  geom_quasirandom(alpha=0.35, aes(color=Visibility, fill=Visibility), position=position_dodge(1))+
  geom_boxplot(notch=TRUE, width = 0.1, outlier.shape=NA, outlier.colour = NULL, position=position_dodge(1), aes_string(colour="Visibility", fill="Visibility"))+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  stat_summary(fun.y=mean, geom="point",  shape=18, size=1.5, color="black",  position=position_dodge(1)) +
  #geom_flat_violin(aes(fill=Visibility, group=Visibility), position = position_nudge(x = .1, y = 0), alpha = .8, trim=TRUE) +
  geom_signif(test =  "t.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=FALSE),
              comparisons = list(c("No", "Yes")), map_signif_level=FALSE, textsize=3, vjust=1.75)+
  xlab("Visibility") +
  ylab("Shared observed payoff")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  facet_grid(treatments_order~., labeller=label_value)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharedObservedPayoff_by_treatment.pdf", plot = plot_shared_payoff, height =5, width = 7.5, units = "in")


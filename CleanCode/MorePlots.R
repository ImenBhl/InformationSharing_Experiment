#house cleaning
rm(list=ls())

#load packages
packages <- c('readxl', 'plyr',  'dplyr',  'ggplot2', 'RColorBrewer','pals', 'vioplot', 'aplpack', 'moments', 'nortest', 'hexbin', 'plotly', 'ggsignif', 'ggpubr', 'cowplot', 'lme4', 'stargazer', 'viridis', 'ggbeeswarm')
#lapply(packages, install.packages, character.only=TRUE)
lapply(packages, require, character.only=TRUE)

#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")

#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}



#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))






#########################################################################################
##Is the search strategy correlated with the performance? and with the innovation rate ?
########################################################################################

d <- ddply(subset(realGame_data,realGame_data$search_strategy_family!="Random exploration" & realGame_data$round_num_in_game>1), ~participant.code+search_strategy_family, summarize,  d = mean(innovation_distance_after_copying, na.rm=TRUE), m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
d$search_strategy_family <- factor(d$search_strategy_family)


my.labels  <- c("Local search\naround best info",
                "Local search\naround individual info",
                "Local search\naround social info"  )

round_payoff_by_searchStrategy <- ggplot(d, aes(x = search_strategy_family, y=m)) +
  geom_quasirandom(aes(color=search_strategy_family, fill=search_strategy_family), position=position_dodge(1), trim=TRUE) +
  geom_boxplot(color="black", position=position_dodge(1), width = 0.1, size = 1, outlier.shape=NA, fill=NA, notch=TRUE) +
  stat_summary(aes(color=search_strategy_family, fill=search_strategy_family), fun.y=mean, geom="point",  size=2.5, shape=18, position=position_dodge(1) ) +
  geom_signif(test =  "wilcox.test", #test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Local search around best info", "Local search around individual info"), 
                                 c("Local search around individual info", "Local search around social info"), 
                                 c("Local search around best info", "Local search around social info")), map_signif_level=TRUE, textsize=3, step_increase = 0.1)+
  
  xlab("Search strategy") +
  ylab("Round payoff")+
  guides(color=guide_legend(title="Search strategy"), fill=guide_legend(title="Search strategy")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=14,  family="sans"))+
  scale_x_discrete(labels= my.labels)+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Payoff_By_SearchStrategy.pdf", plot = round_payoff_by_searchStrategy, height =5, width = 8, units = "in")



innovation_distance_by_search_strategy <- ggplot(d, aes(x = search_strategy_family, y=d)) +
  geom_quasirandom(aes(color=search_strategy_family, fill=search_strategy_family), position=position_dodge(1), trim=TRUE) +
  geom_boxplot(aes(color=search_strategy_family, fill=search_strategy_family), position=position_dodge(1), width = 0.1, outlier.size = 0.5, fill="white", notch=TRUE) +
  stat_summary(aes(color=search_strategy_family, fill=search_strategy_family), fun.y=mean, geom="point",  size=2.5, shape=18, position=position_dodge(1) ) +
  geom_signif(test =  "wilcox.test", #test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Local search around best info", "Local search around individual info"), 
                                 c("Local search around individual info", "Local search around social info"), 
                                 c("Local search around best info", "Local search around social info")), map_signif_level=TRUE, textsize=3, step_increase = 0.1)+
  xlab("Search strategy") +
  ylab("Innovation distance")+
  guides(color=guide_legend(title="Search strategy"), fill=guide_legend(title="Search strategy")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_x_discrete(labels= my.labels)+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/InnovationDistances_ByStrategy.pdf", plot = innovation_distance_by_search_strategy, height =8.5, width = 11, units = "in")




################################################
##How did the innovation rate evolve over time?
###############################################

dt <- ddply(subset(realGame_data,realGame_data$search_strategy_family!="Random exploration"), ~participant.code+round_num_in_game+visibility_radius+treatments_order+search_strategy_family, summarize, m = mean(innovation_distance_after_copying), error = se(innovation_distance_after_copying))
dt$visibility_radius <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
my.labels  <- c("Local search\naround\nbest info",
                "Local search\naround\nindividual info",
                "Local search\naround\nsocial info"  )
dt$search_strategy_family_labels <- factor(dt$search_strategy_family, levels=c("Local search around best info", "Local search around individual info", "Local search around social info"),labels = my.labels)


learning_curve_innovation_distance <- ggplot(subset(dt, round_num_in_game>1), aes(x = round_num_in_game, y = m)) +
  geom_quasirandom(aes(color=search_strategy_family, fill=search_strategy_family), position=position_dodge(1), alpha=0.5, size=0.5) +
  geom_smooth( aes(color=search_strategy_family, fill=search_strategy_family),method="loess")+
  scale_x_continuous(breaks = c(0,5,10,15))+
  facet_grid(.~search_strategy_family_labels, labeller=label_value)+
  labs(fill = NULL) +
  labs(title="")+
  #guides(color=guide_legend(title="Search strategy"), fill=guide_legend(title="Search strategy")) +
  guides(fill=FALSE) + guides(color=FALSE) +
  xlab("Round") +
  ylab("Innovation distance after copying") +
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))
ggsave(filename = "Results/InnovationEvolution_noLegend.pdf", plot = learning_curve_innovation_distance, height =5, width = 8, units = "in")



########################################
##Did the exploration reduce over time?
#######################################

dt <- ddply(realGame_data, ~participant.code+round_num_in_game+visibility_radius, summarize, m = mean(distance_to_own_previous_guess), error = se(distance_to_own_previous_guess))
dt$visibility_radius <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

learning_curve_exploration <- ggplot(subset(dt, round_num_in_game>1), aes(x = round_num_in_game, y = m)) +
  geom_quasirandom(color="grey40")+
  geom_smooth( method="loess" ,color="black", fill="firebrick4", alpha=0.75, size=0.75)+
  scale_x_continuous(breaks = c(0,5,10,15))+
  labs(fill = NULL) +
  labs(title="")+
  xlab("Round") +
  ylab("Distance to own previous choice") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=13,  family="sans"))
ggsave(filename = "Results/ExplorationEvolution.pdf", plot = learning_curve_exploration, height =5, width = 7, units = "in")

learning_curve_exploration2 <- ggplot(subset(dt, round_num_in_game>1), aes(x = round_num_in_game, y = m)) +
  stat_binhex(aes(x=round_num_in_game, y=m,
                  fill = cut(..count.. ,c(0,2, 4,  8, 12, 20, 30,    Inf))),
              colour = NA, bins = 12, alpha = 0.75) +
  scale_fill_brewer(palette = "YlOrRd",
                    labels = c("1-2", "3-4",
                               "5-8","9-12", "13-20",
                               "21-30", "> 30")) +
  geom_smooth( method="loess" ,color="black", fill="firebrick4", alpha=0.75, size=0.75)+
  scale_x_continuous(breaks = c(0,5,10,15))+
  labs(fill = NULL) +
  labs(title="")+
  xlab("Round") +
  ylab("Distance to own previous choice") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=13,  family="sans"))
ggsave(filename = "Results/ExplorationEvolution2.pdf", plot = learning_curve_exploration2, height =5, width = 7, units = "in")





############################################################################
##Was the number of visible neighbours (both sharers and visible neighbours) 
## significantly different depending on the sharing decision ?
###########################################################################

realGame_data$num_all_visibile_neighbours <- as.numeric(realGame_data$num_all_visibile_neighbours)
d <- ddply(realGame_data, ~group.id+visibility_radius+treatments_order+share_lag2, summarize, m = mean(num_all_visibile_neighbours), error = se(num_all_visibile_neighbours))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_num_visible_neighbours_by_sharing_decision <- ggplot(subset(d, !is.na(share_lag2)), aes(x = share_lag2, y=m)) +
  #geom_violin(aes(color=Visibility, fill=Visibility), position=position_dodge(1), trim=TRUE) +
  geom_quasirandom(aes(color=share_lag2, fill=share_lag2), position=position_dodge(1))+
  geom_boxplot(color="black", position=position_dodge(1), width = 0.1, size = 1, outlier.shape=NA, fill=NA, notch=TRUE) +
  stat_summary(color="black" , fun.y=mean, geom="point",  size=3.5, shape=5, position=position_dodge(1) ) +
  geom_signif(test =  "t.test", #test.args=list(alternative = "greater", var.equal = FALSE, paired=TRUE),
              comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  xlab("Share decision\n at the previous round") +
  ylab("Number of visible neighbours")+
  guides(color=guide_legend(title="Share decision at\n the previous round"), fill=guide_legend(title="Share decision at\n the previous round")) +
  facet_grid(.~Visibility, labeller=label_both)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Num_visible_neighbours_byShareDecision.pdf", plot = plot_num_visible_neighbours_by_sharing_decision, height =8.5, width = 11, units = "in")





############################################
##Did sharers do better than non sharers ?
##########################################


subdata <- subset(realGame_data, round_num_in_game>2)
subdata$share_lag2 <- ifelse (subdata$share_lag2 == "Yes", 1, 0)
d <- ddply(subdata, ~participant.code+visibility_radius+treatments_order, summarize, sharingProb = mean(share_lag2), m = mean(normalized_game_payoff), error = se(normalized_game_payoff))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

game_payoff_by_sharingProb <- ggplot(d, aes(x = sharingProb, y = m, color = Visibility)) +
  geom_point( size=1)+
  geom_smooth( aes(fill = Visibility))+
  guides(color=guide_legend(title="Local visibility") , fill=guide_legend(title="Local visibility")) +
  xlab("Sharing frequency") +
  ylab("Game Payoff")+
  facet_grid( .~treatments_order)+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/GamePayoff_by_sharingProb.pdf", plot = game_payoff_by_sharingProb, height =4, width = 6, units = "in")




######################################################################
##HoW DOES SOLUTIONS SIMILARITY EVOLVE ACROSS TIME ?
#####################################################################


realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
dt <- ddply(subset(realGame_data, round_num_in_game>1), ~visibility_radius+treatments_order+round_num_in_game, summarize, m = mean(num_neighbours_in_visibility_radius), error = se(num_neighbours_in_visibility_radius))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
d$num_neighbours_in_visibility_radius <- d$num_neighbours_in_visibility_radius -1


plot_similarity_aross_time <- ggplot(dt, aes(x = round_num_in_game, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  scale_x_continuous(breaks = seq(0,15,5))+
  facet_grid( .~treatments_order, labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Round") +
  ylab("Number of neighbors in visibility radius") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/SimilarityEvolution.pdf", plot = plot_similarity_aross_time, height =4, width = 6, units = "in")





d1 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_1, summarize, m = mean(share), error = se(share))
d1$proximity <- 1
colnames(d1)[colnames(d1) == "num_others_in_distance_of_1"] ="num_others_in_proximity"

d2 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_2, summarize, m = mean(share), error = se(share))
d2$proximity <- 2
colnames(d2)[colnames(d2) == "num_others_in_distance_of_2"] ="num_others_in_proximity"

d3 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_3, summarize, m = mean(share), error = se(share))
d3$proximity <- 3
colnames(d3)[colnames(d3) == "num_others_in_distance_of_3"] ="num_others_in_proximity"

d4 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_4, summarize, m = mean(share), error = se(share))
d4$proximity <- 4
colnames(d4)[colnames(d4) == "num_others_in_distance_of_4"] ="num_others_in_proximity"

d5 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_5, summarize, m = mean(share), error = se(share))
d5$proximity <- 5
colnames(d5)[colnames(d5) == "num_others_in_distance_of_5"] ="num_others_in_proximity"

d6 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_6, summarize, m = mean(share), error = se(share))
d6$proximity <- 6
colnames(d6)[colnames(d6) == "num_others_in_distance_of_6"] ="num_others_in_proximity"

d7 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_7, summarize, m = mean(share), error = se(share))
d7$proximity <- 7
colnames(d7)[colnames(d7) == "num_others_in_distance_of_7"] ="num_others_in_proximity"

d8 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_8, summarize, m = mean(share), error = se(share))
d8$proximity <- 8
colnames(d8)[colnames(d8) == "num_others_in_distance_of_8"] ="num_others_in_proximity"

d9 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_9, summarize, m = mean(share), error = se(share))
d9$proximity <- 9
colnames(d9)[colnames(d9) == "num_others_in_distance_of_9"] ="num_others_in_proximity"

d10 <- ddply(realGame_data, ~visibility_radius+treatments_order+num_others_in_distance_of_10, summarize, m = mean(share), error = se(share))
d10$proximity <- 10
colnames(d10)[colnames(d10) == "num_others_in_distance_of_10"] ="num_others_in_proximity"

d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)

d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))



plot_sharing_and_proximity <- ggplot(d, aes(x = num_others_in_proximity, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  #geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  #scale_x_continuous(breaks = seq(0,5,1))+
  facet_grid( treatments_order~proximity, labeller=labeller(proximity=label_both, treatments_order=label_value))+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Number of neighbors in proximity") +
  ylab("Sharing probability") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Sharing_Proxmity.pdf", plot = plot_sharing_and_proximity, height =8.5, width = 11, units = "in")


d <- ddply(realGame_data, ~visibility_radius+treatments_order+mean_distance_to_others, summarize, m = mean(share), error = se(share))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_sharing_and_proximity2 <- ggplot(d, aes(x = mean_distance_to_others, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  geom_smooth( aes(color=Visibility, fill=Visibility),method="loess")+
  #geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  #scale_x_continuous(breaks = seq(0,5,1))+
  facet_grid( treatments_order~., labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Mean distance to others") +
  ylab("Sharing probability") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Sharing_Proxmity2.pdf", plot = plot_sharing_and_proximity2, height =6, width = 8, units = "in")


realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
d <- ddply(realGame_data, ~visibility_radius+treatments_order+num_neighbours_in_visibility_radius, summarize, m = mean(share), error = se(share))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
d$num_neighbours_in_visibility_radius <- d$num_neighbours_in_visibility_radius -1

plot_sharing_and_proximity3 <- ggplot(d, aes(x = num_neighbours_in_visibility_radius, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  #geom_smooth( aes(color=Visibility, fill=Visibility),method="loess")+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  #scale_x_continuous(breaks = seq(0,5,1))+
  facet_grid( treatments_order~., labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Number of neighbors in visibility radius") +
  ylab("Sharing probability") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Sharing_Proxmity3.pdf", plot = plot_sharing_and_proximity3, height =5, width = 6, units = "in")





realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
realGame_data$share_lag2_numeric <- ifelse (realGame_data$share_lag2 == "Yes", 1, 0)
d <- ddply(subset(realGame_data, round_num_in_game>2), ~participant.code+visibility_radius+treatments_order+game_in_treatment, summarize, m = mean(num_neighbours_in_visibility_radius-1), error = se(num_neighbours_in_visibility_radius), total_sharing = sum(share_lag2_numeric))
d <- ddply(d, ~visibility_radius+total_sharing, summarize, m = mean(m), error = se(error))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot <- ggplot(d, aes(x = total_sharing, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = Visibility), alpha=0.2)+
  geom_smooth( aes(color=Visibility, fill=Visibility),method="lm")+
  #scale_x_continuous(breaks = seq(0,15,5))+
  #facet_grid( .~treatments_order, labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Total number of sharing decisions") +
  ylab("Number of neighbors in visibility radius (t+2)") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/NumRecruitedVsTotalSharing.png", plot = plot, height =4, width = 6, units = "in")





#realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+game_in_treatment+num_neighbours_in_visibility_radius, summarize, m = mean(share), sum=sum(share) )
d$norm_m <- ifelse(d$sum==0, 0, d$m/d$sum)
d <- ddply(d, ~visibility_radius+num_neighbours_in_visibility_radius, summarize, m = mean(norm_m), error = se(norm_m))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
d$num_neighbours_in_visibility_radius <- d$num_neighbours_in_visibility_radius -1

plot <- ggplot(d, aes(x = num_neighbours_in_visibility_radius, y = m, color = Visibility, group = Visibility)) +
  geom_point()+
  geom_line(size=0.75)+
  #geom_smooth( aes(color=Visibility, fill=Visibility),method="lm")+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = share_lag2), alpha=0.2)+
  scale_x_continuous(breaks = seq(0,6,1))+
  #facet_grid( treatments_order~., labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  xlab("Number of neighbors in visibility radius") +
  ylab("Normalized Sharing probability") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/SharingVsNumVisible3.pdf", plot = plot, height =5, width = 6, units = "in")





realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
realGame_data$share_lag2_numeric <- ifelse (realGame_data$share_lag2 == "Yes", 1, 0)
d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+game_in_treatment, summarize, m = mean(num_neighbours_in_visibility_radius-1), m2 = sum(share), error = se(share))
d <- ddply(d, ~visibility_radius+m, summarize, m2 = mean(m2), error = se(error))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot <- ggplot(d, aes(x = m, y = m2, color = Visibility, group = Visibility)) +
  geom_point()+
  #geom_line(size=0.75)+
  #geom_errorbar(aes(ymin=m2 - 2*error, ymax=m2 + 2*error), size=0.75, width=.1) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = Visibility), alpha=0.2)+
  geom_smooth( aes(color=Visibility, fill=Visibility),method="lm")+
  #scale_x_continuous(breaks = seq(0,15,5))+
  #facet_grid( .~treatments_order, labeller=label_value)+
  guides(color=guide_legend(title="Visibility") , fill=guide_legend(title="Visibility")) +
  ylab("Total number of sharing decisions") +
  xlab("Average number of neighbors in visibility radius") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/NumNeighborsVsTotalSharing.pdf", plot = plot, height =4, width = 6, units = "in")



###########################################################################################
##How was performance across the game depending on the share decision ?
##########################################################################################

#Improvement in payoff
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2), ~round_num_in_game+share_lag2+visibility_radius+treatments_order+part+games, summarize, 
            m = mean(difference_in_game_payoff_lag2, na.rm=TRUE), error = se(difference_in_game_payoff_lag2))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
dt$Share <- dt$share_lag2

plot_improvement_game_payoff_aross_time <- ggplot(dt, aes( x = round_num_in_game, y=m, color = Share, group = Share)) +
  geom_point()+
  geom_line(size=0.75)+
  #geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  scale_x_continuous(breaks = seq(0,15,5))+
  facet_grid(treatments_order~part+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  guides(color=guide_legend(title="Share decision \n(two rounds ago)") , fill=guide_legend(title="Share decision \n(two rounds ago)") ) +
  #expand_limits(y=0)+
  xlab("Round") +
  ylab("Improvement in game payoff") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Improvement_game_payoff.pdf", plot = plot_improvement_game_payoff_aross_time, height =6, width = 9, units = "in")


###########################################################################################
##How was the number of splitters across the game depending on the share decision ?
##########################################################################################

#number of splitters
realGame_data$games <- ifelse (realGame_data$game_in_treatment>2, "Last 2 games", "First 2 games")
dt <- ddply(subset(realGame_data, round_num_in_game>2), ~round_num_in_game+share_lag2+visibility_radius+treatments_order+part+games, summarize, 
            m = mean(num_splitters, na.rm=TRUE), error = se(num_splitters))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
dt$Share <- dt$share_lag2

plot_num_splitters_aross_time <- ggplot(dt, aes( x = round_num_in_game, y=m, color = Share, group = Share)) +
  geom_point()+
  geom_line(size=0.75)+
  geom_errorbar(aes(ymin=m - 2*error, ymax=m + 2*error), size=0.5, width=.01, alpha=0.45) +
  #geom_ribbon(aes(ymin = m - 2*error, ymax = m + 2*error, fill = Share), alpha=0.2)+
  scale_x_continuous(breaks = seq(0,15,5))+
  facet_grid(treatments_order~part+games, labeller=labeller(part=label_value, Visibility=label_both, treatments_order=label_value, games=label_value, Share=label_both))+
  guides(color=guide_legend(title="Share decision \n(two rounds ago)") , fill=guide_legend(title="Share decision \n(two rounds ago)") ) +
  xlab("Round") +
  ylab("Number of splitters") +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
ggsave(filename = "Results/Num_splitters_acrossTime_by_treatment_and_setOfGames.pdf", plot = plot_num_splitters_aross_time, height =6, width = 9, units = "in")





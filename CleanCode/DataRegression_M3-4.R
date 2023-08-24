#house cleaning
rm(list=ls())

#load packages
packages <- c('plyr',  'dplyr', 'lme4', 'lmerTest', 'MuMIn', 'effects', 'ggplot2', 'sjPlot', 'dotwhisker', 'stargazer', 'ggbeeswarm', 'cowplot')
lapply(packages, require, character.only=TRUE)


#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")
colorsPalette_sharing <- c("#56B4E9","#E69F00")
colorsPalette_visibility <- c("#af8dc3","#7fbf7b")

#Squashing axis
source('CleanCode/squash_axis.R')


#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}


#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))


###################################################################################
#What determines sharing benefits, social information benefits\\ and performance?
##################################################################################


realGame_data$Visibility <- factor(realGame_data$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- ifelse( realGame_data$Visibility == "Yes", 1, 0)
realGame_data$treatments_order <- relevel(realGame_data$treatments_order, ref = "Visibility first")
realGame_data$treatments_order_dummy <- ifelse( realGame_data$treatments_order == "No visibility first", 1, 0)
realGame_data$part_factor <- as.factor(realGame_data$part)
realGame_data$part_factor <- relevel(realGame_data$part_factor, ref = "Part 1")

realGame_data$share_lag2_dummy <-  ifelse( realGame_data$share_lag2 == "Yes", 1, 0)

realGame_data$mean_additional_info <- apply(realGame_data, 1, function(x) (sum(x$mean_additional_info_from_my_visibilibity_rad_did_not_share, x$mean_additional_info_shared_with_me, na.rm=TRUE)/2))
realGame_data$max_additional_info <- apply(realGame_data, 1, function(x) (max(x$max_additional_info_from_my_visibilibity_rad_did_not_share, x$max_additional_info_shared_with_me, na.rm=TRUE)))
realGame_data$max_additional_info <- apply(realGame_data, 1, function(x) ifelse (x$max_additional_info== -Inf,0 ,x$max_additional_info ))


realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$best_observation <- apply(realGame_data, 1, function(x) (as.numeric(x$best_observation[9])/ x$currency_rate))

realGame_data$normalized_round_payoff_before_splitting_lag2 <- NA
realGame_data$diff_peers_vs_me <- NA
realGame_data$max_additional_info_lag2 <- NA
realGame_data$best_observation_lag2 <- NA
realGame_data$distance_to_max_lag2 <- NA
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]>2){
    realGame_data$normalized_round_payoff_before_splitting_lag2[r] <- realGame_data$normalized_round_payoff_before_splitting[r-2]
    realGame_data$max_additional_info_lag2[r] <- realGame_data$max_additional_info[r-2]
    realGame_data$best_observation_lag2[r] <- realGame_data$best_observation[r-2]
    realGame_data$diff_peers_vs_me[r] <- sum(realGame_data$max_additional_info[r],  -realGame_data$normalized_round_payoff[r-1], na.rm=TRUE) #realGame_data$diff_max_additional_info[r-2]
    realGame_data$distance_to_max_lag2[r] <- realGame_data$distance_to_max[r-2]
  }
}

realGame_data$diff_best_observation <- apply(realGame_data, 1, function(x) sum(x$best_observation,  -x$best_observation_lag2, na.rm=TRUE))

realGame_data$social_info_benefits <- apply(realGame_data, 1, function(x) sum(x$max_additional_info,  -x$max_additional_info_lag2, na.rm=TRUE))

#realGame_data$normalized_difference_in_round_payoff_lag2 <- realGame_data$difference_in_round_payoff_lag2 /  realGame_data$normalized_round_payoff_lag2
#-> normalized_difference_in_round_payoff_lag2 does not look normal, so better to use difference_in_round_payoff_lag2


########################################
##Model 1 ( "Sharing benefits", called Model 4 in the paper)
#######################################


MixedModel1 <- lmer(difference_in_round_payoff_lag2 ~  
                      Visibility
                    + round_num_in_game
                    + part_factor 
                    + log(normalized_round_payoff_before_splitting_lag2)
                    + (1 | participant.code)
                    + (1 + round_num_in_game   | group.id) #correlated random intercept and slope
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1)

# vif(MixedModel1)
# dwplot(MixedModel1)+geom_vline(xintercept=0,lty=2)
# r.squaredGLMM(MixedModel1)
# r.squaredLR(MixedModel1)

Effects_size_model1 <- plot_model(MixedModel1, 
                                  color = "grey30",
                                  vline.color = "grey75", line.size = 0.75,
                                  transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
                                  #axis.labels=c(  "Observed payoff two rounds backward (Log scale)", "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #axis.labels=c( "Constant", "Observed payoff two rounds backward (Log scale):Sharing two rounds backward",  "Sharing two rounds backward", "Observed payofftwo rounds backward (Log scale)", "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #show.intercept = TRUE,
                                  show.values=TRUE, show.p=TRUE,
                                  title="Sharing benefits")+
  ylab("Estimates (Log-Odds)") +
  #theme_sjplot()+ 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Effects_size_Model_sharing_benefits.pdf", plot = Effects_size_model1, height =4, width = 6, units = "in")



d <- ddply(subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ), ~participant.code+visibility_radius, summarize, m = mean(difference_in_round_payoff_lag2), error = se(difference_in_round_payoff_lag2))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))


plot_sharing_benefits <- plot_model(MixedModel1, type = "pred", terms = c("Visibility" ), dot.size=0.75)+
  geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(Visibility), y=m, color=Visibility, fill=Visibility), alpha=0.7)+
  #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
  #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  xlab("Visibility") +
  ylab("Sharing benefits")+
  ggtitle("")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"), legend.position = "none")+
  #scale_y_continuous(trans = squash_axis(10, 20, 10))+
  scale_y_continuous(breaks = c(-2.5, 0, 2.5, 5, 7.5,  12.5,  17.5), trans = squash_axis(7.5, 20, 10))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharingBenefits_quasirandom_squashedAxis2.pdf", plot = plot_sharing_benefits, height =5, width = 7.5, units = "in")


########################################
##Model 2 (called Model 3 in the paper)
#######################################


MixedModel2 <- lmer(social_info_benefits ~  
                      Visibility
                    + round_num_in_game
                    + part_factor 
                    + log(normalized_round_payoff_before_splitting_lag2)
                    + share_lag2
                    + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                    + (1 | participant.code)
                    + (1 + round_num_in_game | group.id) #correlated random intercept and slope
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2)

# vif(MixedModel2)
# dwplot(MixedModel2)+geom_vline(xintercept=0,lty=2)
# r.squaredGLMM(MixedModel2)
# r.squaredLR(MixedModel2)


Effects_size_model2 <- plot_model(MixedModel2, 
                                  color = "grey30",
                                  vline.color = "grey75", line.size = 0.75,
                                  transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
                                  #axis.labels=c( "Observed payoff two rounds backward (Log scale):Sharing two rounds backward", "Observed payoff two rounds backward (Log scale)",  "Sharing two rounds backward",  "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #show.intercept = TRUE,
                                  show.values=TRUE, show.p=TRUE,
                                  title="Social info benefits")+
  ylab("Estimates (Log-Odds)") +
  #theme_sjplot()+ 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Effects_size_Model_social_info_benefits.pdf", plot = Effects_size_model2, height =4, width = 6, units = "in")


d <- ddply(subset(realGame_data, round_num_in_game>2 ), ~participant.code+visibility_radius, summarize, m = mean(social_info_benefits), error = se(social_info_benefits))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))


plot_social_info_benefits <- plot_model(MixedModel2, type = "pred", terms = c("Visibility" ), dot.size=0.75)+
  geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(Visibility), y=m, color=Visibility, fill=Visibility), alpha=0.7)+
  #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
  #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  xlab("Visibility") +
  ylab("Social information benefits")+
  ggtitle("")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"), legend.position = "none")+
  #scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SocialInfoBenefits_quasirandom.pdf", plot = plot_social_info_benefits, height =5, width = 7.5, units = "in")


# ########################################
# ##Model 3 (called Model 5 in the paper)
# #######################################
# 
# MixedModel3 <- lmer(log(normalized_round_payoff) ~ 
#                       round_num_in_game
#                     + Visibility
#                     + part_factor 
#                     #+ share_lag2*visibility_radius_dummy#*part_factor
#                     + log(normalized_round_payoff_before_splitting_lag2)
#                     + share_lag2
#                     + share_lag2*log(normalized_round_payoff_before_splitting_lag2)#*visibility_radius_dummy
#                     + (1 | participant.code)
#                     + (1 + round_num_in_game   | group.id) #correlated random intercept and slope
#                     , #-1,
#                     control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
#                     REML = FALSE,
#                     data = subset(realGame_data, round_num_in_game>2 ))
# summary(MixedModel3)
# 
# # vif(MixedModel3)
# # dwplot(MixedModel3)+geom_vline(xintercept=0,lty=2)
# # r.squaredGLMM(MixedModel3)
# # r.squaredLR(MixedModel3)
# 
# Effects_size_model3 <- plot_model(MixedModel3, 
#                                   color = "grey30", # "#2166AC"
#                                   vline.color = "grey75", line.size = 0.75,
#                                   transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
#                                   axis.labels=c( "Observed payoff two rounds backward (Log scale):Sharing two rounds backward",  "Sharing two rounds backward", "Observed payoff two rounds backward (Log scale)",  "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
#                                   #show.intercept = TRUE,
#                                   show.values=TRUE, show.p=TRUE,
#                                   title="Payoff (Log scale)")+
#   ylab("Estimates (Log-Odds)") +
#   #theme_sjplot()+ 
#   theme_classic()+
#   theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
# #ggsave(filename = "Results/Effects_size_Model_perofrmance.pdf", plot = Effects_size_model3, height =4, width = 6, units = "in")
# 
# 
# 
# 
# d <- ddply(subset(realGame_data, round_num_in_game>2  ), ~participant.code+visibility_radius, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
# d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
# 
# 
# plot_performance <- plot_model(MixedModel3, type = "pred", terms = c("Visibility" ))+
#   geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(Visibility), y=m, color=Visibility, fill=Visibility), alpha=0.7)+
#   #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
#   #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
#   xlab("Visibility") +
#   ylab("Score")+
#   ggtitle("")+
#   guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
#   theme_classic()+
#   theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
#   #scale_y_continuous(trans = squash_axis(10, 20, 10))+
#   scale_y_continuous(breaks = c(0, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
#   coord_trans(y="log2")+
#   scale_fill_manual(values=colorsPalette_visibility)+
#   scale_color_manual(values=colorsPalette_visibility)
# ggsave(filename = "Results/Score_quasirandom.pdf", plot = plot_performance, height =5, width = 7.5, units = "in")
# 
# # see https://github.com/strengejacke/ggeffects/issues/62 about the warning (Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.)
# # "Standard errors aren't printed any longer anyway, so I could remove that part of the warning. According to your question: yes, predicted values and their CIs are on the correct scale (back transformed to the original scale)."





class(MixedModel1) <- "lmerMod"
class(MixedModel2) <- "lmerMod"
#class(MixedModel3) <- "lmerMod"
#stargazer(MixedModel3, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )
stargazer(MixedModel2,MixedModel1, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



# Effects_size <- plot_models(MixedModel1, MixedModel2, MixedModel3, 
#                             show.p = TRUE,
#                             show.values = TRUE,
#                             #p.shape = TRUE,
#                             vline.color = "grey75", 
#                             line.size = 0.5,
#                             dot.size = 1,
#                             #geom.colors = "Dark2", 
#                             wrap.labels = 48,
#                             grid = TRUE,
#                             axis.lim = c(-2,4),
#                             axis.labels=c( expression(paste(Log(Payoff[t-2]):Sharing[t-2])),  expression(paste(Sharing[t-2])),expression(paste(Log(Payoff[t-2]))),"Part 2", "Visibility",  "Round" ),
#                             m.labels = c( "Sharing benefits", "Social information benefits", "Log(Score)"))+
#   facet_grid(~group, scales = "free_x") + 
#   scale_y_continuous(limits = c(NA, NA), expand = c(0.5, 0)) +              
#   theme_classic()+
#   theme(strip.background=element_blank(), text = element_text(size=14, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 10, face="bold"))
# ggsave(filename = "Results/Regressions/Effects_size_M2-4.pdf", plot = Effects_size, height =5, width = 12, units = "in")



Effects_size <- plot_models(MixedModel1, MixedModel2, 
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
                            axis.labels=c( expression(paste(atop('Log(Payoff[t-2])','*Sharing[t-2]'))),  expression(paste(Sharing[t-2])), expression(paste(Log(Payoff[t-2]))), "Part 2",  "Round" , "Visibility"),
                            m.labels = c( "Sharing benefits\n (Model 4)", "Social information benefits\n(Model 3)"))+
  facet_grid(~group, scales = "free_x") + 
  scale_color_sjplot(palette = "reefs")+
  #ylim(-2, 4) +
  #scale_y_continuous(limits = c(NA, NA), expand = c(0.5, 0)) + 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 10, face="bold"), legend.position = "none") #, legend.position = c(.9, .5)
ggsave(filename = "Results/Regressions/Effects_size_M3-4.pdf", plot = Effects_size, height =5, width = 7.5, units = "in")




plot <- ggdraw() +
  draw_plot(Effects_size, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot(plot_social_info_benefits, x = 0.1, y = 0, width = 0.35, height = .5) +
  draw_plot(plot_sharing_benefits, x = 0.55, y = 0, width = 0.4, height = .5) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0, 0.55 ), y = c(1, 0.5, .5))
ggsave(filename = "Results/Model3-4_V2.pdf", plot = plot, height =7.25, width = 8, units = "in")

################################################
### Model Selection - Staircasing procedure  ###
################################################

########## ANOVA comparisons for fixed effects selection  ############

########## Model 2 ############

MixedModel3_1 <- lmer(log(normalized_round_payoff) ~ log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1  | participant.code)
                      + (1  | group.id)
                      , 
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3_1)

MixedModel3_2 <- lmer(log(normalized_round_payoff) ~  visibility_radius_dummy + log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1  | participant.code)
                      + (1  | group.id)
                      , 
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3_2)


MixedModel3_3 <- lmer(log(normalized_round_payoff) ~  round_num_in_game + visibility_radius_dummy  + log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1  | participant.code)
                      + (1  | group.id)
                      , 
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3_3)

MixedModel3 <- lmer(log(normalized_round_payoff) ~  round_num_in_game + visibility_radius_dummy + part_factor + log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                    + (1  | participant.code)
                    + (1  | group.id)
                    , 
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3)




anova(MixedModel3_1, MixedModel3_2)
anova(MixedModel3_2, MixedModel3_3)
anova(MixedModel3_3, MixedModel3)



########## Model 3 ############

MixedModel2_1 <- lmer(social_info_benefits ~  log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2_1)

MixedModel2_2 <- lmer(social_info_benefits ~  visibility_radius_dummy + log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2_2)

MixedModel2_3 <- lmer(social_info_benefits ~   round_num_in_game + visibility_radius_dummy +  log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2_3)

MixedModel2 <- lmer(social_info_benefits ~   round_num_in_game + visibility_radius_dummy + part_factor +  log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                    + (1   | participant.code)
                    + (1  | group.id) 
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2)



anova(MixedModel2_1, MixedModel2_2)
anova(MixedModel2_2, MixedModel2_3)
anova(MixedModel2_3, MixedModel2)



########## Model 4 ############

MixedModel1_1 <- lmer(difference_in_round_payoff_lag2 ~   log(normalized_round_payoff_before_splitting_lag2) 
                      + (1 | participant.code) 
                      + (1 | group.id)
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1_1)

MixedModel1_2 <- lmer(difference_in_round_payoff_lag2 ~  visibility_radius_dummy + log(normalized_round_payoff_before_splitting_lag2)  
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1_2)

MixedModel1_3 <- lmer(difference_in_round_payoff_lag2 ~   round_num_in_game + visibility_radius_dummy +  log(normalized_round_payoff_before_splitting_lag2)  
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1_3)

MixedModel1 <- lmer(difference_in_round_payoff_lag2 ~   round_num_in_game + visibility_radius_dummy + part_factor +  log(normalized_round_payoff_before_splitting_lag2)  
                    + (1   | participant.code)
                    + (1  | group.id) 
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1)

anova(MixedModel1_1, MixedModel1_2)
anova(MixedModel1_2, MixedModel1_3)
anova(MixedModel1_3, MixedModel1)




########## ANOVA comparisons for random effects selection  ############


########## Model 2 ############

MixedModel3_1 <- lmer(log(normalized_round_payoff) ~ visibility_radius_dummy + round_num_in_game   + part_factor +log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1  | participant.code)
                      + (1  | group.id)
                      , 
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3_1)

MixedModel3_2 <- lmer(log(normalized_round_payoff) ~ visibility_radius_dummy + round_num_in_game   + part_factor +log(normalized_round_payoff_before_splitting_lag2) + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1 | participant.code)
                      + (1 + round_num_in_game | group.id)
                      , 
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3_2)

anova(MixedModel3_1, MixedModel3_2)




MixedModel2_1 <- lmer(social_info_benefits ~   visibility_radius_dummy + round_num_in_game   + part_factor +log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code)
                      + (1  | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2_1)

MixedModel2_2 <- lmer(social_info_benefits ~    visibility_radius_dummy + round_num_in_game   + part_factor +log(normalized_round_payoff_before_splitting_lag2)  + share_lag2 + share_lag2*log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code)
                      + (1 + round_num_in_game   | group.id) 
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2_2)

anova(MixedModel2_1, MixedModel2_2)



MixedModel1_1 <- lmer(difference_in_round_payoff_lag2 ~ visibility_radius_dummy + round_num_in_game   + part_factor + log(normalized_round_payoff_before_splitting_lag2)
                      + (1 | participant.code) 
                      + (1 | group.id)
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1_1)

MixedModel1_2 <- lmer(difference_in_round_payoff_lag2 ~ visibility_radius_dummy + round_num_in_game   + part_factor + log(normalized_round_payoff_before_splitting_lag2)
                      + (1   | participant.code) 
                      + (1 + round_num_in_game   | group.id)
                      , #-1,
                      control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                      REML = FALSE,
                      data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes"  ))
summary(MixedModel1_2)


anova(MixedModel1_1, MixedModel1_2)





#load packages
packages <- c('lme4' , 'lmerTest', 'stargazer', 'ggplot2', 'sjPlot', 'dotwhisker') 
lapply(packages, require, character.only=TRUE)

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


########################################
##Model 1 ( "Sharing benefits", called Model 3 in the paper)
#######################################


MixedModel1 <- lmer(difference_in_round_payoff_lag2 ~  
                     round_num_in_game
                    + visibility_radius_dummy
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
                                  axis.labels=c(  "Observed payofftwo rounds backward (Log scale)", "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #axis.labels=c( "Constant", "Observed payoff two rounds backward (Log scale):Sharing two rounds backward",  "Sharing two rounds backward", "Observed payofftwo rounds backward (Log scale)", "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #show.intercept = TRUE,
                                  show.values=TRUE, show.p=TRUE,
                                  title="Sharing benefits")+
  ylab("Estimates (Log-Odds)") +
  #theme_sjplot()+ 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Effects_size_Model_sharing_benefits.pdf", plot = Effects_size_model1, height =4, width = 6, units = "in")


########################################
##Model 2 (called Model 4 in the paper)
#######################################


MixedModel2 <- lmer(social_info_benefits ~  
                     round_num_in_game
                    + visibility_radius_dummy
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
                                  axis.labels=c( "Observed payoff two rounds backward (Log scale):Sharing two rounds backward", "Observed payoff two rounds backward (Log scale)",  "Sharing two rounds backward",  "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #show.intercept = TRUE,
                                  show.values=TRUE, show.p=TRUE,
                                  title="Social info benefits")+
  ylab("Estimates (Log-Odds)") +
  #theme_sjplot()+ 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Effects_size_Model_social_info_benefits.pdf", plot = Effects_size_model2, height =4, width = 6, units = "in")


########################################
##Model 3 (called Model 5 in the paper)
#######################################

MixedModel3 <- lmer(log(normalized_round_payoff) ~ 
                      round_num_in_game
                    + visibility_radius_dummy
                    + part_factor 
                    #+ share_lag2*visibility_radius_dummy#*part_factor
                    + log(normalized_round_payoff_before_splitting_lag2)
                    + share_lag2
                    + share_lag2*log(normalized_round_payoff_before_splitting_lag2)#*visibility_radius_dummy
                    + (1 | participant.code)
                    + (1 + round_num_in_game   | group.id) #correlated random intercept and slope
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel3)

# vif(MixedModel3)
# dwplot(MixedModel3)+geom_vline(xintercept=0,lty=2)
# r.squaredGLMM(MixedModel3)
# r.squaredLR(MixedModel3)

Effects_size_model3 <- plot_model(MixedModel3, 
                                  color = "grey30", # "#2166AC"
                                  vline.color = "grey75", line.size = 0.75,
                                  transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
                                  axis.labels=c( "Observed payoff two rounds backward (Log scale):Sharing two rounds backward",  "Sharing two rounds backward", "Observed payoff two rounds backward (Log scale)",  "Playing the second part of the experiment",  "Visibility", "Round in the game"  ),
                                  #show.intercept = TRUE,
                                  show.values=TRUE, show.p=TRUE,
                                  title="Payoff (Log scale)")+
  ylab("Estimates (Log-Odds)") +
  #theme_sjplot()+ 
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Effects_size_Model_perofrmance.pdf", plot = Effects_size_model3, height =4, width = 6, units = "in")



class(MixedModel1) <- "lmerMod"
class(MixedModel2) <- "lmerMod"
class(MixedModel3) <- "lmerMod"
#stargazer(MixedModel3, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )
stargazer(MixedModel3,MixedModel2,MixedModel1, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



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
                            axis.labels=c( expression(paste(Log(Payoff[t-2]):Sharing[t-2])),  expression(paste(Sharing[t-2])),expression(paste(Log(Payoff[t-2]))),"Part 2", "Visibility",  "Round" ),
                            m.labels = c( "Sharing benefits", "Social information benefits", "Log(Score)"))+
  facet_grid(~group, scales = "free_x") + 
  scale_y_continuous(limits = c(NA, NA), expand = c(0.5, 0)) +              
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=14, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 10, face="bold"))
ggsave(filename = "Results/Regressions/Effects_size_M2-4.pdf", plot = Effects_size, height =5, width = 12, units = "in")




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





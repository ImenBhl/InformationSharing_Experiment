#load packages
#packages <- c('pglm', 'plm', 'lme4' , 'lmerTest', 'corrplot', 'stargazer', 'texreg', 'car', 'dotwhisker' , 'MuMIn', 'effects', 'sjmisc') 
packages <- c('lme4' , 'lmerTest', 'stargazer',  'dotwhisker') 
lapply(packages, require, character.only=TRUE)

#load data
realGame_data<-get(load("Code/RealGameData.Rdata"))


###################################################################################
#What determines sharing benefits, social information benefits\\ and performance?
##################################################################################


realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- ifelse( realGame_data$Visibility == "Yes", 1, 0)
realGame_data$treatments_order <- relevel(realGame_data$treatments_order, ref = "Visibility first")
realGame_data$treatments_order_dummy <- ifelse( realGame_data$treatments_order == "No visibility first", 1, 0)
realGame_data$part_factor <- as.factor(realGame_data$part)
realGame_data$part_factor <- relevel(realGame_data$part_factor, ref = "Treatment 1")

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
  print (r)
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
##Model 1 (called Model 3 in the paper)
#######################################

MixedModel1 <- lmer(difference_in_round_payoff_lag2 ~  round_num_in_game
                   + game_in_treatment
                   + visibility_radius_dummy
                   + part_factor 
                   + normalized_round_payoff_before_splitting_lag2
                   + (1|participant.code)
                   + (1 + visibility_radius_dummy | group.id) #correlated random intercept and slope
                   , #-1,
                   control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   REML = FALSE,
                   data = subset(realGame_data, round_num_in_game>2 & share_lag2=="Yes" ))
summary(MixedModel1)
vif(MixedModel1)
dwplot(MixedModel1)+geom_vline(xintercept=0,lty=2)
r.squaredGLMM(MixedModel1)
r.squaredLR(MixedModel1)


########################################
##Model 2 (called Model 4 in the paper)
#######################################


MixedModel2 <- lmer(social_info_benefits ~  round_num_in_game
                   + game_in_treatment
                   + visibility_radius_dummy
                   + part_factor 
                   + share_lag2
                   + normalized_round_payoff_before_splitting_lag2
                   + (1|participant.code)
                   + (1 + visibility_radius_dummy | group.id)
                   , #-1,
                   control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   REML = FALSE,
                   data = subset(realGame_data, round_num_in_game>2))
summary(MixedModel2)
vif(MixedModel2)
dwplot(MixedModel2)+geom_vline(xintercept=0,lty=2)
r.squaredGLMM(MixedModel2)
r.squaredLR(MixedModel2)


MixedModel2bis <- lmer(social_info_benefits ~  round_num_in_game
                    + game_in_treatment
                    + visibility_radius_dummy
                    + part_factor
                    + share_lag2
                    + normalized_round_payoff_before_splitting_lag2
                    + normalized_round_payoff_before_splitting_lag2:share_lag2
                    + (1|participant.code)
                    + (1 + visibility_radius_dummy | group.id)
                    , #-1,
                    control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                    REML = FALSE,
                    data = subset(realGame_data, round_num_in_game>2 ))
summary(MixedModel2bis)
vif(MixedModel2bis)
dwplot(MixedModel2bis)+geom_vline(xintercept=0,lty=2)
r.squaredGLMM(MixedModel2bis)
r.squaredLR(MixedModel2bis)


########################################
##Model 3 (called Model 5 in the paper)
#######################################

MixedModel3 <- lmer(log(normalized_round_payoff) ~  #
                        round_num_in_game
                       + game_in_treatment
                       + visibility_radius_dummy
                       + part_factor 
                       + share_lag2
                       + log(normalized_round_payoff_before_splitting_lag2)
                       + log(normalized_round_payoff_before_splitting_lag2):share_lag2
                       #+ social_info_benefits
                       #+ diff_peers_vs_me
                       + (1|participant.code)
                       #!!!#
                       + (1 + visibility_radius_dummy | group.id)
                       , #-1,
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                       REML = FALSE,
                       data = subset(realGame_data, round_num_in_game>2))
summary(MixedModel3)
vif(MixedModel3)
dwplot(MixedModel3)+geom_vline(xintercept=0,lty=2)
r.squaredGLMM(MixedModel3)
r.squaredLR(MixedModel3)



class(MixedModel1) <- "lmerMod"
class(MixedModel2) <- "lmerMod"
class(MixedModel2bis) <- "lmerMod"
class(MixedModel3) <- "lmerMod"
stargazer(MixedModel1, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )
stargazer(MixedModel1,MixedModel2,MixedModel2bis,MixedModel3, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )




######################
### Interaction plots
#####################

# Model 2bis (in the paper: Model 4)

e <- allEffects(MixedModel2bis)
#plot(e)
interaction_share_payoff <- e[[5]]
e.df <- as.data.frame(interaction_share_payoff)

g2_bis <- ggplot(e.df,aes(x=normalized_round_payoff_before_splitting_lag2,y=fit,color=share_lag2,fill=share_lag2,ymin=fit-se,ymax=fit+se))+  
  geom_line( linetype = "dashed")+
  geom_ribbon(alpha=0.2)+
  guides(color=guide_legend(title="Sharing two rounds ago") , fill=guide_legend(title="Sharing two rounds ago")) +
  xlab("Observed payoff (two rounds ago)") +
  ylab("Social info benefits") +
  ggtitle("Interaction plot: \nEffect of sharing decision and the corresponding observed payoff on social info benefits")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
plot(g2_bis)
ggsave(filename = "Results/InteractionPlot_Sharing_ObservedPayoff_Model4.pdf", plot = g2_bis, height =7, width = 8.5, units = "in")



#Model3 (in the paper: Model 5)

e <- allEffects(MixedModel3)
#plot(e)
interaction_share_payoff <- e[[5]]
e.df <- as.data.frame(interaction_share_payoff)

g3 <- ggplot(e.df,aes(x=normalized_round_payoff_before_splitting_lag2,y=fit,color=share_lag2,fill=share_lag2,ymin=fit-se,ymax=fit+se))+  
  geom_line( linetype = "dashed")+
  geom_ribbon(alpha=0.2)+
  guides(color=guide_legend(title="Sharing two rounds ago") , fill=guide_legend(title="Sharing two rounds ago")) +
  xlab("Observed payoff (two rounds ago)") +
  ylab("Payoff") +
  ggtitle("Interaction plot: \nEffect of sharing decision and the corresponding observed payoff on the obtained payoff")+
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"))+
  scale_fill_manual(values=cbbPalette)+
  scale_color_manual(values=cbbPalette)
plot(g3)
ggsave(filename = "Results/InteractionPlot_Sharing_ObservedPayoff_Model5.pdf", plot = g3, height =7, width = 8.5, units = "in")


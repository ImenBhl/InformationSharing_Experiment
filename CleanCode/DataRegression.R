#house cleaning
rm(list=ls())

#load packages
#packages <- c('readxl', 'plyr',  'dplyr',  'ggplot2', 'RColorBrewer','pals', 'vioplot', 'aplpack', 'moments', 'nortest', 'hexbin', 'plotly', 'ggsignif', 'ggpubr', 'cowplot', 'lme4', 'stargazer', 'viridis', 'ggbeeswarm', 'dotwhisker', 'pglm', 'MuMIn', 'effects')
packages <- c('lme4', 'MuMIn', 'effects')
lapply(packages, require, character.only=TRUE)

#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")

#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}


#load data
realGame_data<-get(load("Code/RealGameData.Rdata"))


########################################
##What determines the sharing decision?
#######################################

### MODEL 1 ###

realGame_data$Visibility <- factor(realGame_data$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- realGame_data$visibility_radius/2
realGame_data$part_id <- ifelse(realGame_data$part=="Treatment 1", 1, 2)
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$best_observation <- apply(realGame_data, 1, function(x) as.numeric(x$best_observation[9])/x$currency_rate)
realGame_data$best_observation <- ifelse(is.na(realGame_data$best_observation),0, realGame_data$best_observation)
realGame_data$best_observation <- ifelse(realGame_data$normalized_round_payoff_before_splitting>realGame_data$best_observation,realGame_data$normalized_round_payoff_before_splitting, realGame_data$best_observation)
realGame_data$difference_from_best <- realGame_data$normalized_round_payoff_before_splitting-realGame_data$best_observation


m1 <- glmer(share ~ Visibility +  normalized_round_payoff_before_splitting + round_num_in_game + game_in_treatment
            + (1| participant.code) #+ (1 | group.id_in_subsession) #
            + (visibility_radius_dummy -1 | group.id)
            #+ (1 | session.code)
            , #-1,
            data = subset(realGame_data, round_num_in_game>1),
            family = binomial(link = "logit"))
summary(m1)
plot(allEffects(m1))

stargazer(m1, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



#####################################################################
##What determines the sharing decision  in the visibility treatment ?
#####################################################################

### MODEL 2 ###
realGame_data$num_sharers_in_group <- apply(realGame_data, 1, function(x) sum(x$my_sharing_neighbours!=x$id_in_group))
realGame_data$num_neighbours_in_visibility_radius <- apply(realGame_data, 1, function(x) length(x$neighbours_in_my_visibility_radius)-1)
realGame_data$num_all_visibile_neighbours <- apply(realGame_data, 1, function(x) length(x$all_visibile_neighbours)-1)
realGame_data$treatments_order_dummy <- ifelse( realGame_data$treatments_order == "No visibility first", 1, 0)
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$num_sharers_in_group_lag1 <- as.numeric(realGame_data$num_sharers_in_group_lag1)
realGame_data$num_sharers_in_group_lag2 <- as.numeric(realGame_data$num_sharers_in_group_lag2)


m2 <- glmer(share ~ normalized_round_payoff_before_splitting
              + treatments_order_dummy
              + as.numeric(num_sharers_in_group_lag1)
              + num_neighbours_in_visibility_radius
              #+ num_neighbours_in_visibility_radius:treatments_order#_dummy
              #+ (1| participant.code) 
              #+ (game_in_treatment - 1 | participant.code)
              + (1 + game_in_treatment | participant.code)
              #+ (1 | group.id)
              #+ (round_num_in_game - 1 | group.id)
              + (1 + round_num_in_game | group.id)
              #+ (1 | session.code)
              , #-1,
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
              data = subset(realGame_data, round_num_in_game>1 & Visibility=="Yes") ,
              family = binomial(link = "logit"))
summary(m2)
plot(allEffects(m2))

r.squaredGLMM(m2)
r.squaredLR(m2)

stargazer(m2, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )






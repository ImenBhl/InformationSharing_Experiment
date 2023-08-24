#house cleaning
rm(list=ls())

#load packages
packages <- c('lme4' , 'lmerTest', 'stargazer', 'ggplot2', 'sjPlot', 'dotwhisker') 
lapply(packages, require, character.only=TRUE)


#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))


realGame_data$Visibility <- factor(realGame_data$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- realGame_data$visibility_radius/2
#realGame_data$part_id <- ifelse(realGame_data$part=="Treatment 1", 1, 2)
realGame_data$part_factor <- as.factor(realGame_data$part)
realGame_data$part_factor <- relevel(realGame_data$part_factor, ref = "Part 1")
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$normalized_round_payoff_before_splitting_rounded <- round(realGame_data$normalized_round_payoff_before_splitting , 2)
realGame_data$log_normalized_round_payoff_before_splitting <- log(realGame_data$normalized_round_payoff_before_splitting_rounded)
realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)-1







m1test <- glmer(share ~ Visibility + part_factor + round_num_in_game
                + log_normalized_round_payoff_before_splitting
                + log_normalized_round_payoff_before_splitting*Visibility
                + num_neighbours_in_visibility_radius
                + num_neighbours_in_visibility_radius*Visibility
                + (1 | participant.code)
                + (1 + round_num_in_game | group.id)
                , #-1,
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                data = subset(realGame_data, round_num_in_game>1),
                family = binomial(link = "logit"))
summary(m1test)
#plot(allEffects(m1final))

class(m1test) <- "lmerMod"
stargazer(m1test, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )




realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)
realGame_data$max_distance_to_others <- as.numeric(realGame_data$max_distance_to_others)
realGame_data$min_distance_to_others <- as.numeric(realGame_data$min_distance_to_others)


m1test2 <- lmer(num_neighbours_in_visibility_radius ~ share_lag2*Visibility*log_normalized_round_payoff_before_splitting
                + part_factor 
                + round_num_in_game
                + (1 | participant.code)
                + (1 + round_num_in_game | group.id)
                , #-1,
                control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                REML = FALSE,
                data = subset(realGame_data, round_num_in_game>2))
summary(m1test2)
#plot(allEffects(m1final))

plot_model(m1test2, type = "int")
plot_model(m1test2, type = "pred")
plot_model(m1test2, type = "pred", terms = c("share_lag2", "Visibility" ))

class(m1test2) <- "lmerMod"
stargazer(m1test2, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



MixedModel4 <- lmer(num_neighbours_in_visibility_radius ~  
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
summary(MixedModel4)





realGame_data$num_neighbours_in_visibility_radius <- as.numeric(realGame_data$num_neighbours_in_visibility_radius)


m1test3 <- lmer(log_normalized_round_payoff_before_splitting ~ Visibility
                + part_factor 
                + round_num_in_game
                + (1 | participant.code)
                + (1 + round_num_in_game | group.id)
                , #-1,
                control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                REML = FALSE,
                data = subset(realGame_data,  share == 1 ))
summary(m1test3)

class(m1test3) <- "lmerMod"
stargazer(m1test3, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )

plot_model(m1test3, type = "pred")




m1test4 <- lmer(log_normalized_round_payoff_before_splitting ~ Visibility
                + part_factor 
                + round_num_in_game
                + num_neighbours_in_visibility_radius*Visibility 
                + (1 | participant.code)
                + (1 + round_num_in_game | group.id)
                , #-1,
                control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                REML = FALSE,
                data = subset(realGame_data,  share == 1 ))
summary(m1test4)

class(m1test4) <- "lmerMod"
stargazer(m1test4, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )


plot_model(m1test4, type = "int")
plot_model(m1test4, type = "pred")
plot_model(m1test4, type = "pred", terms = c("num_neighbours_in_visibility_radius", "Visibility" ))


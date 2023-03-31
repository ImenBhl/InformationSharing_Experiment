#house cleaning
rm(list=ls())

#load packages
packages <- c('lme4', 'MuMIn', 'effects', 'ggplot2', 'sjPlot', 'stargazer')
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






########################################
##What determines the sharing decision?
#######################################

### MODEL 1 ###



m1 <- glmer(share ~ Visibility + part_factor + round_num_in_game
            + (1 | participant.code)
            + (1 + round_num_in_game | group.id)
            , #-1,
            control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
            data = subset(realGame_data, round_num_in_game>1),
            family = binomial(link = "logit"))
summary(m1)
#plot(allEffects(m1))

class(m1) <- "lmerMod"
stargazer(m1, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )


EffectSize_m1 <-
plot_model(m1,
           color = "grey30",
           vline.color = "grey75", line.size = 0.75,
           show.values=TRUE, show.p=TRUE,
           transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
           axis.title = "Estimates",# (Log-Odds)
           title = "Sharing decision",
           axis.labels = c("Round" ,"Part 2" , "Visibility"),
           axis.lim= c(-1, 1),
           wrap.labels = 30
           ) + 
          theme_classic()+ 
          #theme_sjplot()+
          theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Regressions/Effects_size_M1.pdf", plot = EffectSize_m1, height =4, width = 6, units = "in")




m1bis <- glmer(share ~ Visibility + part_factor + round_num_in_game 
               + log_normalized_round_payoff_before_splitting
               + (1 | participant.code)
               + (1 + round_num_in_game | group.id)
               , #-1,
               control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
               data = subset(realGame_data, round_num_in_game>1),
               family = binomial(link = "logit"))
summary(m1bis)
#plot(allEffects(m1bis))

class(m1bis) <- "lmerMod"
stargazer(m1bis, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



EffectSize_m1bis <-
  plot_model(m1bis,
             color = "grey30",
             vline.color = "grey75", line.size = 0.75,
             show.values=TRUE, show.p=TRUE,
             transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
             axis.title = "Estimates",# (Log-Odds)
             title = "Sharing decision",
             axis.labels = c("Log(Payoff)", "Round" , "Part 2" , "Visibility"),
             axis.lim= c(-1, 1),
             wrap.labels = 30
  ) + 
  theme_classic()+ 
  #theme_sjplot()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
#ggsave(filename = "Results/Regressions/Effects_size_M1.pdf", plot = EffectSize_m1bis, height =4, width = 6, units = "in")





###### m1final is our Model 1 in the paper

m1final <- glmer(share ~ Visibility + part_factor + round_num_in_game
               + log_normalized_round_payoff_before_splitting
               + log_normalized_round_payoff_before_splitting*Visibility
               + (1 | participant.code)
               + (1 + round_num_in_game | group.id)
               , #-1,
               control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
               data = subset(realGame_data, round_num_in_game>1),
               family = binomial(link = "logit"))
summary(m1final)
#plot(allEffects(m1final))

class(m1final) <- "lmerMod"
stargazer(m1final, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



EffectSize_m1final <-
  plot_model(m1final,
             color = "grey30",
             vline.color = "grey75", 
             line.size = 0.5,
             dot.size = 1,
             #value.size = 3,
             show.values=TRUE, show.p=TRUE,
             transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
             axis.title = "Estimates",# (Log-Odds)
             title = "Sharing decision",
             axis.labels = c("Log(Payoff)\n*Visibility", "Log(Payoff)", "Round" , "Part 2" , "Visibility"),
             axis.lim= c(-1, 1),
             wrap.labels = 30
  ) + 
  ylim(-.6, .2) +
  theme_classic()+ 
  #theme_sjplot()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
ggsave(filename = "Results/Regressions/Effects_size_M1.pdf", plot = EffectSize_m1final, height =4, width = 6, units = "in")





# EffectSize_m1_and_m1bis <-
#   plot_models(m1, m1bis,
#              #color = "grey30",
#              vline.color = "grey75", 
#              line.size = 0.5,
#              dot.size = 1,
#              spacing = 0.5,
#              show.values=TRUE, show.p=TRUE,
#              transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
#              axis.title = "Estimates",# (Log-Odds)
#              legend.title = "",
#              m.labels = c("Model 1a", "Model 1b"),
#              title = "Sharing decision",
#              axis.labels = c("Log(Payoff)", "Round" , "Part 2" , "Visibility"),
#              axis.lim= c(-1, 1),
#              wrap.labels = 30
#   ) +
#   ylim(-.4, .2) +
#   theme_classic()+ 
#   #theme_sjplot()+
#   theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))


# EffectSize_m1_m1bis_m1final <-
#   plot_models(m1, m1bis, m1final,
#               #color = "grey30",
#               vline.color = "grey75", 
#               line.size = 0.5,
#               dot.size = 1,
#               value.size = 2.5,
#               spacing = 0.6,
#               show.values=TRUE, show.p=TRUE,
#               transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
#               axis.title = "Estimates",# (Log-Odds)
#               legend.title = "",
#               m.labels = c("Model 1a", "Model 1b", "Model 1c"),
#               title = "Sharing decision",
#               axis.labels = c("Log(Payoff)\n*Visibility", "Log(Payoff)", "Round" , "Part 2" , "Visibility"),
#               axis.lim= c(-1, 1),
#               wrap.labels = 30
#   ) +
#   ylim(-.8, .5) +
#   theme_classic()+ 
#   #theme_sjplot()+
#   theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))


################################################
### Model Selection - Staircasing procedure  ###
################################################

########## ANOVA comparisons for fixed effects selection  ############

m1final_1 <- glmer(share ~ Visibility 
                   + (1  | participant.code) + (1 | group.id), 
                   control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   data = subset(realGame_data, round_num_in_game>1),
                   family = binomial(link = "logit"))
summary(m1final_1)

m1final_2 <- glmer(share ~ Visibility + part_factor
                   + (1  | participant.code) + (1 | group.id), 
                   control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   data = subset(realGame_data, round_num_in_game>1),
                   family = binomial(link = "logit"))
summary(m1final_2)

m1final_3 <- glmer(share ~ Visibility + part_factor + round_num_in_game
                   + (1  | participant.code) + (1 | group.id), 
                   control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   data = subset(realGame_data, round_num_in_game>1),
                   family = binomial(link = "logit"))
summary(m1final_3)

m1final_4 <- glmer(share ~ Visibility + part_factor   + round_num_in_game + log_normalized_round_payoff_before_splitting
                   + (1  | participant.code) + (1 | group.id), 
                   control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   data = subset(realGame_data, round_num_in_game>1),
                   family = binomial(link = "logit"))
summary(m1final_4)

m1final_5 <- glmer(share ~ Visibility + part_factor  + round_num_in_game + log_normalized_round_payoff_before_splitting + log_normalized_round_payoff_before_splitting*Visibility
                   + (1  | participant.code) + (1 | group.id), 
                   control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                   data = subset(realGame_data, round_num_in_game>1),
                   family = binomial(link = "logit"))
summary(m1final_5)

anova(m1final_1, m1final_2)
anova(m1final_2, m1final_3)
anova(m1final_3, m1final_4)
anova(m1final_4, m1final_5)


########## ANOVA comparisons for random effects selection  ############

m1final_1 <- glmer(share ~ Visibility + part_factor + round_num_in_game + log_normalized_round_payoff_before_splitting  + log_normalized_round_payoff_before_splitting*Visibility
              + (1  | participant.code) + (1  | group.id), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
              data = subset(realGame_data, round_num_in_game>1),
              family = binomial(link = "logit"))
summary(m1final_1)

m1final_2 <- glmer(share ~ Visibility + part_factor + round_num_in_game + log_normalized_round_payoff_before_splitting + log_normalized_round_payoff_before_splitting*Visibility
              + (1  | participant.code) + (1 + round_num_in_game | group.id), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
              data = subset(realGame_data, round_num_in_game>1),
              family = binomial(link = "logit"))
summary(m1final_2)

# m1final_3 <- glmer(share ~ Visibility + part_factor + log_normalized_round_payoff_before_splitting + round_num_in_game + log_normalized_round_payoff_before_splitting*Visibility
#               + (1 + round_num_in_game | participant.code) + (1  | group.id),
#               control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
#               data = subset(realGame_data, round_num_in_game>1),
#               family = binomial(link = "logit"))
# summary(m1final_3)
# 
# m1final_4 <- glmer(share ~ Visibility + part_factor + log_normalized_round_payoff_before_splitting + round_num_in_game + log_normalized_round_payoff_before_splitting*Visibility
#             + (1 + round_num_in_game | participant.code)  + (1 + round_num_in_game | group.id),
#             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
#             data = subset(realGame_data, round_num_in_game>1),
#             family = binomial(link = "logit"))
# summary(m1final_4)


anova(m1final_1, m1final_2)
# anova(m1final_1, m1final_3)
# anova(m1final_2, m1final_4)
# anova(m1final_3, m1final_4)



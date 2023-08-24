#house cleaning
rm(list=ls())

#load packages
packages <- c('plyr',  'dplyr', 'lme4', 'MuMIn', 'effects', 'ggplot2', 'sjPlot', 'stargazer', 'ggbeeswarm', 'cowplot')
lapply(packages, require, character.only=TRUE)



#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")
colorsPalette_sharing <- c("#56B4E9","#E69F00")
colorsPalette_visibility <- c("#af8dc3","#7fbf7b")


#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}


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

m1final <- glmer(share ~ Visibility 
               + log(normalized_round_payoff_before_splitting)
               + round_num_in_game
               + part_factor 
               #+ log(normalized_round_payoff_before_splitting)*Visibility
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
             value.offset = .3,
             show.values=TRUE, show.p=TRUE,
             transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
             axis.title = "Estimates",# (Log-Odds)
             title = "Sharing decision",
             axis.labels = c( "Part 2" , "Round" ,  "Log(Payoff)","Visibility"),
             axis.lim= c(-1, 1),
             wrap.labels = 30
  ) + 
  ylim(-.4, .1) +
  theme_classic()+ 
  #theme_sjplot()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
ggsave(filename = "Results/Regressions/Effects_size_M1.pdf", plot = EffectSize_m1final, height =4, width = 6, units = "in")





d <- ddply(subset(realGame_data, round_num_in_game>1), ~participant.code+visibility_radius, summarize, m = mean(share), error = se(share))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

plot_sharing_rate <- plot_model(m1final, type = "pred", terms = c("Visibility" ), dot.size=0.75)+
  geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(Visibility), y=m, color=Visibility, fill=Visibility), alpha=0.6)+
  #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
  #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  xlab("Visibility") +
  ylab("P(share)")+
  ggtitle("")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"), legend.position="none")+ #,legend.position = c(.5, .75)
  #scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharingRate_quasirandom.pdf", plot = plot_sharing_rate, height =5, width = 7.5, units = "in")




d <- ddply(subset(realGame_data, round_num_in_game>1), ~normalized_round_payoff_before_splitting_rounded+visibility_radius, summarize, m = mean(share), error = se(share))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
#or
#d <- ddply(subset(realGame_data, round_num_in_game>1), ~participant.code+normalized_round_payoff_before_splitting, summarize, m = mean(share), error = se(share))
#d <- ddply(d, ~normalized_round_payoff_before_splitting, summarize, m = mean(m), error_share = se(m))

plot_sharing_rate_payoff <- plot_model(m1final, type = "pred", terms = c("normalized_round_payoff_before_splitting[all]"  ))+
  geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(normalized_round_payoff_before_splitting_rounded), y=m , color=Visibility, fill=Visibility),  alpha=0.6)+
  #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
  #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  #geom_line(color=colorsPalette_sharing[1]) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill=colorsPalette_sharing[2], linetype=2, alpha=0.1) +
  xlab("Payoff") +
  ylab("P(share)")+
  ggtitle("")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=14,  family="sans"))+ #, axis.text.x = element_text(size=9.5)
  #scale_x_continuous(trans = 'log2'+)+
  scale_x_continuous(breaks = c(0, 2, 5, 10, 20, 30, 40, 50, 60, 70,  80, 90, 100))+
  coord_trans(x="log2")+
  #scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharingRate_Payoff_quasirandom2.pdf", plot = plot_sharing_rate_payoff, height =6.5, width = 11, units = "in")







###
plot_model(m1final, type = "pred", terms = c("normalized_round_payoff_before_splitting[all]", "Visibility" ))
plot_model(m1final, type = "pred", terms = c("normalized_round_payoff_before_splitting[all]"))
                                             
                                             

plot <- ggdraw() +
  draw_plot(EffectSize_m1final, x = 0, y = 0.5, width = 0.97, height = 0.5) +
  #draw_plot(Effects_size, x = 0, y = 0.5, width = 0.97, height = 0.5) +
  draw_plot(plot_sharing_rate, x = 0, y = 0, width = 0.5, height = .5) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0, width = 0.5, height = .5) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0, 0.5), y = c(1, .5, .5))
ggsave(filename = "Results/Model1.pdf", plot = plot, height =5, width = 7.5, units = "in")

#or

plot <- ggdraw() +
  draw_plot(EffectSize_m1final, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(plot_sharing_rate, x = 0.5, y = 0.5, width = 0.5, height = .5) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0, width = 0.5, height = .5) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0.5, 0.5), y = c(1, 1, .5))
ggsave(filename = "Results/Model1.pdf", plot = plot, height =5, width = 7.5, units = "in")



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

m1final_1 <- glmer(share ~ Visibility + part_factor + round_num_in_game + log_normalized_round_payoff_before_splitting  
              + (1  | participant.code) + (1  | group.id), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
              data = subset(realGame_data, round_num_in_game>1),
              family = binomial(link = "logit"))
summary(m1final_1)

m1final_2 <- glmer(share ~ Visibility + part_factor + round_num_in_game + log_normalized_round_payoff_before_splitting 
              + (1  | participant.code) + (1 + round_num_in_game | group.id), 
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
              data = subset(realGame_data, round_num_in_game>1),
              family = binomial(link = "logit"))
summary(m1final_2)

# m1final_3 <- glmer(share ~ Visibility + part_factor + log_normalized_round_payoff_before_splitting + round_num_in_game
#               + (1 + round_num_in_game | participant.code) + (1  | group.id),
#               control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
#               data = subset(realGame_data, round_num_in_game>1),
#               family = binomial(link = "logit"))
# summary(m1final_3)
# 
# m1final_4 <- glmer(share ~ Visibility + part_factor + log_normalized_round_payoff_before_splitting + round_num_in_game
#             + (1 + round_num_in_game | participant.code)  + (1 + round_num_in_game | group.id),
#             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
#             data = subset(realGame_data, round_num_in_game>1),
#             family = binomial(link = "logit"))
# summary(m1final_4)


anova(m1final_1, m1final_2)
# anova(m1final_1, m1final_3)
# anova(m1final_2, m1final_4)
# anova(m1final_3, m1final_4)



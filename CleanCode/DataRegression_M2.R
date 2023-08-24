#house cleaning
rm(list=ls())

#load packages
packages <- c('plyr',  'dplyr', 'lme4', 'lmerTest', 'MuMIn', 'effects', 'ggplot2', 'sjPlot', 'dotwhisker', 'stargazer', 'ggbeeswarm', 'cowplot')
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




m2 <- lmer(log(normalized_round_payoff_before_splitting) ~ Visibility
           + round_num_in_game
           + part_factor 
           + (1 | participant.code)
           + (1 + round_num_in_game | group.id)
           , #-1,
           control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
           REML = FALSE,
           data = subset(realGame_data,  share == 1 ))
summary(m2)

plot_model(m2, type = "pred")


class(m2) <- "lmerMod"
stargazer(m2, type = 'latex', star.cutoffs = c(0.05, 0.01, 0.001),notes.append = T )



d <- ddply(subset(realGame_data,  share == 1 ), ~participant.code+visibility_radius, summarize, m = mean(normalized_round_payoff_before_splitting), error = se(normalized_round_payoff_before_splitting))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))


plot_shared_values <- plot_model(m2, type = "pred", terms = c("Visibility" ), dot.size=0.75)+
  geom_quasirandom(data = d, inherit.aes = FALSE, aes(x = as.numeric(Visibility), y=m, color=Visibility, fill=Visibility), alpha=0.6)+
  #geom_signif(data=d, inherit.aes = FALSE, aes(x = Visibility, y=m), test =  "wilcox.test", test.args=list(alternative = "two.sided", var.equal = FALSE, paired=TRUE),
  #            comparisons = list(c("Yes", "No")), map_signif_level=TRUE, textsize=3)+
  xlab("Visibility") +
  ylab("Shared payoffs")+
  ggtitle("")+
  guides(color=guide_legend(title="Visibility"), fill=guide_legend(title="Visibility")) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12,  family="sans"), legend.position="none")+
  #scale_y_continuous(trans = squash_axis(10, 20, 10))+
  scale_y_continuous(breaks = c(0, 2, 5, 10, 20,  50,  100))+
  coord_trans(y="log2")+
  scale_fill_manual(values=colorsPalette_visibility)+
  scale_color_manual(values=colorsPalette_visibility)
ggsave(filename = "Results/SharedPayoffs_quasirandom.pdf", plot = plot_shared_values, height =5, width = 7.5, units = "in")

# see https://github.com/strengejacke/ggeffects/issues/62 about the warning (Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.)
# "Standard errors aren't printed any longer anyway, so I could remove that part of the warning. According to your question: yes, predicted values and their CIs are on the correct scale (back transformed to the original scale)."





EffectSize_m2 <-
  plot_model(m2,
             color = "grey30",
             vline.color = "grey75", 
             line.size = 0.5,
             dot.size = 1,
             #value.size = 3,
             value.offset = .3,
             show.values=TRUE, show.p=TRUE,
             transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
             axis.title = "Estimates",# (Log-Odds)
             title = "Log(Shared payoffs)",
             axis.labels = c( "Part 2" , "Round" ,"Visibility"),
             axis.lim= c(-1, 1),
             wrap.labels = 30
  ) + 
  ylim(-.1, .2) +
  theme_classic()+ 
  #theme_sjplot()+
  theme(strip.background=element_blank(), text = element_text(size=11, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 12, face="bold"))
ggsave(filename = "Results/Regressions/Effects_size_M2.pdf", plot = EffectSize_m2, height =4, width = 6, units = "in")



plot <- ggdraw() +
  draw_plot(EffectSize_m2, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(plot_shared_values, x = 0.5, y = 0, width = 0.5, height =1) +
  draw_plot_label(label = c("a", "b"), size = 12,
                  x = c(0, 0.5), y = c(1, 1))
ggsave(filename = "Results/Model2.pdf", plot = plot, height =3, width = 7.5, units = "in")

#or

plot <- ggdraw() +
  draw_plot(EffectSize_m1final, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(plot_sharing_rate, x = 0.5, y = 0.5, width = 0.5, height = .5) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0, width = 0.5, height = .5) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0.5, 0.5), y = c(1, 1, .5))
ggsave(filename = "Results/Model1.pdf", plot = plot, height =5, width = 7.5, units = "in")


Effects_size <- plot_models( m2, m1final, 
                            show.p = TRUE,
                            show.values = TRUE,
                            #p.shape = TRUE,
                            vline.color = "grey75", 
                            line.size = 0.5,
                            dot.size = 1,
                            wrap.labels = 48,
                            grid = TRUE,
                            transform = NULL, # "plogis": probabilities, or NULL: Log-Odds, or remove the lien: Odds Ratios
                            axis.title = "Estimates",# (Log-Odds)
                            #axis.lim = c(-2,4),
                            axis.labels=c( "Log(Payoff)",  "Part 2",  "Round" , "Visibility" ),
                            m.labels = c(  "Log(Shared payoff)\n(Model 2)", "Sharing decicion\n(Model 1)"))+
  facet_grid(~group, scales = "free_x") + 
  #scale_y_continuous(limits = c(NA, NA), expand = c(0.5, 0)) +  
  #scale_color_sjplot("simply")+
  scale_color_sjplot(palette = "reefs")+
  #scale_color_manual(values=c("red", "blue"))+
  #ylim(-.4, .4) +
  theme_classic()+
  theme(strip.background=element_blank(), text = element_text(size=12, face="bold"), axis.text.y = element_text(size = 12, face="plain"), axis.title.x = element_text( size = 10, face="bold"), legend.position = "none") #, legend.position = c(.9, .5)
ggsave(filename = "Results/Regressions/Effects_size_M1-2.pdf", plot = Effects_size, height =5, width = 7.5, units = "in")



#### with plot_sharing_rate_payoff also:

plot <- ggdraw() +
  draw_plot(EffectSize_m1final, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(plot_sharing_rate, x = 0.5, y = 0.75, width = 0.5, height = .25) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0.5, width = 0.5, height = .25) +
  draw_plot(EffectSize_m2, x = 0.03, y = 0, width = 0.47, height = 0.5) +
  draw_plot(plot_shared_values, x = 0.515, y = 0, width = 0.485, height =0.5) +
  draw_plot_label(label = c("a", "b", "c", "d", "e"), size = 12,
                  x = c(0, 0.5, 0.5,  0 ,  0.5), y = c(1, 1, .75,  0.5, 0.5))
ggsave(filename = "Results/Model1-2.pdf", plot = plot, height =6.5, width = 7.5, units = "in")


plot <- ggdraw() +
  draw_plot(EffectSize_m1final, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(plot_sharing_rate, x = 0.5, y = 0.75, width = 0.5, height = .25) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0.5, width = 0.5, height = .25) +
  draw_plot(EffectSize_m2, x = 0.03, y = 0, width = 0.47, height = 0.5) +
  draw_plot(plot_shared_values, x = 0.515, y = 0, width = 0.485, height =0.5) +
  draw_plot_label(label = c("a", "b", "c", "d", "e"), size = 12,
                  x = c(0, 0.5, 0.5,  0 ,  0.5), y = c(1, 1, .75,  0.5, 0.5))
ggsave(filename = "Results/Model1-2.pdf", plot = plot, height =6.5, width = 7.5, units = "in")




plot <- ggdraw() +
  draw_plot(Effects_size, x = 0, y = 0, width = 0.47, height = 0.95) +
  draw_plot(plot_sharing_rate, x = 0.5, y = 0.66, width = 0.5, height = .33) +
  draw_plot(plot_sharing_rate_payoff, x = 0.5, y = 0.33, width = 0.5, height = .33) +
  draw_plot(plot_shared_values, x = 0.515, y = 0, width = 0.485, height =0.33) +
  draw_plot_label(label = c("a", "b", "c", "d"), size = 12,
                  x = c(0, 0.5, 0.5,  0.5 ), y = c(1, 1, .66,  0.33))
ggsave(filename = "Results/Model1-2_V2.pdf", plot = plot, height =6.5, width = 8, units = "in")



plot <- ggdraw() +
  draw_plot(Effects_size, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot(plot_sharing_rate, x = 0, y = 0., width = 0.33, height = .5) +
  draw_plot(plot_sharing_rate_payoff, x = 0.33, y = 0, width = 0.33, height = .5) +
  draw_plot(plot_shared_values, x = 0.66, y = 0, width = 0.33, height =0.5) +
  draw_plot_label(label = c("a", "b", "c", "d"), size = 12,
                  x = c(0, 0, 0.33,  0.66 ), y = c(1, 0.5, 0.5, 0.5))
ggsave(filename = "Results/Model1-2_V3.pdf", plot = plot, height =7.25, width = 8, units = "in")



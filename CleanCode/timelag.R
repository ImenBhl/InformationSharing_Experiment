
rm(list=ls()) #house keeping


#load packages
packages <- c('ggplot2','lmtest','tidyverse', "DescTools")
invisible(lapply(packages, require, character.only = TRUE))


#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))


realGame_data$Visibility <- factor(realGame_data$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- realGame_data$visibility_radius/2
realGame_data$part_id <- ifelse(realGame_data$part=="Treatment 1", 1, 2)
realGame_data$part_factor <- as.factor(realGame_data$part)
realGame_data$part_factor <- relevel(realGame_data$part_factor, ref = "Treatment 1")
#realGame_data$treatments_order_dummy <- ifelse( realGame_data$treatments_order == "No visibility first", 1, 0)
realGame_data$log_normalized_round_payoff <- log(realGame_data$normalized_round_payoff)
realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$normalized_round_payoff_before_splitting_rounded <- round(realGame_data$normalized_round_payoff_before_splitting , 2)
realGame_data$log_normalized_round_payoff_before_splitting <- log(realGame_data$normalized_round_payoff_before_splitting_rounded)


keep <- c( "participant.code", "round_num_in_game", "game_in_treatment",  "Visibility", "part_id", "share", "normalized_round_payoff",  "normalized_round_payoff_before_splitting")
df = subset(realGame_data, select = keep )

df <- df %>% 
  rename(
    score = normalized_round_payoff ,
    payoff = normalized_round_payoff_before_splitting
  )



#Utility function
lag_func <- function(x, k = 1, pad = NA){
  if(k == 0)
    return(x)
  nas <- rep(pad, min(length(x), abs(k)))
  if(k < 0)
    c(tail(x, k), nas) else c(nas, head(x, -k))
}

##############################################################################################


offsets <- seq(-5,5)
timelagDF <- data.frame()

# for (s in sessionList){ #loop through data
#   for (p in players){
#     for (r in rounds){
#       for (rewardOffset in offsets){
#       
#         timelagDF <- rbind(timelagDF, data.frame(round = r, session = s, offset = rewardOffset,
#                            sharingPayoff = cor.test(lag_func(df$sharing, rewardOffset),df$payoff)$estimate,
#                            payoff = cor.test(lag_func(df$payoff, rewardOffset),df$payoff)$estimate,
#                            sharing = cor.test(lag_func(df$sharing, rewardOffset),df$sharing)$estimate))
#             }
#         }
#       }
# }




participants <- unique(df$participant.code)
parts <- unique(df$part_id)
games  <- unique(df$game_in_treatment)
rounds <- unique(df$round_num_in_game)


for (p in participants){ #loop through data
  for (part in parts){
    for (g in games){
      for (offset in offsets){
        d <- subset(df, participant.code==p & part_id==part & game_in_treatment==g)
        timelagDF <- rbind(timelagDF, data.frame(visibility = d$Visibility , participant = p, part = part, game=g, offset = offset,
                                               sharingPayoff = cor.test(lag_func(d$share, offset),d$payoff)$estimate,
                                               sharingScore = cor.test(lag_func(d$share, offset),d$score)$estimate,
                                               payoff = cor.test(lag_func(d$payoff, offset),d$payoff)$estimate,
                                               score = cor.test(lag_func(d$score, offset),d$score)$estimate,
                                               sharing = cor.test(lag_func(d$share, offset),d$share)$estimate))
      }
    }
  }
}



timelagDF$sharingPayoff.z <- FisherZ(timelagDF$sharingPayoff)
timelagDF$sharingScore.z <- FisherZ(timelagDF$sharingScore)
timelagDF$payoff.z <- FisherZ(timelagDF$payoff)
timelagDF$score.z <- FisherZ(timelagDF$score)
timelagDF$sharing.z <- FisherZ(timelagDF$sharing)


saveRDS(timelagDF, 'Results/timeLag/timelagDF.Rds')

##############################################################################################

timelagDF <- readRDS('Results/timeLag/timelagDF.Rds')
# Plot
DF <- data.frame()
#plotDF <- groupDynamicsDF%>% pivot_longer(c(sharingPayoff.z, sharingScore.z, payoff.z, score.z, sharing.z), names_to = 'comparison')
plotDF <- DF%>% pivot_longer(c(sharingPayoff.z, sharingScore.z), names_to = 'comparison')


keep <- c( "visibility","participant","part","game","offset","sharingPayoff","sharingScore","payoff","score","sharing","sharingPayoff.z")
d1 <- subset(timelagDF, select = keep)
d1$comparison <- 'sharingPayoff.z'
d1 <- d1 %>% rename( value = sharingPayoff.z)

keep <- c( "visibility","participant","part","game","offset","sharingPayoff","sharingScore","payoff","score","sharing","sharingScore.z")
d2 <- subset(timelagDF, select = keep)
d2$comparison <- 'sharingScore.z'
d2 <- d2 %>% rename( value = sharingScore.z)

plotDF <- rbind(d1, d2)


plotDF <- plotDF %>% group_by(visibility, offset, comparison) %>%  ## group_by visibility and part_factor
  dplyr::summarize(zscore = mean(value, na.rm=TRUE), ssd = sd(value, na.rm=TRUE), count = n()) %>%
  mutate(se = ssd/sqrt(count), 
         lower_ci = zscore - (qnorm(0.975)*se),
         upper_ci = zscore + (qnorm(0.975)*se))       

plotDF$comparison <- factor(plotDF$comparison, levels = c('sharingPayoff.z', 'sharingScore.z'),
                            labels = c('Sharing ~ Payoff',  'Sharing ~ Score'))


#Use colorblindfriendly palette for discrete data
cbbPalette <- c( "#0072B2", "#CC79A7","#E69F00", "#D55E00", "#56B4E9", "#009E73", "#F0E442",    "#000000")

p_timeLag_zScore<- ggplot(plotDF, aes(x = offset, y = zscore, color = visibility))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = visibility), alpha = 0.3, color = NA)+
  geom_line()+
  scale_x_continuous(breaks=seq(-5,5,1))+
  facet_wrap(~comparison, scales='free_y')+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('z-score')+
  xlab('Offset (s)')+
  scale_fill_manual(values =  cbbPalette, name = 'Visibility')+
  scale_color_manual(values =  cbbPalette, name = 'Visibility')+
  theme( strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

p_timeLag_zScore
ggsave(filename = "Results/timeLag/p_timeLag_zScore.pdf", plot = p_timeLag_zScore, height =8, width = 11, units = "in")


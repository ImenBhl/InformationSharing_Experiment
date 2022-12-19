
rm(list=ls()) #house keeping


#load packages
packages <- c('ggplot2','lmtest','tidyverse')
invisible(lapply(packages, require, character.only = TRUE))



#Utility function
lag_func <- function(x, k = 1, pad = NA){
  if(k == 0)
    return(x)
  nas <- rep(pad, min(length(x), abs(k)))
  if(k < 0)
    c(tail(x, k), nas) else c(nas, head(x, -k))
}

##############################################################################################


offsets <- seq(-10,10)
timelagDF <- data.frame()

for (s in sessionList){ #loop through data
  for (p in players){
    for (r in rounds){
      for (rewardOffset in offsets){
      
        timelagDF <- rbind(timelagDF, data.frame(round = r, session = s, offset = rewardOffset,
                           sharingPayoff = cor.test(lag_func(df$sharing, rewardOffset),df$payoff)$estimate,
                           payoff = cor.test(lag_func(df$payoff, rewardOffset),df$payoff)$estimate,
                           sharing = cor.test(lag_func(df$sharing, rewardOffset),df$sharing)$estimate))
            }
        }
      }
     }
timelagDF$sharingPayoff.z <- FisherZ(timelagDF$sharingPayoff)
timelagDF$payoff.z <- FisherZ(timelagDF$payoff)
timelagDF$sharing.z <- FisherZ(timelagDF$sharing)


saveRDS(timelagDF, 'data/timelagDF.Rds')

##############################################################################################

timelagDF <- readRDS('data/timelagDF.Rds')
# Plot
plotDF <- groupDynamicsDF%>% pivot_longer(c(visSpatial.z, visReward.z, visForaging.z,spatialReward.z, spatialForaging.z, foragingReward.z), names_to = 'comparison')


plotDF <- plotDF %>% group_by(env, offset, comparison) %>% 
  dplyr::summarize(zscore = mean(value, na.rm=TRUE), ssd = sd(value, na.rm=TRUE), count = n()) %>%
  mutate(se = ssd/sqrt(count), 
         lower_ci = zscore - (qnorm(0.975)*se),
         upper_ci = zscore + (qnorm(0.975)*se))       

plotDF$comparison <- factor(plotDF$comparison, levels = c('visSpatial.z', 'visReward.z', 'visForaging.z', 'spatialReward.z', 'spatialForaging.z','foragingReward.z' ),
                            labels = c('Vis ~ Spatial', 'Vis ~ Reward', 'Vis ~ Foraging', 'Spatial ~ Reward', 'Spatial ~ Foraging', 'Foraging ~ Reward'))


pGroupDynamics <- ggplot(plotDF, aes(x = offset, y = zscore, color = env))+
  geom_hline(yintercept = 0,  color = 'black')+
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black')+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = env), alpha = 0.3, color = NA)+
  geom_line()+
  #facet_grid(~comparison)+
  facet_wrap(~comparison, scales='free_y')+
  #ggh4x::facet_grid2(comparison ~. , scales = "free_y", independent = "y")+
  theme_classic()+
  ylab('z-score')+
  xlab('Offset (s)')+
  scale_fill_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  scale_color_manual(values =  c("#E69F00","#009E73"), name = 'Environment')+
  theme(legend.position=c(0,1),legend.justification=c(0,1), strip.background=element_blank(), legend.background=element_blank(),legend.key=element_blank())

pGroupDynamics

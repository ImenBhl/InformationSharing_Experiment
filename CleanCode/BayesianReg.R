#Bayesian regression results
rm(list=ls()) #house keeping

#load packages
packages <- c('tidyverse', 'brms', 'sjPlot', 'ggdist', 'tidybayes')
invisible(lapply(packages, require, character.only = TRUE))


####################################
#Load data
####################################

#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))




realGame_data$Visibility <- factor(realGame_data$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))
realGame_data$Visibility <- relevel(realGame_data$Visibility, ref = "No")
realGame_data$visibility_radius_dummy <- realGame_data$visibility_radius/2

realGame_data$part_factor <- as.factor(realGame_data$part)
realGame_data$part_factor <- relevel(realGame_data$part_factor, ref = "Part 1")

realGame_data$normalized_round_payoff_before_splitting <- realGame_data$normalized_round_payoff*realGame_data$num_splitters
realGame_data$normalized_round_payoff_before_splitting_rounded <- round(realGame_data$normalized_round_payoff_before_splitting , 2)
realGame_data$log_normalized_round_payoff_before_splitting <- log(realGame_data$normalized_round_payoff_before_splitting_rounded)

df <- subset(realGame_data, round_num_in_game>1)


########################################################
#BRMS wrapper
########################################################
#Wrapper for brm models such that it saves the full model the first time it is run, otherwise it loads it from disk
run_model <- function(expr, modelName, path='Results/brmsModels/', reuse = TRUE) {
  path <- paste0(path,'/', modelName, ".brm")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

####################################
#Load data
####################################
#Run model
# mVis <- run_model(brm(share~Visibility + part_factor + log_normalized_round_payoff_before_splitting 
#                       +log_normalized_round_payoff_before_splitting*Visibility 
#                       + (1 + game_in_treatment + round_num_in_game + Visibility + part_factor + log_normalized_round_payoff_before_splitting | participant.code)
#                       #+ (1 + round_num_in_game + game_in_treatment | participant.code)
#                       + (1 + round_num_in_game | group.id) ,
#                       data = df,family = bernoulli(link = "logit"),
#                       cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visSharing_allRandomEffects')

memory.limit(size = 150000)

mVis <- run_model(brm(share~Visibility * part_factor * round_num_in_game  *log_normalized_round_payoff_before_splitting 
                      + (1 + Visibility + part_factor + round_num_in_game + log_normalized_round_payoff_before_splitting | participant.code/group.id),
                      data = df,family = bernoulli(link = "logit"),
                      cores=4,  iter = 6000, warmup = 1000,  backend = 'rstan'), modelName = 'visSharing_6000iter')

logOdds <- mVis %>% gather_draws(b_Intercept, b_VisibilityYes, b_part_factorPart2, b_log_normalized_round_payoff_before_splitting ) %>% median_hdi()
print(exp(logOdds[c(".value",".lower",".upper")])) #Odds ratio


sequence_payoffs <- round(100/(seq(1,66,1)),2)

#Now generate posterior predictions from model
newdat <-expand.grid(Visibility = c('Yes', 'No'), part_factor = c("Part 1", "Part 2"), log_normalized_round_payoff_before_splitting = log(sequence_payoffs), round_num_in_game=seq(2,15,1))
preds <- fitted(mVis, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
newdat$share = preds[,1]
newdat$lower= preds[,3]
newdat$upper = preds[,4]

#levels(newdat$part_factor)[levels(newdat$part_factor)=="Treatment 1"] <- "Part 1"
#levels(newdat$part_factor)[levels(newdat$part_factor)=="Treatment 2"] <- "Part 2"

#levels(df$part_factor)[levels(df$part_factor)=="Treatment 1"] <- "Part 1"
#levels(df$part_factor)[levels(df$part_factor)=="Treatment 2"] <- "Part 2"

#To have plottable raw data
df$payoffBreaks<- cut(df$log_normalized_round_payoff_before_splitting, breaks=log(sequence_payoffs))
df$payoffBreaks <- as.numeric(df$payoffBreaks) # ???

pSharingVis <- ggplot(df, aes(log_normalized_round_payoff_before_splitting, share, color = Visibility, fill=Visibility))+
  stat_summary(data = subset(df, !is.na(payoffBreaks) ), aes(x = log_normalized_round_payoff_before_splitting, y = as.numeric(share),  color = Visibility), fun.y=mean, geom='point', alpha = 0.8)+  ## was: x = payoffBreaks
  geom_ribbon(data = newdat, aes(ymin=lower, ymax = upper, color = Visibility, fill=Visibility), alpha = 0.2)+
  geom_line(data=newdat, aes(color = Visibility))+  ## add aes(x=share)
  facet_grid(round_num_in_game~part_factor)+
  theme_classic()+
  xlab('Log(Payoff)')+
  ylab('P(sharing)')
  #annotate("text", x = 50, y = 0.9, label = "OR: 1.13, 95% HPD: [1.12,1.14]")
pSharingVis
ggsave(filename = "Results/brmsModels/pSharingVis.pdf", plot = pSharingVis, height =8, width = 11, units = "in")


#coefficient plot
#use "axis.labels =c('smooth:group', 'group', 'smooth')" within plot_model to rename y-axis labels
pCoeff_sharing <- plot_model(mVis,  bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  #ylim(c(0.5, 1.5))+ #set this to tighten up the plot
  ggtitle('Sharing decision')
 

pCoeff_sharing
ggsave(filename = "Results/brmsModels/pCoeff_sharing.pdf", plot = pCoeff_sharing, height =8, width = 14, units = "in")


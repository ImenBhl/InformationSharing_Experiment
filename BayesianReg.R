#Bayesian regression results
rm(list=ls()) #house keeping

#load packages
packages <- c('tidyverse', 'brms', 'sjPlot')
invisible(lapply(packages, require, character.only = TRUE))


####################################
#Load data
####################################


########################################################
#BRMS wrapper
########################################################
#Wrapper for brm models such that it saves the full model the first time it is run, otherwise it loads it from disk
run_model <- function(expr, modelName, path='brmsModels/', reuse = TRUE) {
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
mVis <- run_model(brm(sharingDecision~visibility*payoff + (1+visibility+payoff+game+round|id/session) ,
                      data = df,family = bernoulli(link = "logit"),
                      cores=4,  iter = 4000, warmup = 1000,  backend = 'rstan'), modelName = 'visSharing')

logOdds <- mVis %>% gather_draws(b_Intercept, visibility ) %>% median_hdi()
print(exp(logOdds[c(".value",".lower",".upper")])) #Odds ratio


#Now generate posterior predictions from model
newdat <-expand.grid(visibility = c(TRUE, FALSE), payoff = seq(minScore,maxScore, length.out=100))
preds <- fitted(mVis, re_formula = NA, newdata = newdat, probs = c(0.025, 0.975))
fixedDF$sharingDecision = preds[,1]
fixedDF$lower= preds[,3]
fixedDF$upper = preds[,4]

#To have plottable raw data
df$payoffBreaks<- cut(df$payoff, breaks=seq(minScore,maxScore, length.out=100))
df$payoffBreaks <- as.numeric(df$payoffBreaks)

pSharingVis <- ggplot(df, aes(payoff, sharingDecision, color = visibility))+
  stat_summary(data = subset(df, !is.na(payoffBreaks)), aes(x = payoffBreaks, y = as.numeric(sharingDecision)), fun.y=mean, geom='point', color = "#377eb8", alpha = 0.8)+
  geom_ribbon(data = fixedDF, aes(ymin=lower, ymax = upper), color = NA, alpha = 0.4, fill = '#e41a1c')+
  geom_line(data=fixedDF, color = '#e41a1c')+
  theme_classic()+
  xlab('Payoff')+
  ylab('P(sharing)')
  #annotate("text", x = 50, y = 0.9, label = "OR: 1.13, 95% HPD: [1.12,1.14]")
pSharingVis

#coefficient plot
#use "axis.labels =c('smooth:group', 'group', 'smooth')" within plot_model to rename y-axis labels
pCoeff_sharing <- plot_model(mVis, bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
  theme_classic()+
  xlab('')+
  ylab('Estimates')+
  #ylim(c(-.25, .6))+ #set this to tighten up the plot
  ggtitle('Reward')
 

pCoeff_sharing

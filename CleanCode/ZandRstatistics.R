#house cleaning
rm(list=ls())

#load packages
packages <- c('plyr',  'dplyr')
#lapply(packages, install.packages, character.only=TRUE)
lapply(packages, require, character.only=TRUE)

#Now let's make learning curves
se <- function(x){
  return (sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}


#load data
realGame_data<-get(load("CleanCode/Data/RealGameData.Rdata"))




d <- ddply(realGame_data, ~participant.code+visibility_radius+treatments_order+part, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
d$Visibility <- factor(d$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

dt <- ddply(realGame_data, ~visibility_radius+round_num_in_game, summarize, m = mean(normalized_round_payoff), error = se(normalized_round_payoff))
dt$Visibility <- factor(dt$visibility_radius,levels = c(0,2),labels = c("No", "Yes"))

order1 <- subset(d, treatments_order=="No visibility first")
order2 <- subset(d, treatments_order=="Visibility first")
order1_Last2games <- subset(d, treatments_order=="No visibility first" & games =="Last 2 games")


x <- subset(d, visibility_radius==2 )$m
y <- subset(d, visibility_radius==0 )$m
#or
x <- subset(d, visibility_radius==2 & treatments_order=="No visibility first")$m
y <- subset(d, visibility_radius==0 & treatments_order=="No visibility first")$m
#or
x <- subset(d, visibility_radius==2 & treatments_order=="Visibility first")$m
y <- subset(d, visibility_radius==0 & treatments_order=="Visibility first")$m
#or
x <- subset(d, share_factor=="Yes" )$m
y <- subset(d, share_factor=="No" )$m
#or 
x <- subset(d, share_factor=="Yes" & visibility_radius==2 & treatments_order=="No visibility first")$m
y <- subset(d, share_factor=="No" & visibility_radius==2 & treatments_order=="No visibility first")$m
#or
x <- subset(d, visibility_radius==2 & treatments_order=="No visibility first" & games =="Last 2 games")$m
y <- subset(d, visibility_radius==0 & treatments_order=="No visibility first" & games =="Last 2 games")$m
#or best configuration
best_config <- subset(d, visibility_radius==2 & treatments_order=="No visibility first" )



### non parametric ###

Wilcoxon <- wilcox.test(x , y, exact=FALSE, alternative = "two.sided", paired=TRUE)
Wilcoxon <- wilcox.test(x , y, exact=FALSE, alternative = "two.sided", paired=FALSE)
Wilcoxon
#if paired = TRUE
Z <- qnorm(Wilcoxon$p.value/2)
#if paired = FALSE
Z <- qnorm(Wilcoxon$p.value) 
Z
N <- length(x) + length(y)
r = abs(Z)/sqrt(N)
r


### parametric ###

t.test(x , y, exact=FALSE, alternative = "two.sided", paired=TRUE)

t.test(x , y, exact=FALSE, alternative = "two.sided", paired=FALSE)

cohen_d <- (mean(x)-mean(y))/sd(d$m)
cohen_d

cohen_d <- (mean(x)-mean(y))/sd(order1$m)
cohen_d

cohen_d <- (mean(x)-mean(y))/sd(order2$m)
cohen_d

cohen_d <- (mean(x)-mean(y))/sd(order1_Last2games$m)
cohen_d

cohen_d <- (mean(x)-mean(y))/sd(best_config$m)
cohen_d

#house cleaning
rm(list=ls())


#load packages
packages <- c('plyr', 'dplyr', 'readxl')
lapply(packages, require, character.only=TRUE)


#load data
data<-get(load("CleanCode/Data/Data.Rdata"))
#subset with the real game only (removing the training period)
realGame_data <- subset(data, subsession.round_number>12)

#order data
realGame_data <- realGame_data[with(realGame_data, order(realGame_data$participant.code, realGame_data$round_num)), ]
#utils::View(realGame_data)


#Parameters values (fixed)
num_games = 8
num_games_per_treatment = 4
num_rounds_per_game = 15
num_training_rounds = 12

#Create some variables: number of the game in the treatment (1 to 4), round number in the treatment, and whether it is the first or the second treatment to be player (Part)
realGame_data$game_in_treatment <- ifelse(realGame_data$game_num<=num_games_per_treatment, realGame_data$game_num, realGame_data$game_num-num_games_per_treatment)
realGame_data$round_num_in_game <- (realGame_data$round_num-num_training_rounds) %% num_rounds_per_game
realGame_data$round_num_in_game <- ifelse(realGame_data$round_num_in_game==0, num_rounds_per_game, realGame_data$round_num_in_game)
realGame_data$part <- ifelse(realGame_data$game_num<=4, "Part 1", "Part 2")

#Group Id
realGame_data$group.id <- paste0(as.character(realGame_data$group.id_in_subsession),"_",as.character(realGame_data$game_in_treatment),"_", as.character(realGame_data$session.code))


######################
###lagging variables
#####################s

#Share decision in the two previous rounds and Copy decisions in the previous round
realGame_data$share_lag1 <- NA
realGame_data$share_lag2 <- NA
realGame_data$copy_lag1 <- NA
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]==2){
    realGame_data$share_lag1[r] <- realGame_data$share[r-1]
    realGame_data$copy_lag1[r] <- realGame_data$share[r-1] 
  }else if (realGame_data$round_num_in_game[r]>2){
    realGame_data$share_lag1[r] <- realGame_data$share[r-1]
    realGame_data$copy_lag1[r] <- realGame_data$share[r-1] 
    realGame_data$share_lag2[r] <- realGame_data$share[r-2]
  }
}
realGame_data$share_lag1 <- factor(realGame_data$share_lag1,levels = c(0,1),labels = c("No", "Yes"))
realGame_data$copy_lag1 <- factor(realGame_data$copy_lag1,levels = c(0,1),labels = c("No", "Yes"))
realGame_data$share_lag2 <- factor(realGame_data$share_lag2,levels = c(0,1),labels = c("No", "Yes"))


#Number of splitters in the previous round
realGame_data$num_splitters_lag1 <- NA
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]>1){
    realGame_data$num_splitters_lag1[r] <- realGame_data$num_splitters[r-1]
  }
}

#round payoff, game payoff
realGame_data$normalized_round_payoff_lag2 <- NA
realGame_data$normalized_game_payoff_lag2 <- NA
realGame_data$normalized_round_payoff_before_splitting_lag2 <- NA
realGame_data$difference_in_round_payoff_lag2 <- NA
realGame_data$difference_in_game_payoff_lag2 <- NA

for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]>2){
    realGame_data$normalized_round_payoff_lag2[r] <- realGame_data$normalized_round_payoff[r-2]
    realGame_data$normalized_round_payoff_before_splitting_lag2[r] <- realGame_data$normalized_round_payoff_before_splitting[r-2]
    realGame_data$normalized_game_payoff_lag2[r] <- realGame_data$normalized_game_payoff[r-2]
    realGame_data$difference_in_round_payoff_lag2[r] <- realGame_data$normalized_round_payoff[r] - realGame_data$normalized_round_payoff_lag2[r]
    realGame_data$difference_in_game_payoff_lag2[r] <- realGame_data$normalized_game_payoff[r] - realGame_data$normalized_game_payoff_lag2[r]
  }
}


######################
###Group variables
#####################

#toal number of sharers in my group
realGame_data$total_num_sharers_in_group <- apply(realGame_data, 1, function(x) sum(x$my_sharing_neighbours!=0))
realGame_data$total_num_sharers_in_group <- ifelse((realGame_data$my_sharing_neighbours=="-1"),ifelse(realGame_data$share==1, 1, 0), realGame_data$total_num_sharers_in_group)

#number of sharers in my group except me
realGame_data$num_sharers_in_group <- apply(realGame_data, 1, function(x) sum(x$my_sharing_neighbours!=x$id_in_group))
realGame_data$num_sharers_in_group <- ifelse((realGame_data$my_sharing_neighbours=="-1"),0, realGame_data$num_sharers_in_group)

#number of neighbours in my visibility radius (without me)
realGame_data$num_neighbours_in_visibility_radius <- apply(realGame_data, 1, function(x) length(x$neighbours_in_my_visibility_radius)-1)
realGame_data$num_neighbours_in_visibility_radius <- factor(realGame_data$num_neighbours_in_visibility_radius,levels = c(0,1,2,3,4,5),labels = c("0","1","2","3","4","5"))

#number of all visibile neighbours (i.e. sharers and/or those in my visibility radius)
realGame_data$num_all_visibile_neighbours <- apply(realGame_data, 1, function(x) length(x$all_visibile_neighbours)-1)
realGame_data$num_all_visibile_neighbours <- factor(realGame_data$num_all_visibile_neighbours,levels = c(0,1,2,3,4,5),labels = c("0","1","2","3","4","5"))
realGame_data$all_visibile_neighbours_without_me <- apply(realGame_data, 1, function(x) x$all_visibile_neighbours[x$all_visibile_neighbours!=x$id_in_group])


#number of sharers in my group in the previous and before previous round
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]==2){
    realGame_data$num_sharers_in_group_lag1[r] <- realGame_data$num_sharers_in_group[r-1]
  }else if (realGame_data$round_num_in_game[r]>2){
    realGame_data$num_sharers_in_group_lag1[r] <- realGame_data$num_sharers_in_group[r-1]
    realGame_data$num_sharers_in_group_lag2[r] <- realGame_data$num_sharers_in_group[r-2]
  }
}
realGame_data$num_sharers_in_group_lag1 <- factor(realGame_data$num_sharers_in_group_lag1,levels = c(0,1,2,3,4,5),labels = c("0","1","2","3","4","5"))
realGame_data$num_sharers_in_group_lag2 <- factor(realGame_data$num_sharers_in_group_lag2,levels = c(0,1,2,3,4,5),labels = c("0","1","2","3","4","5"))
realGame_data$num_sharers_in_group <- factor(realGame_data$num_sharers_in_group,levels = c(0,1,2,3,4,5),labels = c("0","1","2","3","4","5"))


#Sharing condition:
#We classify the sharing decisions distribution at each round into different scenarios (as in our simulations): "All sharers","Free-rider","Free-giver","No sharers", or "Mixed distribution" otherwise.
realGame_data$sharing_condition <- "Mixed distribution"
realGame_data$sharing_condition <- ifelse((realGame_data$share==1 & realGame_data$num_sharers_in_group=="5"), "All sharers", realGame_data$sharing_condition)
realGame_data$sharing_condition <- ifelse((realGame_data$share==0 & realGame_data$num_sharers_in_group=="5"), "Free-rider", realGame_data$sharing_condition)
realGame_data$sharing_condition <- ifelse((realGame_data$share==1 & realGame_data$num_sharers_in_group=="0"), "Free-giver", realGame_data$sharing_condition)
realGame_data$sharing_condition <- ifelse((realGame_data$share==0 & realGame_data$num_sharers_in_group=="0"), "No sharers", realGame_data$sharing_condition)


########################################################################################################################
###Participant's average sharing decision, copying decision, payoff and distance to max in the experiment and per game
#######################################################################################################################

#My average sharing decision and average copying decision in the whole experiment
subdata <- subset(realGame_data, select=c(participant.code, share, copy))
aggdata <- aggregate(subdata, by=list(subdata$participant.code), FUN=mean, na.rm=TRUE)
aggdata <- subset(aggdata, select = -c(participant.code))
aggdata <- plyr::rename(aggdata, c("Group.1"="participant.code", "share"="share_average", "copy"="copy_average"))
realGame_data <- merge(realGame_data,aggdata,by="participant.code")

#My average sharing decision and average copying decision in the game
subdata <- subset(realGame_data, select=c(participant.code, game_num, share, copy))
aggdata <- aggregate(subdata, by=list(subdata$participant.code, subdata$game_num), FUN=mean, na.rm=TRUE)
aggdata <- subset(aggdata, select = -c(participant.code, Group.2))
aggdata <- plyr::rename(aggdata, c("Group.1"="participant.code", "share"="share_average_in_game", "copy"="copy_average_in_game"))
realGame_data <- merge(realGame_data,aggdata,by=c("participant.code","game_num"))

#My average payoff and my average distance to the max in the whole experiment
subdata <- subset(realGame_data, select=c(participant.code, normalized_round_payoff, normalized_game_payoff, distance_to_max))
aggdata <- aggregate(subdata, by=list(subdata$participant.code), FUN=mean, na.rm=TRUE)
aggdata <- subset(aggdata, select = -c(participant.code))
aggdata <- plyr::rename(aggdata, c("Group.1"="participant.code", "normalized_round_payoff"="normalized_round_payoff_average", "normalized_game_payoff"="normalized_game_payoff_average", "distance_to_max"="distance_to_max_average"))
realGame_data <- merge(realGame_data,aggdata,by=c("participant.code"))

#My average payoff and my average distance to the max in the game
subdata <- subset(realGame_data, select=c(participant.code, game_num, normalized_round_payoff, normalized_game_payoff, distance_to_max))
aggdata <- aggregate(subdata, by=list(subdata$participant.code, subdata$game_num), FUN=mean, na.rm=TRUE)
aggdata <- subset(aggdata, select = -c(participant.code, Group.2))
aggdata <- plyr::rename(aggdata, c("Group.1"="participant.code", "normalized_round_payoff"="normalized_round_payoff_average_in_game", "normalized_game_payoff"="normalized_game_payoff_average_in_game", "distance_to_max"="distance_to_max_average_in_game"))
realGame_data <- merge(realGame_data,aggdata,by=c("participant.code","game_num"))


######################################################################################
###My distance to others in my group, to copied player, to best observation, ...
#####################################################################################

#My distance to the other players in my group
realGame_data$min_distance_to_others <- apply(realGame_data, 1, function(x) min(x$distance_to_players[-x$id_in_group]))
realGame_data$max_distance_to_others <- apply(realGame_data, 1, function(x) max(x$distance_to_players[-x$id_in_group]))
realGame_data$mean_distance_to_others <- apply(realGame_data, 1, function(x) mean(as.numeric(x$distance_to_players[-x$id_in_group]), na.rm = TRUE ))

#My distance to the visible players in my group
realGame_data$min_distance_to_visible_others <- apply(realGame_data, 1, function(x) min(x$distance_to_players[as.numeric(x$all_visibile_neighbours_without_me)]))
realGame_data$max_distance_to_visible_others <- apply(realGame_data, 1, function(x) max(x$distance_to_players[as.numeric(x$all_visibile_neighbours_without_me)]))
realGame_data$mean_distance_to_visible_others <- apply(realGame_data, 1, function(x) mean(as.numeric(x$distance_to_players[as.numeric(x$all_visibile_neighbours_without_me)]), na.rm = TRUE))

#Whether I am in the visibility radius of someone
realGame_data$min_distance_to_visible_others_smaller_than_2 <- ifelse(realGame_data$min_distance_to_visible_others<=2, TRUE, FALSE)

#My distance to the best observation so far
realGame_data$distance_to_best_observation <- apply(realGame_data, 1, function(x) max(abs(x$face-as.numeric(x$best_observation[1])) , abs(x$hair-as.numeric(x$best_observation[2])) , abs(x$mouth-as.numeric(x$best_observation[3])) , abs(x$brow-as.numeric(x$best_observation[4])) , abs(x$eye_width-as.numeric(x$best_observation[7])) , abs(x$eye_height-as.numeric(x$best_observation[8]))))

#My distance to the player I copied if I copied someone ("NA" otherwise)
realGame_data$distance_to_copied_player <- apply(realGame_data, 1, function(x) ifelse(x$copied_player==0, NA, (x$distance_to_players[x$copied_player])))

#My distance to my own guess in the previous round
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]==1){
    realGame_data$distance_to_own_previous_guess[r] <- NA
  }else {
    realGame_data$distance_to_own_previous_guess[r] <- max(abs(realGame_data$face[r]-realGame_data$face[r-1]) , abs(realGame_data$hair[r]-realGame_data$hair[r-1]) , abs(realGame_data$brow[r]-realGame_data$brow[r-1]) , abs(realGame_data$mouth[r]-realGame_data$mouth[r-1]) , abs(realGame_data$eye_width[r]-realGame_data$eye_width[r-1]) , abs(realGame_data$eye_height[r]-realGame_data$eye_height[r-1]))
  }
}


realGame_data$num_others_in_distance_of_1 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=1))
realGame_data$num_others_in_distance_of_2 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=2))
realGame_data$num_others_in_distance_of_3 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=3))
realGame_data$num_others_in_distance_of_4 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=4))
realGame_data$num_others_in_distance_of_5 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=5))
realGame_data$num_others_in_distance_of_6 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=6))
realGame_data$num_others_in_distance_of_7 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=7))
realGame_data$num_others_in_distance_of_8 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=8))
realGame_data$num_others_in_distance_of_9 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=9))
realGame_data$num_others_in_distance_of_10 <- apply(realGame_data, 1, function(x) sum((x$distance_to_players[-x$id_in_group])<=10))




######################
###Search strategies 
#####################

#Defining different search strategies: "random exploration", "copy best", "copy best and innovate", "copy others", "copy others and innovate"

threshold = 2  # equal to the visibility radius
realGame_data$search_strategy <- NA
for (r in (1:nrow(realGame_data))) {
  #print (r)
  if (realGame_data$round_num_in_game[r]==1){
    realGame_data$search_strategy[r] <- "random exploration"
  }else if(realGame_data$copy[r]==1){
    if (realGame_data$copy_max[r]==1){
      if (realGame_data$distance_to_best_observation[r]==0){
        realGame_data$search_strategy[r] <- "copy best"
      }else if(realGame_data$distance_to_best_observation[r]<=threshold){
        realGame_data$search_strategy[r] <- "copy best and innovate"
      }else {
        realGame_data$search_strategy[r] <- "copy best and innovate"
      }
    } else if(realGame_data$copied_info[r]=="social"){
      if(realGame_data$min_distance_to_others[r]==0){
        realGame_data$search_strategy[r] <- "copy others"
      }else if(realGame_data$min_distance_to_others[r]<=threshold){
        realGame_data$search_strategy[r] <- "copy others and innovate"
      }else{
        realGame_data$search_strategy[r] <- "copy others and innovate"
      }
    } else if(realGame_data$copied_info[r]=="individual"){
      if(realGame_data$distance_to_own_previous_guess[r]==0){
        realGame_data$search_strategy[r] <- "copy self"
      }else if(realGame_data$distance_to_own_previous_guess[r]<=threshold){
        realGame_data$search_strategy[r] <- "copy self and innovate"
      }else{
        realGame_data$search_strategy[r] <- "copy self and innovate"
      }
    }
  }else{
    if (realGame_data$distance_to_best_observation[r]==0){
      realGame_data$search_strategy[r] <- "copy best"
    }else if(realGame_data$distance_to_best_observation[r]<=threshold){
      realGame_data$search_strategy[r] <- "copy best and innovate"
    }else  if(realGame_data$distance_to_own_previous_guess[r]==0){
      realGame_data$search_strategy[r] <- "copy self"
    }else if(realGame_data$distance_to_own_previous_guess[r]<=threshold){
      realGame_data$search_strategy[r] <- "copy self and innovate"
    }else if(realGame_data$min_distance_to_others[r]==0){
      realGame_data$search_strategy[r] <- "copy others"
    }else if(realGame_data$min_distance_to_others[r]<=threshold){
      realGame_data$search_strategy[r] <- "copy others and innovate"
    }else{
      realGame_data$search_strategy[r] <- "random exploration"
    }
    
  }
}
realGame_data$search_strategy <- factor(realGame_data$search_strategy)


#Categorizing the different search strategies into famimlies: "Local search around best info", "Local search around social info", "Local search around individual info", "Random exploration"

realGame_data$search_strategy_family <- NA
realGame_data$search_strategy_family <- ifelse(realGame_data$search_strategy %in% c("copy best", "copy best and innovate"), "Local search around best info", 
                                               ifelse(realGame_data$search_strategy %in% c("copy others", "copy others and innovate"), "Local search around social info",
                                                      ifelse(realGame_data$search_strategy %in% c("copy self", "copy self and innovate"), "Local search around individual info","Random exploration")))
realGame_data$search_strategy_family <- factor(realGame_data$search_strategy_family)


#Computing the innovation distance after copying ("NA" if it is random exploration)
realGame_data$innovation_distance_after_copying <- ifelse(realGame_data$search_strategy_family=="Local search around best info", realGame_data$distance_to_best_observation, ifelse(realGame_data$search_strategy_family=="Local search around social info", realGame_data$min_distance_to_others,ifelse(realGame_data$search_strategy_family=="Local search around individual info",realGame_data$distance_to_own_previous_guess,NA)))
realGame_data$innovation_distance_after_copying <- as.numeric(realGame_data$innovation_distance_after_copying)


#order data
realGame_data <- realGame_data[with(realGame_data, order(realGame_data$round_num,
                                                         realGame_data$treatment_name, 
                                                         realGame_data$group.id,  
                                                         realGame_data$round_num_in_game, 
                                                         realGame_data$id_in_group)), ]


#Informations of players in my group
for (r in (1:nrow(realGame_data))) {
  #print(r)
  realGame_data$normalized_round_payoff_player1[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r], realGame_data$normalized_round_payoff_player1[r-realGame_data$id_in_group[r]+1])
  realGame_data$normalized_round_payoff_player2[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r+1], realGame_data$normalized_round_payoff_player2[r-realGame_data$id_in_group[r]+1])
  realGame_data$normalized_round_payoff_player3[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r+2], realGame_data$normalized_round_payoff_player3[r-realGame_data$id_in_group[r]+1])
  realGame_data$normalized_round_payoff_player4[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r+3], realGame_data$normalized_round_payoff_player4[r-realGame_data$id_in_group[r]+1])
  realGame_data$normalized_round_payoff_player5[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r+4], realGame_data$normalized_round_payoff_player5[r-realGame_data$id_in_group[r]+1])
  realGame_data$normalized_round_payoff_player6[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$normalized_round_payoff[r+5], realGame_data$normalized_round_payoff_player6[r-realGame_data$id_in_group[r]+1])
  
  realGame_data$num_splitters_player1[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r], realGame_data$num_splitters_player1[r-realGame_data$id_in_group[r]+1])
  realGame_data$num_splitters_player2[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r+1], realGame_data$num_splitters_player2[r-realGame_data$id_in_group[r]+1])
  realGame_data$num_splitters_player3[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r+2], realGame_data$num_splitters_player3[r-realGame_data$id_in_group[r]+1])
  realGame_data$num_splitters_player4[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r+3], realGame_data$num_splitters_player4[r-realGame_data$id_in_group[r]+1])
  realGame_data$num_splitters_player5[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r+4], realGame_data$num_splitters_player5[r-realGame_data$id_in_group[r]+1])
  realGame_data$num_splitters_player6[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$num_splitters[r+5], realGame_data$num_splitters_player6[r-realGame_data$id_in_group[r]+1])
  
  realGame_data$distance_to_max_player1[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r], realGame_data$distance_to_max_player1[r-realGame_data$id_in_group[r]+1])
  realGame_data$distance_to_max_player2[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r+1], realGame_data$distance_to_max_player2[r-realGame_data$id_in_group[r]+1])
  realGame_data$distance_to_max_player3[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r+2], realGame_data$distance_to_max_player3[r-realGame_data$id_in_group[r]+1])
  realGame_data$distance_to_max_player4[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r+3], realGame_data$distance_to_max_player4[r-realGame_data$id_in_group[r]+1])
  realGame_data$distance_to_max_player5[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r+4], realGame_data$distance_to_max_player5[r-realGame_data$id_in_group[r]+1])
  realGame_data$distance_to_max_player6[r] <- ifelse (realGame_data$id_in_group[r]==1, realGame_data$distance_to_max[r+5], realGame_data$distance_to_max_player6[r-realGame_data$id_in_group[r]+1])
}


#Additional information from my visibilibity radius (mean, max)
for (r in (1:nrow(realGame_data))) {
  #print(r)
  if(realGame_data$num_neighbours_in_visibility_radius[r] == 0){
    realGame_data$mean_additional_info_from_my_visibilibity_rad[r] <- NA
    realGame_data$max_additional_info_from_my_visibilibity_rad[r]<- NA
    realGame_data$mean_additional_info_from_my_visibilibity_rad_shared[r] <- NA
    realGame_data$max_additional_info_from_my_visibilibity_rad_shared[r] <- NA
    realGame_data$mean_additional_info_from_my_visibilibity_rad_did_not_share[r] <- NA
    realGame_data$max_additional_info_from_my_visibilibity_rad_did_not_share[r] <- NA
  }else{
    payoffs_of_neighbors_in_my_visibility_radius <- c()
    payoffs_of_neighbors_in_my_visibility_radius_who_shared <- c()
    payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share <- c()
    for (i in (1:length(realGame_data$neighbours_in_my_visibility_radius[r][[1]]))){
      neighbor = realGame_data$neighbours_in_my_visibility_radius[r][[1]][i]
      if(neighbor != realGame_data$id_in_group[r]){
        neighbor_choice <- realGame_data[r, paste("normalized_round_payoff_player", neighbor, sep = "")]
        payoffs_of_neighbors_in_my_visibility_radius <- append(payoffs_of_neighbors_in_my_visibility_radius, neighbor_choice)   
        if(neighbor %in% realGame_data$my_sharing_neighbours[r][[1]]){
          payoffs_of_neighbors_in_my_visibility_radius_who_shared <- append(payoffs_of_neighbors_in_my_visibility_radius_who_shared, neighbor_choice)   
        }else{
          payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share <- append(payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share, neighbor_choice)   
        }
      }
    }
    realGame_data$mean_additional_info_from_my_visibilibity_rad[r] <- mean(payoffs_of_neighbors_in_my_visibility_radius)
    realGame_data$max_additional_info_from_my_visibilibity_rad[r] <- max(payoffs_of_neighbors_in_my_visibility_radius)
    if (!is.null(payoffs_of_neighbors_in_my_visibility_radius_who_shared)){
      realGame_data$mean_additional_info_from_my_visibilibity_rad_shared[r] <- mean(payoffs_of_neighbors_in_my_visibility_radius_who_shared)
      realGame_data$max_additional_info_from_my_visibilibity_rad_shared[r] <- max(payoffs_of_neighbors_in_my_visibility_radius_who_shared)
    }
    if (!is.null(payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share)){
      realGame_data$mean_additional_info_from_my_visibilibity_rad_did_not_share[r] <- mean(payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share)
      realGame_data$max_additional_info_from_my_visibilibity_rad_did_not_share[r] <- max(payoffs_of_neighbors_in_my_visibility_radius_who_did_not_share)
    }
  }
}


#Additional information shared with me (mean, max)
for (r in (1:nrow(realGame_data))) {
  #print(r)
  if(realGame_data$num_sharers_in_group[r] == 0){
    realGame_data$mean_additional_info_shared_with_me[r] <- NA
    realGame_data$max_additional_info_shared_with_me[r]<- NA
  }else{
    payoffs_of_sharers <- c()
    for (i in (1:length(realGame_data$my_sharing_neighbours[r][[1]]))){
      neighbor = realGame_data$my_sharing_neighbours[r][[1]][i]
      if(neighbor != realGame_data$id_in_group[r]){
        neighbor_choice <- realGame_data[r, paste("normalized_round_payoff_player", neighbor, sep = "")]
        payoffs_of_sharers <- append(payoffs_of_sharers, neighbor_choice)   
      }
    }
    realGame_data$mean_additional_info_shared_with_me[r] <- mean(payoffs_of_sharers)
    realGame_data$max_additional_info_shared_with_me[r] <- max(payoffs_of_sharers)
  }
}


realGame_data <- realGame_data[with(realGame_data, order(realGame_data$participant.code, realGame_data$round_num)), ]

#save data
save(realGame_data, file="CleanCode/Data/RealGameData.Rdata")
#utils::View(realGame_data)


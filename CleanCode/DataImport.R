#house cleaning
rm(list=ls())


#load packages
packages <- c('plyr', 'readxl')
lapply(packages, require, character.only=TRUE)



#variables to drop
drop <- c("participant.session", "player.id", "participant.label", "participant._current_page", 
          "participant._is_bot", "participant._index_in_pages" , "participant._max_page_index" ,
          "session.is_demo" , "Group.id_in_subsession" , "participant.name" , 
          "participant._current_app_name", "participant._round_number", 
          "participant._current_page_name", "participant.status", "player.roles_sequence",
          "participant.last_request_succeeded", "participant.ip_address", 
          "participant.visited", "participant.mturk_worker_id", "participant.mturk_assignment_id", 
          "player.role", "player.num_etudiant", "player.nickname", "session.experimenter_name", 
          "session.real_world_currency_per_point", "session.time_scheduled", "session.time_started", 
          "session.mturk_HITId", "session.mturk_HITGroupId", "session.participation_fee", 
          "session.special_category")


colums_types = c("numeric", "text", "text", 
              "numeric", "numeric", "numeric", 
              "text", "text", "text", "text", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "text", "text", "text", "text", "numeric", 
              "text", "text", "text", "numeric", 
              "text", "text", "text", "numeric", 
              "text", "text", "text", "numeric", 
              "text", "text", "text", "numeric", 
              "text", "numeric", "numeric", "text", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "text", "text", 
              "text", "text", "text", "numeric", 
              "text", "numeric", "numeric", "numeric", 
              "text", "numeric", "numeric", "numeric", 
              "numeric", "text", "text", "text", 
              "text", "text", "text", "text", "text", 
              "text", "numeric", "numeric", "numeric", 
              "text", "text", "text", "numeric", 
              "numeric", "text", "numeric")


######  All Data
#import raw data (all sessions)
rawData <- read_excel("Data/All/Clean/information_sharing_formatted.xlsx", 
                      col_types = colums_types)

#clean data (session1)
fullData <- subset(rawData, session.label %in% c('T1_18_players', 'T1_24_players', 'T2_18_players', 'T2_24_players'))
fullData$treatment_group <- ifelse((fullData$session.label %in% c('T1_18_players', 'T1_24_players')),"Group 1","Group 2")
fullData$treatment_group <- factor(fullData$treatment_group) 
fullData$treatments_order <- ifelse(fullData$treatment_group=="Group 1", "Visibility first", "No visibility first")
fullData$treatments_order <- factor(fullData$treatments_order) 
#drop unwanted variables
data <- fullData[,!(names(fullData) %in% drop)]


demographicsData <- read_excel("Data/All/Clean/demographics.xlsx")
demographicsData <- subset(demographicsData, session.label %in% c('T1_18_players', 'T1_24_players', 'T2_18_players', 'T2_24_players'))
demographicsData <- demographicsData[,(names(demographicsData) %in% c('participant.code', 'player.q_studies', 'player.q_age', 'player.q_gender'))]

data <- merge(data,demographicsData,by="participant.code")

#rename some variables
data <- plyr::rename(data, c("player.q_studies"="studies", "player.q_age"="age", "player.q_gender"="gender"))
data <- plyr::rename(data, c("player.payoff"="payoff", 
                             "player.id_in_group"="id_in_group",
                             "player.num_poste"="num_poste",
                             "player.round_num"="round_num",
                             "player.treatment"="treatment",
                             "player.treatments_sequence"="treatments_sequence",
                             "player.treatment_name"="treatment_name",
                             "player.questions_correct"="questions_correct",
                             "player.game_num"="game_num",
                             "player.share"="share",
                             "player.face"="face",
                             "player.hair"="hair",
                             "player.mouth"="mouth",
                             "player.brow"="brow",
                             "player.nose_width"="nose_width",
                             "player.nose_height"="nose_height",
                             "player.eye_width"="eye_width",
                             "player.eye_height"="eye_height",
                             "player.face_maximum"="face_maximum",
                             "player.hair_maximum"="hair_maximum",
                             "player.mouth_maximum"="mouth_maximum",
                             "player.brow_maximum"="brow_maximum",
                             "player.nose_width_maximum"="nose_width_maximum",
                             "player.nose_height_maximum"="nose_height_maximum",
                             "player.eye_width_maximum"="eye_width_maximum",
                             "player.eye_height_maximum"="eye_height_maximum",
                             "player.distance_to_max"="distance_to_max",
                             "player.visibility_radius"="visibility_radius",
                             "player.distance_to_players"="distance_to_players",
                             "player.my_sharing_neighbours"="my_sharing_neighbours",
                             "player.neighbours_in_my_visibility_radius"="neighbours_in_my_visibility_radius",
                             "player.all_visibile_neighbours"="all_visibile_neighbours",
                             "player.best_observation"="best_observation",
                             "player.num_splitters"="num_splitters",
                             "player.copy"="copy",
                             "player.copy_max"="copy_max",
                             "player.copied_player"="copied_player",
                             "player.copied_info"="copied_info",
                             "player.round_payoff"="round_payoff",
                             "player.game_payoff"="game_payoff",
                             "player.max_score"="max_score",
                             "player.currency_rate"="currency_rate",
                             "player.currency_symbol"="currency_symbol"))


#normalize payoff
data$normalized_round_payoff <- data$round_payoff / data$currency_rate
data$normalized_game_payoff <- data$game_payoff / data$currency_rate

#rename treatments 
data$treatment_name <- ifelse(data$treatment==1, "Visibility", "No visibility")
data$treatment_name <- factor(data$treatment_name) 

#share as factor
data$share_factor <- ifelse(data$share==1, "Yes", "No")
data$share_factor <- factor(data$share_factor) 

#copy as factor
data$copy_factor <- ifelse(data$copy==1, "Yes", "No")
data$copy_factor <- factor(data$copy_factor) 

#convert strings to vectors
data$distance_to_players <- strsplit(data$distance_to_players, ";")
data$my_sharing_neighbours <- strsplit(data$my_sharing_neighbours, ";")
data$neighbours_in_my_visibility_radius <- strsplit(data$neighbours_in_my_visibility_radius, ";")
data$all_visibile_neighbours <- strsplit(data$all_visibile_neighbours, ";")
data$best_observation <- strsplit(data$best_observation, ";")

#example
row <- 100
value_index <- 1
as.numeric(data$distance_to_players[[row]])
as.numeric(data$distance_to_players[[row]][[value_index]])

#order data
data <- data[with(data, order(data$session.code, data$round_num, data$group.id_in_subsession, data$id_in_group)), ]

#save data
save(data, file="CleanCode/Data/Data.Rdata")

#load data
#data<-get(load("CleanCode/Data/Data.Rdata"))

#View data
#View(data)

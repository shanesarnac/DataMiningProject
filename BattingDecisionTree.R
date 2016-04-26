# Shane Sarnac
# April 19, 2016
# This file aims to make predictions about batters using decision trees

findCommonPlayers = function(year1, year2) {
  players_year1 = which(batters$yearID == year1)
  players_year2 = which(batters$yearID == year2)
  wanted_players = NULL
  print(length(players_year1))
  print(length(players_year2))
  previous_player = ""
  
  for(player_yr1 in batters$playerID[players_year1]) {
    for(player_yr2 in batters$playerID[players_year2]) {
      if(player_yr1 == player_yr2 & player_yr1 != previous_player) {
        # print(paste("player_yr1 =", player_yr1))
        # print(paste("previous player =", previous_player))
        # print("")
        wanted_players = c(wanted_players, which(batters$playerID == player_yr1 & batters$yearID == year1 & batters$AB > 0 & batters$stint == 1))
        break
      }
    }
    previous_player = player_yr1
  }
  return(batters[wanted_players,])
}

findCommonPlayers = function(year1, year2, data) {
  players_year1 = which(data$yearID == year1)
  players_year2 = which(data$yearID == year2)
  wanted_players = NULL
  print(length(players_year1))
  print(length(players_year2))
  previous_player = ""
  
  for(player_yr1 in data$playerID[players_year1]) {
    for(player_yr2 in data$playerID[players_year2]) {
      if(player_yr1 == player_yr2 & player_yr1 != previous_player) {
        print(paste("player_yr1 =", player_yr1))
        print(paste("previous player =", previous_player))
        print("")
        wanted_players = c(wanted_players, which(data$playerID == player_yr1 & data$yearID == year1 & data$stint == 1))
        break
      }
    }
    previous_player = player_yr1
  }
  return(data[wanted_players,])
}

combinePlayerStatsHitter = function(player) {
  #print(player)
  combined_player = NULL
  combined_player$playerID = NA
  combined_player$playerID = player$playerID[1]
  #print(combined_player$playerID)
  combined_player$yearID = player$yearID[1]
  combined_player$stint = length(player$stint)
  combined_player$teamID = NA
  combined_player$lgID = NA
  combined_player$G = sum(player$G)
  combined_player$AB = sum(player$AB)
  combined_player$R = sum(player$R)
  combined_player$H = sum(player$H)
  combined_player$X2B = sum(player$X2B)
  combined_player$X3B = sum(player$X3B)
  combined_player$HR = sum(player$HR)
  combined_player$RBI = sum(player$RBI)
  combined_player$SB = sum(player$SB)
  combined_player$CS = sum(player$CS)
  combined_player$BB = sum(player$BB)
  combined_player$SO = sum(player$SO)
  combined_player$IBB = sum(player$IBB)
  combined_player$HBP = sum(player$HBP)
  combined_player$SH = sum(player$SH)
  combined_player$SF = sum(player$SF)
  combined_player$GIDP = sum(player$GIDP)
  combined_player$OBP = ifelse(combined_player$AB + combined_player$BB + combined_player$HBP + combined_player$SF == 0, 
                               0, 
                               (combined_player$H + combined_player$BB + combined_player$HBP) / (combined_player$AB + combined_player$BB + combined_player$HBP + combined_player$SF))
  combined_player$BABIP = ifelse(combined_player$AB - combined_player$SO - combined_player$HR + combined_player$SF == 0, 
                                 0, 
                                 (combined_player$H - combined_player$HR)/ (combined_player$AB - combined_player$SO - combined_player$HR + combined_player$SF))
  combined_player$KP = combined_player$SO / combined_player$AB
  combined_player$BBP = combined_player$BB / combined_player$AB
  combined_player$X1B = combined_player$H - combined_player$HR - combined_player$X3B - combined_player$X2B
  combined_player$SLG = (combined_player$X1B + 2*combined_player$X2B + 3*combined_player$X3B + 4*combined_player$HR)/ combined_player$AB
  combined_player$AVE = combined_player$H / combined_player$AB
  combined_player$ISO = combined_player$SLG - combined_player$AVE
  combined_player$OPS = combined_player$OBP + combined_player$SLG
  return (combined_player)
}

standardizeYearHitter = function(wanted_players, year) {
  player_locations = NULL
  standardized_players = NULL
  for (player in wanted_players$playerID) {
    player_locations = which(batters$playerID == player & batters$yearID == year)
    #print(player)
    #print(length(player_locations))
    if(length(player_locations) > 1) {
      temp_holder = NULL
      for (i in player_locations) {
        temp_holder = rbind(temp_holder, batters[i,])
      }
      standardized_players = rbind(standardized_players, combinePlayerStatsHitter(temp_holder))
    }
    else {
      standardized_players = rbind(standardized_players, batters[player_locations,])
    }
  }
  return (standardized_players)
}

# import necessary library for decision trees
library(rpart)

# import for printing decision trees
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# library(rattle)
library(rpart.plot)
#library(RColorBrewer)

# Remember to read in batter data form Batting_Model.R prior to running this file
year1_data <- batters[which(batters$yearID == 2005),]

year1 = 2005
year2 = 2006

# Find all players that played in both year1 and year2; returns all year1 statistics
training_set = findCommonPlayers(year1, year2)
training_set = standardizeYearHitter(training_set, year1)

# Find corresponding players in year 2 and standardize
year2_players = findCommonPlayers(year2, year1)
year2_players = standardizeYearHitter(year2_players, year2)

# append information
training_set$year2OPS = as.numeric(year2_players$OPS)
training_set$year2G = as.numeric(year2_players$G)
training_set$year2AB = as.numeric(year2_players$AB)

tree = rpart(year2OPS ~ effective_age + OPS + G + AB + BABIP + SO + OBP + SLG, data = training_set, method = "anova")

rpart.plot(tree)
printcp(tree)
prp(tree, faclen = 0, extra = 1, main = "Average OPS in Year 2")

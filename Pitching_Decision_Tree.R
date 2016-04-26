# Author: Shane Sarnac
# Date: April 2, 2016
# Summary: This R script aims to create predictions about pitchers using a decision tree

##### Determines the quality of a pitcher's command, returns factors (strings) #####
eval_command = function() {
  command = NULL
  for (i in 1:length(pitchers$playerID)) {
    if (is.na(pitchers$SO_9[i]) | is.na(pitchers$SO_Percentage[i])) {
      command = c(command, "NA")
    }
    else if (pitchers$SO_9[i] > 9.0 & pitchers$SO_Percentage[i] > 0.24) {
      command = c(command,"Excellent")
    }
    else if (pitchers$SO_9[i] > 8.2 & pitchers$SO_Percentage[i] > 0.22) {
      command = c(command, "Great") 
    }
    else if (pitchers$SO_9[i] > 7.7 & pitchers$SO_Percentage[i] > 0.20) {
      command = c(command, "Above Average")
    }
    else if (pitchers$SO_9[i] > 7.0 & pitchers$SO_Percentage[i] > 0.17) {
      command = c(command,"Average")
    }
    else if (pitchers$SO_9[i] > 6.0 & pitchers$SO_Percentage[i] > 0.15) {
      command = c(command, "Below Average")
    }
    else if (pitchers$SO_9[i] > 5.0 & pitchers$SO_Percentage[i] > 0.13) {
      command = c(command, "Poor")
    }
    else {
      command = c(command, "Awful")
    }
  }
 return (command)
}

##### Determines the quality of a pitcher's control, returns factors (strings) #####


##### Standardize Pitchers stat's who got traded #####
combinePlayerStatsPitcher = function(player) {
  combined_player = NULL
  combined_player$playerID = NA
  combined_player$playerID = player$playerID[1]
  combined_player$yearID = player$yearID[1]
  combined_player$stint = length(player$stint)
  combined_player$teamID = NA
  combined_player$lgID = NA
  combined_player$W = sum(player$W)
  combined_player$L = sum(player$L)
  combined_player$G = sum(player$G)
  combined_player$GS = sum(player$GS)
  combined_player$CG = sum(player$CG)
  combined_player$SHO = sum(player$SHO)
  combined_player$SV = sum(player$SV)
  combined_player$IPouts = sum(player$IPouts)
  combined_player$H = sum(player$H)
  combined_player$ER = sum(player$ER)
  combined_player$HR = sum(player$HR)
  combined_player$BB = sum(player$BB)
  combined_player$SO = sum(player$SO)
  combined_player$BAOpp = NA
  combined_player$ERA = 9* combined_player$ER / (combined_player$IPout/3)
  combined_player$IBB = sum(player$IBB)
  combined_player$WP = sum(player$WP)
  combined_player$HBP = sum(player$HBP)
  combined_player$BK = sum(player$BK)
  combined_player$BFP = sum(player$BFP)
  combined_player$GF = sum(player$GF)
  combined_player$R = sum(player$R)
  combined_player$SH = sum(player$SH)
  combined_player$SF = sum(player$SF)
  combined_player$GIDP = sum(player$GIDP)
  combined_player$IP = combined_player$IPouts / 3
  combined_player$SO_9 = combined_player$SO * 9 / combined_player$IP
  combined_player$SO_Percentage = combined_player$SO / combined_player$BFP
  combined_player$BB_9 = combined_player$BB *9 / combined_player$IP
  combined_player$BB_Percentage = combined_player$BB / combined_player$BFP
  combined_player$effective_age = player$effective_age[1]
  combined_player$command = NULL
  if (is.na(combined_player$SO_9[i]) | is.na(combined_player$SO_Percentage[i])) {
    combined_player$command = c(combined_player$command, "NA")
  }
  else if (combined_player$SO_9[i] > 9.0 & combined_player$SO_Percentage[i] > 0.24) {
    combined_player$command = c(combined_player$command,"Excellent")
  }
  else if (combined_player$SO_9[i] > 8.2 & combined_player$SO_Percentage[i] > 0.22) {
    combined_player$command = c(combined_player$command, "Great") 
  }
  else if (combined_player$SO_9[i] > 7.7 & combined_player$SO_Percentage[i] > 0.20) {
    combined_player$command = c(combined_player$command, "Above Average")
  }
  else if (combined_player$SO_9[i] > 7.0 & combined_player$SO_Percentage[i] > 0.17) {
    combined_player$command = c(combined_player$command,"Average")
  }
  else if (combined_player$SO_9[i] > 6.0 & combined_player$SO_Percentage[i] > 0.15) {
    combined_player$command = c(combined_player$command, "Below Average")
  }
  else if (combined_player$SO_9[i] > 5.0 & combined_player$SO_Percentage[i] > 0.13) {
    combined_player$command = c(combined_player$command, "Poor")
  }
  else {
    combined_player$command = c(combined_player$command, "Awful")
  }
  return (as.factor(combined_player))
}

standardizeYearPitcher = function(wanted_players, year) {
  player_locations = NULL
  standardized_players = NULL
  for (player in wanted_players$playerID) {
    player_locations = which(pitchers$playerID == player & pitchers$yearID == year)
    #print(player)
    #print(length(player_locations))
    if(length(player_locations) > 1) {
      temp_holder = NULL
      for (i in player_locations) {
        temp_holder = rbind(temp_holder, pitchers[i,])
      }
      standardized_players = rbind(standardized_players, combinePlayerStatsPitcher(temp_holder))
    }
    else {
      standardized_players = rbind(standardized_players, pitchers[player_locations,])
    }
  }
  return (standardized_players)
}

##### End Standardize Player Stats #####

pitchers = read.csv(file = 'Baseball/Pitching.csv', header = TRUE)

# Add evaluating statistics
pitchers$IP = pitchers$IPouts / 3
pitchers$SO_9 = pitchers$SO * 9 / pitchers$IP
pitchers$SO_Percentage = pitchers$SO / pitchers$BFP
pitchers$BB_9 = pitchers$BB *9 / pitchers$IP
pitchers$BB_Percentage = pitchers$BB / pitchers$BFP
pitcher_indices_in_master = match(pitchers$playerID, master$playerID)
pitchers$effective_age = as.numeric(findSeasonAge(
  pitchers$yearID,
  master$birthYear[pitcher_indices_in_master],
  master$birthMonth[pitcher_indices_in_master]
))

pitchers$command = eval_command()
pitchers$command = as.factor(pitchers$command)

year1 = 2005
year2 = 2006
pitchers_year1 = pitchers[pitchers$yearID == year1,]
pitchers_year2 = pitchers[pitchers$yearID == year2,]

train = findCommonPlayers(year1, year2, pitchers)
train = standardizeYearPitcher(train, year1)

pitchers_year2 = findCommonPlayers(year2, year1, pitchers)
pitchers_year2 = standardizeYearPitcher(pitchers_year2, year2)

train$command_year2 = pitchers_year2$command
train$command_year2 = as.factor(train$command_year2)
train$ERA_year2 = pitchers_year2$ERA
train$IP_year2 = pitchers_year2$IP

tree = rpart(ERA_year2 ~ IP + ERA + SO_Percentage + BB_Percentage + W + SO + effective_age, data = train, method = "anova")
prp(tree, faclen = 0, extra = 1, main = "Year 2 ERA")

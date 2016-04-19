# Shane Sarnac
# April 19, 2016
# This file aims to make predictions about batters using decision trees

findCommonPlayers = function(year1, year2) {
  players_year1 = which(batters$yearID == year1)
  players_year2 = which(batters$yearID == year2)
  wanted_players = NULL
  print(length(players_year1))
  print(length(players_year2))
  
  for(player_yr1 in batters$playerID[players_year1]) {
    for(player_yr2 in batters$playerID[players_year2]) {
      if(player_yr1 == player_yr2) {
        wanted_players = c(wanted_players, which(batters$playerID == player_yr1 & batters$yearID == 2005))
        break
      }
    }
    #print(player_yr1)
  }
  return(batters[wanted_players,])
}

# import necessary library for decision trees
library(rpart)

# Remember to read in batter data form Batting_Model.R prior to running this file
year1_data <- batters[which(batters$yearID == 2005),]

training_set_indicies = findCommonPlayers(2005, 2006)
test <- findCommonPlayers(2005, 2006)



year1 = 2005
year2 = 2006
players_year1 = which(batters$yearID == year1)
players_year2 = which(batters$yearID == year2)

wanted_players = NULL
print(length(players_year1))
print(length(players_year2))

for(player_yr1 in batters$playerID[players_year1]) {
  for(player_yr2 in batters$playerID[players_year2]) {
    if(player_yr1 == player_yr2) {
      wanted_players = c(wanted_players, which(batters$playerID == player_yr1 & batters$yearID == 2005))
      break
    }
  }
  #print(player_yr1)
}
next_year_OPS = NULL
common_players = NULL
for(player in batters$playerID[wanted_players]) {
  common_players = c(common_players, which(which(batters$playerID == player) == which(batters$yearID == 2006)))
}

players_2006 = which(batters$yearID == 2006)




# Collaborators: Shane Sarnac, Ryan Smith, Zachary Collins
# Date: March 11, 2016
# Summary: This code aims to find trends and develop a model to predict hitters' performances. 

findSeasonAge = function(season_year, birth_year, birth_month) {
  effective_birth_year = ifelse(birth_month < 7, birth_year, birth_year - 1) 
  age = season_year - effective_birth_year
  return (age)
}

pitchers = read.csv(file = 'Baseball/Pitching.csv', header = TRUE)
batters = read.csv(file = 'Baseball/Batting.csv', header = TRUE)
teams = read.csv(file = 'Baseball/Teams.csv', header = TRUE)
master = read.csv(file = 'Baseball/Master.csv', header = TRUE)

############################################################################################
# Adding more advanced metrics to data set
# OBP = On Base Percentage
batters$OBP = ifelse(batters$AB + batters$BB + batters$HBP + batters$SF == 0, 
      0, 
     (batters$H + batters$BB + batters$HBP) / (batters$AB + batters$BB + batters$HBP + batters$SF))

# BABIP = Batting Average on Ball In Play
batters$BABIP = ifelse(batters$AB - batters$SO - batters$HR + batters$SF == 0, 
      0, 
      (batters$H - batters$HR)/ (batters$AB - batters$SO - batters$HR + batters$SF))

# KP = Strikeout percentage (K%)
batters$KP = batters$SO / batters$AB

# BBP = Walk percentage (BB %)
batters$BBP = batters$BB / batters$AB

# X1B = Single (needed for calculation of Slugging Percentage (SLG))
batters$X1B = batters$H - batters$HR - batters$X3B - batters$X2B

# SLG = Slugging Percentage
batters$SLG = (batters$X1B + 2*batters$X2B + 3*batters$X3B + 4*batters$HR)/ batters$AB

# AVE = Batting Average
batters$AVE = batters$H / batters$AB

# ISO = Isolated Power
batters$ISO = batters$SLG - batters$AVE

# OPS = On Base Percentage + Slugging Percentage
batters$OPS = batters$OBP + batters$SLG

# Effective Season age
batter_indices_in_master = match(batters$playerID, master$playerID)
batters$Age = findSeasonAge(
  batters$yearID,
  master$birthYear[batter_indices_in_master],
  master$birthMonth[batter_indices_in_master]
)

############################################################################################



# Collaborators: Shane Sarnac, Ryan Smith, Zach Collins
# Date: March 11, 2016
# Summary: This code aims to find trends and develop a model to predict hitters' performances. 

# Read in data from the Pitching, Team, and  Master csv files from Lahmann database
batters = read.csv(file = "Baseball/Batting.csv", header = TRUE)
teams = read.csv(file = 'Baseball/Teams.csv', header = TRUE)
master = read.csv(file = 'Baseball/Master.csv', header = TRUE)


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

# OPS = On base Plus Slugging
batters$OPS = batters$SLG + batters$OBP

AVG = batters$H[batters$AB > 0]/batters$AB[batters$AB > 0]

# Find the age that a player was for the year in question. The adopted convention for this
# project is that if a player's birthday occured before July, the player's season age is his
# birth age. If a player's birthday occured in July or after, the player's season age is one
# year younger than his birth age. 
findSeasonAge = function(season_year, birth_year, birth_month) {
  effective_birth_year = ifelse(birth_month < 7, birth_year, birth_year - 1) 
  age = season_year - effective_birth_year
  return (age)
}

batter_indices_in_master = match(batters$playerID, master$playerID)

# Determine the age of the pitcher during a given season
batters$effective_age = findSeasonAge(
  batters$yearID,
  master$birthYear[batter_indices_in_master],
  master$birthMonth[batter_indices_in_master]
)

minAB = 100

findAverageByYear = function(stat, startYear, endYear){
  total = mean(stat[batters$AB > minAB & batters$yearID == startYear], na.rm = TRUE)
  for (i in (startYear+1):endYear) {
    total = c(total,mean(stat[batters$AB > minAB & batters$yearID == i], na.rm = TRUE))
  }
  return(total)
}

findAverageByAge = function(stat, startAge, endAge){
  total = mean(stat[batters$AB > minAB & batters$effective_age == startAge], na.rm = TRUE)
  for (i in (startAge+1):endAge) {
    total = c(total,mean(stat[batters$AB > minAB & batters$effective_age == i], na.rm = TRUE))
  }
  return(total)
}

plot(x = 1871:2014, y = findAverageByYear(batters$AVE, 1871, 2014), main = 'Batting Average By Year', xlab = 'Year', ylab = 'Average Batting Avg')
plot(x = 1871:2014, y = findAverageByYear(batters$G, 1871, 2014), main = 'Games By Year', xlab = 'Year', ylab = 'Average G')
plot(x = 1871:2014, y = findAverageByYear(batters$AB, 1871, 2014), main = 'AB By Year', xlab = 'Year', ylab = 'Average AB')
plot(x = 1871:2014, y = findAverageByYear(batters$R, 1871, 2014), main = 'R By Year', xlab = 'Year', ylab = 'Average R')
plot(x = 1871:2014, y = findAverageByYear(batters$H, 1871, 2014), main = 'H By Year', xlab = 'Year', ylab = 'Average H')
plot(x = 1871:2014, y = findAverageByYear(batters$X1B, 1871, 2014), main = '1B By Year', xlab = 'Year', ylab = 'Average 1B')
plot(x = 1871:2014, y = findAverageByYear(batters$X2B, 1871, 2014), main = '2B By Year', xlab = 'Year', ylab = 'Average 2B')
plot(x = 1871:2014, y = findAverageByYear(batters$X3B, 1871, 2014), main = '3B By Year', xlab = 'Year', ylab = 'Average 3B')
plot(x = 1871:2014, y = findAverageByYear(batters$HR, 1871, 2014), main = 'HR By Year', xlab = 'Year', ylab = 'Average HR')
plot(x = 1871:2014, y = findAverageByYear(batters$RBI, 1871, 2014), main = 'RBI By Year', xlab = 'Year', ylab = 'Average RBI')
plot(x = 1871:2014, y = findAverageByYear(batters$SB, 1871, 2014), main = 'SB By Year', xlab = 'Year', ylab = 'Average SB')
plot(x = 1871:2014, y = findAverageByYear(batters$CS, 1871, 2014), main = 'CS By Year', xlab = 'Year', ylab = 'Average CS')
plot(x = 1871:2014, y = findAverageByYear(batters$BB, 1871, 2014), main = 'BB By Year', xlab = 'Year', ylab = 'Average BB')
plot(x = 1871:2014, y = findAverageByYear(batters$SO, 1871, 2014), main = 'SO By Year', xlab = 'Year', ylab = 'Average SO')
plot(x = 1871:2014, y = findAverageByYear(batters$IBB, 1871, 2014), main = 'IBB By Year', xlab = 'Year', ylab = 'Average IBB')
plot(x = 1871:2014, y = findAverageByYear(batters$HBP, 1871, 2014), main = 'HBP By Year', xlab = 'Year', ylab = 'Average HBP')
plot(x = 1871:2014, y = findAverageByYear(batters$SH, 1871, 2014), main = 'SH By Year', xlab = 'Year', ylab = 'Average SH')
plot(x = 1871:2014, y = findAverageByYear(batters$SF, 1871, 2014), main = 'SF By Year', xlab = 'Year', ylab = 'Average SF')
plot(x = 1871:2014, y = findAverageByYear(batters$GIDP, 1871, 2014), main = 'GIDP By Year', xlab = 'Year', ylab = 'Average GIDP')
plot(x = 1871:2014, y = findAverageByYear(batters$KP, 1871, 2014), main = 'KP By Year', xlab = 'Year', ylab = 'Average KP')
plot(x = 1871:2014, y = findAverageByYear(batters$BBP, 1871, 2014), main = 'BBP By Year', xlab = 'Year', ylab = 'Average BBP')
plot(x = 1871:2014, y = findAverageByYear(batters$OBP, 1871, 2014), main = 'OBP By Year', xlab = 'Year', ylab = 'Average OBP')
plot(x = 1871:2014, y = findAverageByYear(batters$SLG, 1871, 2014), main = 'SLG By Year', xlab = 'Year', ylab = 'Average SLG')
plot(x = 1871:2014, y = findAverageByYear(batters$ISO, 1871, 2014), main = 'ISO By Year', xlab = 'Year', ylab = 'Average ISO')
plot(x = 1871:2014, y = findAverageByYear(batters$OPS, 1871, 2014), main = 'OPS By Year', xlab = 'Year', ylab = 'Average OPS')
plot(x = 1871:2014, y = findAverageByYear(batters$BABIP, 1871, 2014), main = 'BABIP By Year', xlab = 'Year', ylab = 'Average BABIP')

startAge = 16
endAge = 50

plot(x = startAge:endAge, y = findAverageByAge(batters$AVE, startAge, endAge), main = 'Batting Average By Age', xlab = 'Age', ylab = 'Average Batting Avg')
plot(x = startAge:endAge, y = findAverageByAge(batters$G, startAge, endAge), main = 'Games By Age', xlab = 'Age', ylab = 'Average G')
plot(x = startAge:endAge, y = findAverageByAge(batters$AB, startAge, endAge), main = 'AB By Age', xlab = 'Age', ylab = 'Average AB')
plot(x = startAge:endAge, y = findAverageByAge(batters$R, startAge, endAge), main = 'R By Age', xlab = 'Age', ylab = 'Average R')
plot(x = startAge:endAge, y = findAverageByAge(batters$H, startAge, endAge), main = 'H By Age', xlab = 'Age', ylab = 'Average H')
plot(x = startAge:endAge, y = findAverageByAge(batters$X1B, startAge, endAge), main = '1B By Age', xlab = 'Age', ylab = 'Average 1B')
plot(x = startAge:endAge, y = findAverageByAge(batters$X2B, startAge, endAge), main = '2B By Age', xlab = 'Age', ylab = 'Average 2B')
plot(x = startAge:endAge, y = findAverageByAge(batters$X3B, startAge, endAge), main = '3B By Age', xlab = 'Age', ylab = 'Average 3B')
plot(x = startAge:endAge, y = findAverageByAge(batters$HR, startAge, endAge), main = 'HR By Age', xlab = 'Age', ylab = 'Average HR')
plot(x = startAge:endAge, y = findAverageByAge(batters$RBI, startAge, endAge), main = 'RBI By Age', xlab = 'Age', ylab = 'Average RBI')
plot(x = startAge:endAge, y = findAverageByAge(batters$SB, startAge, endAge), main = 'SB By Age', xlab = 'Age', ylab = 'Average SB')
plot(x = startAge:endAge, y = findAverageByAge(batters$CS, startAge, endAge), main = 'CS By Age', xlab = 'Age', ylab = 'Average CS')
plot(x = startAge:endAge, y = findAverageByAge(batters$BB, startAge, endAge), main = 'BB By Age', xlab = 'Age', ylab = 'Average BB')
plot(x = startAge:endAge, y = findAverageByAge(batters$SO, startAge, endAge), main = 'SO By Age', xlab = 'Age', ylab = 'Average SO')
plot(x = startAge:endAge, y = findAverageByAge(batters$IBB, startAge, endAge), main = 'IBB By Age', xlab = 'Age', ylab = 'Average IBB')
plot(x = startAge:endAge, y = findAverageByAge(batters$HBP, startAge, endAge), main = 'HBP By Age', xlab = 'Age', ylab = 'Average HBP')
plot(x = startAge:endAge, y = findAverageByAge(batters$SH, startAge, endAge), main = 'SH By Age', xlab = 'Age', ylab = 'Average SH')
plot(x = startAge:endAge, y = findAverageByAge(batters$SF, startAge, endAge), main = 'SF By Age', xlab = 'Age', ylab = 'Average SF')
plot(x = startAge:endAge, y = findAverageByAge(batters$GIDP, startAge, endAge), main = 'GIDP By Age', xlab = 'Age', ylab = 'Average GIDP')
plot(x = startAge:endAge, y = findAverageByAge(batters$KP, startAge, endAge), main = 'KP By Age', xlab = 'Age', ylab = 'Average KP')
plot(x = startAge:endAge, y = findAverageByAge(batters$BBP, startAge, endAge), main = 'BBP By Age', xlab = 'Age', ylab = 'Average BBP')
plot(x = startAge:endAge, y = findAverageByAge(batters$OBP, startAge, endAge), main = 'OBP By Age', xlab = 'Age', ylab = 'Average OBP')
plot(x = startAge:endAge, y = findAverageByAge(batters$SLG, startAge, endAge), main = 'SLG By Age', xlab = 'Age', ylab = 'Average SLG')
plot(x = startAge:endAge, y = findAverageByAge(batters$ISO, startAge, endAge), main = 'ISO By Age', xlab = 'Age', ylab = 'Average ISO')
plot(x = startAge:endAge, y = findAverageByAge(batters$OPS, startAge, endAge), main = 'OPS By Age', xlab = 'Age', ylab = 'Average OPS')
plot(x = startAge:endAge, y = findAverageByAge(batters$BABIP, startAge, endAge), main = 'BABIP By Age', xlab = 'Age', ylab = 'Average BABIP')


mean(AVG[which(batters$AB > 100 & batters$yearID == 1990)], na.rm = TRUE)

#OBP vs SLG
smoothScatter(x = batters$OBP[batters$AB > minAB], y = batters$SLG[batters$AB > minAB], main = 'OBP vs SLG', xlab = 'OBP', ylab = 'SLG')
#Positive correlation

#Strikout percentage vs walk percentage
smoothScatter(x = batters$KP[batters$AB > minAB], y = batters$BBP[batters$AB > minAB], main = 'KP vs BBP', xlab = 'KP', ylab = 'BBP')
#Not much correlation

#Strikout percentage vs batting average
smoothScatter(x = batters$KP[batters$AB > minAB], y = batters$AVE[batters$AB > minAB], main = 'KP vs AVE', xlab = 'KP', ylab = 'AVE')
#Small negative correlation

#AVG = batters$H[batters$AB > 0 & batters$yearID > 1999]/batters$AB[batters$AB > 0 && batters$yearID > 1999]
hist(AVG)
boxplot(AVG, main = 'Box Plot of Batting Average')
plot(batters$yearID, AVG)
cor(batters$H, batters$HR) #not working?
mean(AVG, na.rm = TRUE)
betterAVG = AVG[batters$H > 20]
mean(betterAVG, na.rm = TRUE)
smoothScatter(batters$yearID[batters$H > 20], betterAVG)
hist(betterAVG, main = 'Histogram of Batting Average For Players With >20 Hits', xlab = 'Batting Average')
smoothScatter(batters$AB[batters$H > 20], betterAVG, main = 'Number of At Bats vs Batting Average', xlab = 'Number of At Bats', ylab = 'Batting Average')
plot(batters$HR[batters$H > 20], betterAVG, main = 'Number of Home Runs vs Batting Average', xlab = 'Number of Home Runs', ylab = 'Batting Average')

SLG = as.numeric(as.character(batters$SLG[batters$AB > 500 && batters$yearID > 1990]))
OBP = batters$OBP[batters$AB > 500 && batters$yearID > 1990]
max(SLG, na.rm = TRUE)
plot(OBP[1], SLG[1])

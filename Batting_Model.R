
# Collaborators: Shane Sarnac, Ryan Smith, Zach Collins
# Date: March 11, 2016
# Summary: This code aims to find trends and develop a model to predict hitters' performances. 

# Read in data from the Batting, and  Master csv files from Lahmann database
batters = read.csv(file = "Baseball/Batting.csv", header = TRUE)
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

minAB = 125

findAverageByYear = function(stat, startYear, endYear, percentile = 0){
  
  total = mean(stat[batters$AB > minAB & batters$yearID == startYear], na.rm = TRUE)
  for (i in (startYear+1):endYear) {
    total = c(total,mean(stat[batters$AB > minAB & batters$yearID == i], na.rm = TRUE))
  }
  return(total)
}

minYear = 2000
maxYear = 2015

findAverageByAge = function(stat, startAge=20, endAge=40, percentile = 0, ages = batters$effective_age){
  stat = GetValidData(stat)
  ages = GetValidData(ages)
  lowerLimit = quantile(stat[ages == startAge], c(percentile), na.rm = TRUE)
  total = mean(stat[ages == startAge & stat >= lowerLimit], na.rm = TRUE)
  for (i in (startAge+1):endAge) {
    lowerLimit = quantile(stat[ages == i], c(percentile), na.rm = TRUE)
    total = c(total,mean(stat[ages == i & stat >= lowerLimit], na.rm = TRUE))
  }
  return(total)
}



plot(x = minYear:maxYear, y = findAverageByYear(batters$AVE, minYear, maxYear), main = 'Batting Average By Year', xlab = 'Year', ylab = 'Average Batting Avg')
plot(x = 1871:2014, y = findAverageByYear(batters$G, 1871, 2014), main = 'Games By Year', xlab = 'Year', ylab = 'Average G')
plot(x = 1871:2014, y = findAverageByYear(batters$AB, 1871, 2014), main = 'AB By Year', xlab = 'Year', ylab = 'Average AB')
plot(x = 1871:2014, y = findAverageByYear(batters$R, 1871, 2014), main = 'R By Year', xlab = 'Year', ylab = 'Average R')
plot(x = 1871:2014, y = findAverageByYear(batters$H, 1871, 2014), main = 'H By Year', xlab = 'Year', ylab = 'Average H')
plot(x = 1871:2014, y = findAverageByYear(batters$X1B, 1871, 2014), main = '1B By Year', xlab = 'Year', ylab = 'Average 1B')
plot(x = 1871:2014, y = findAverageByYear(batters$X2B, 1871, 2014), main = '2B By Year', xlab = 'Year', ylab = 'Average 2B')
plot(x = 1871:2014, y = findAverageByYear(batters$X3B, 1871, 2014), main = '3B By Year', xlab = 'Year', ylab = 'Average 3B')
plot(x = minYear:maxYear, y = findAverageByYear(batters$HR, minYear, maxYear), main = 'HR By Year', xlab = 'Year', ylab = 'Average HR')
plot(x = 1871:2014, y = findAverageByYear(batters$RBI, 1871, 2014), main = 'RBI By Year', xlab = 'Year', ylab = 'Average RBI')
plot(x = 1871:2014, y = findAverageByYear(batters$SB, 1871, 2014), main = 'SB By Year', xlab = 'Year', ylab = 'Average SB')
plot(x = 1871:2014, y = findAverageByYear(batters$CS, 1871, 2014), main = 'CS By Year', xlab = 'Year', ylab = 'Average CS')
plot(x = 1871:2014, y = findAverageByYear(batters$BB, 1871, 2014), main = 'BB By Year', xlab = 'Year', ylab = 'Average BB')
plot(x = 1871:2014, y = findAverageByYear(batters$SO, 1871, 2014), main = 'SO By Year', xlab = 'Year', ylab = 'Average SO')
plot(x = 1871:2014, y = findAverageByYear(batters$IBB, 1871, 2014), main = 'IBB By Year', xlab = 'Year', ylab = 'Average IBB')
plot(x = 1871:2014, y = findAverageByYear(batters$HBP, 1871, 2014), main = 'HBP By Year', xlab = 'Year', ylab = 'Average HBP')
plot(x = 1871:2014, y = findAverageByYear(batters$GIDP, 1871, 2014), main = 'GIDP By Year', xlab = 'Year', ylab = 'Average GIDP')
plot(x = minYear:maxYear, y = findAverageByYear(batters$KP, minYear, maxYear), main = 'KP By Year', xlab = 'Year', ylab = 'Average KP')
plot(x = minYear:maxYear, y = findAverageByYear(batters$BBP, 1871, 2014), main = 'BBP By Year', xlab = 'Year', ylab = 'Average BBP')
plot(x = minYear:maxYear, y = findAverageByYear(batters$OBP, minYear, maxYear), main = 'OBP By Year', xlab = 'Year', ylab = 'Average OBP')
plot(x = minYear:maxYear, y = findAverageByYear(batters$SLG, minYear, maxYear), main = 'SLG By Year', xlab = 'Year', ylab = 'Average SLG')
plot(x = 1871:2014, y = findAverageByYear(batters$ISO, 1871, 2014), main = 'ISO By Year', xlab = 'Year', ylab = 'Average ISO')
plot(x = minYear:maxYear, y = findAverageByYear(batters$OPS, minYear, maxYear), main = 'OPS By Year', xlab = 'Year', ylab = 'Average OPS')
plot(x = 1871:2014, y = findAverageByYear(batters$BABIP, 1871, 2014), main = 'BABIP By Year', xlab = 'Year', ylab = 'Average BABIP')

startAge = 20
endAge = 40

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
plot(x = startAge:endAge, y = findAverageByAge(batters$GIDP, startAge, endAge), main = 'GIDP By Age', xlab = 'Age', ylab = 'Average GIDP')
plot(x = startAge:endAge, y = findAverageByAge(batters$KP, startAge, endAge), main = 'KP By Age', xlab = 'Age', ylab = 'Average KP')
plot(x = startAge:endAge, y = findAverageByAge(batters$BBP, startAge, endAge), main = 'BBP By Age', xlab = 'Age', ylab = 'Average BBP')
plot(x = startAge:endAge, y = findAverageByAge(batters$OBP, startAge, endAge), main = 'OBP By Age', xlab = 'Age', ylab = 'Average OBP')
plot(x = startAge:endAge, y = findAverageByAge(batters$SLG, startAge, endAge), main = 'SLG By Age', xlab = 'Age', ylab = 'Average SLG')
plot(x = startAge:endAge, y = findAverageByAge(batters$ISO, startAge, endAge), main = 'ISO By Age', xlab = 'Age', ylab = 'Average ISO')
plot(x = startAge:endAge, y = findAverageByAge(batters$BABIP, startAge, endAge), main = 'BABIP By Age', xlab = 'Age', ylab = 'Average BABIP')

par(mfrow = c(1,2))
plot(x = startAge:endAge, y = findAverageByAge(batters$AVE, startAge, endAge), main = 'Batting Average By Age', xlab = 'Age', ylab = 'Average Batting Avg')
plot(x = startAge:endAge, y = findAverageByAge(batters$OPS, startAge, endAge), main = 'OPS By Age', xlab = 'Age', ylab = 'Average OPS')

par(mfrow = c(1,1))

plot(x = startAge:endAge, y = findAverageByAge(batters$AVE, startAge, endAge, 0.96), main = 'Batting Average By Age', xlab = 'Age', ylab = 'Average Batting Avg')
plot(x = startAge:endAge, y = findAverageByAge(batters$OPS, startAge, endAge, 0.96), main = 'OPS By Age', xlab = 'Age', ylab = 'Average OPS')
plot(x = startAge:endAge, y = findAverageByAge(batters$HR, startAge, endAge, 0.96), main = 'HR By Age', xlab = 'Age', ylab = 'Average HR')


#OBP vs SLG
smoothScatter(x = batters$OBP[batters$AB > minAB], y = batters$SLG[batters$AB > minAB], main = 'OBP vs SLG', xlab = 'OBP', ylab = 'SLG')
#Positive correlation

#BABIP vs SLG
smoothScatter(x = batters$OBP[batters$AB > minAB], y = batters$SLG[batters$AB > minAB], main = 'OBP vs SLG', xlab = 'OBP', ylab = 'SLG')


#Strikout percentage vs walk percentage
smoothScatter(x = batters$KP[batters$AB > minAB], y = batters$BBP[batters$AB > minAB], main = 'KP vs BBP', xlab = 'KP', ylab = 'BBP')
#Not much correlation

#Strikout percentage vs batting average
smoothScatter(x = batters$KP[batters$AB > minAB], y = batters$AVE[batters$AB > minAB], main = 'KP vs AVE', xlab = 'KP', ylab = 'AVE')
#Small negative correlation

OBP = batters$OBP[batters$AB > 500 && batters$yearID > 1990]

GetValidData = function(stat)
{
  return (stat[batters$AB > minAB & batters$yearID >= minYear])
}

ConvertData = function(stat)
{
  newStat = c()
  for (i in 0:length(stat))
  {
    newStat = c(newStat, stat[i])
  }
  return(newStat)
}

ISO = batters$ISO[batters$AB > minAB & batters$yearID >= minYear]
newISO = ConvertData(ISO)
OBP = batters$OBP[batters$AB > minAB & batters$yearID >= minYear]
newOBP = ConvertData(OBP)
smoothScatter(ISO, OBP)

ClusterStat = function(mydata, numClusters = 5)
{
  # K-Means Cluster Analysis
  fit = kmeans(mydata, numClusters) # 5 cluster solution
  # get cluster means 
  aggregate(mydata,by=list(fit$cluster),FUN=mean)
  # append cluster assignment
  clusteredData <- data.frame(mydata, fit$cluster)
  return(clusteredData)
}

clusteredISO = ClusterStat(newISO)
plot(clusteredISO)
clusteredOBP = ClusterStat(newOBP)
plot(clusteredOBP)

OPS = GetValidData(batters$OPS)
clusteredOPS = ClusterStat(OPS, 5)
plot(clusteredOPS)

GetCluster = function(cData, clusterNum)
{
  newStat = c()
  for (i in 1:dim(cData)[1])
  {
    if (cData[i,2] == clusterNum) newStat = c(newStat, cData[i,1])
  }
  return(newStat)
}

GetClusterIndices = function(cData, clusterNum)
{
  newStat = c()
  for (i in 1:dim(cData)[1])
  {
    if (cData[i,2] == clusterNum) newStat = c(newStat, i)
  }
  return(newStat)
}


targetCluster = clusteredOPS

cluster1 = GetCluster(targetCluster, 1)
cluster2 = GetCluster(targetCluster, 2)
cluster3 = GetCluster(targetCluster, 3)
cluster4 = GetCluster(targetCluster, 4)
cluster5 = GetCluster(targetCluster, 5)

ci1 = GetClusterIndices(targetCluster, 1)
ci2 = GetClusterIndices(targetCluster, 2)
ci3 = GetClusterIndices(targetCluster, 3)
ci4 = GetClusterIndices(targetCluster, 4)
ci5 = GetClusterIndices(targetCluster, 5)
targetCluster[ci1,1]

age = GetValidData(batters$effective_age)

xvar = SLG
yvar = OBP

plot(xvar[ci1], yvar[ci1], col = "green", xlim = c(min(xvar), max(xvar)), 
     ylim = c(min(yvar),max(yvar)), xlab = "SLG", ylab = "OBP", main = "Clustered OPS")
points(xvar[ci2], yvar[ci2], col = "yellow")
points(xvar[ci3], yvar[ci3], col = "orange")
points(xvar[ci4], yvar[ci4], col = "red")
points(xvar[ci5], yvar[ci5], col = "blue")


findAverageByAgeClustered = function(stat, ages, startAge=20, endAge=40)
{
  total = mean(stat[ages == startAge], na.rm = TRUE)
  for (i in (startAge+1):endAge) 
  {
    total = c(total,mean(stat[ages == i], na.rm = TRUE))
  }
  return(total)
}

#Apply poly regression to average OPS by age
avgCluster1 = findAverageByAgeClustered(OPS[ci1], age[ci1])

minPlayerAB = 500

AB = GetValidData(batters$AB)
playerID = GetValidData(batters$playerID)



GetValidPlayers = function()
{
  validPlayers = c()
  for (player in master$playerID)
  {
    if (player %in% playerID & sum(AB[playerID == player]) > minPlayerAB)
    {
      validPlayers = c(validPlayers,  player)
      
    }
  }
  return(validPlayers)
}

#players that have played since 2000
validPlayers = GetValidPlayers()

GetPlayerOPS = function()
{
  playerOPS = c()
  for (player in master$playerID)
  {
    if (player %in% playerID & sum(AB[playerID == player]) > minPlayerAB)
    {
      playerOPS = c(playerOPS, mean(OPS[playerID == player]))
      
    }
  }
  return(playerOPS)
}
playerOPS = GetPlayerOPS()

#Cluster players
cPlayerOPS = ClusterStat(playerOPS, 5)
plot(cPlayerOPS)

validPlayers[cPlayerOPS[,2] == 2]

age = GetValidData(batters$effective_age)

AverageOPSByAgeForCluster = function(clust)
{
  clusterPlayers = validPlayers[cPlayerOPS[,2] == clust]
  ageAVG = c()
  for (i in startAge:endAge)
  {
    ageAVG = c(ageAVG,mean(OPS[playerID %in% clusterPlayers & age == i]))
  }
  return(ageAVG)
}

cluster1AVG = AverageOPSByAgeForCluster(1)
cluster2AVG = AverageOPSByAgeForCluster(2)
cluster3AVG = AverageOPSByAgeForCluster(3)
cluster4AVG = AverageOPSByAgeForCluster(4)
cluster5AVG = AverageOPSByAgeForCluster(5)

plot(startAge:endAge, cluster1AVG)

xvar = startAge:endAge
yvar = OPS

plot(xvar,cluster1AVG, col = "orange", xlim = c(startAge, endAge), 
     ylim = c(0.5,1), xlab = "Age", ylab = "OPS", main = "Clustered OPS by AGE")
points(xvar, cluster2AVG, col = "blue")
points(xvar, cluster3AVG, col = "red")
points(xvar, cluster4AVG, col = "green")
points(xvar, cluster5AVG, col = "cyan")

lines(xvar, cluster1AVG, col = "orange")
lines(xvar, cluster2AVG, col = "blue")
lines(xvar, cluster3AVG, col = "red")
lines(xvar, cluster4AVG, col = "green")
lines(xvar, cluster5AVG, col = "cyan")

#change from previous year
Diff = function(data)
{
  diff = c()
  for (i in 1:length(data)-1)
  {
    diff = c(diff, data[i+1]-data[i])
  }
  return(diff)
}

diff1 = Diff(cluster1AVG)
diff2 = Diff(cluster2AVG)
diff3 = Diff(cluster3AVG)
diff4 = Diff(cluster4AVG)
diff5 = Diff(cluster5AVG)

#predict performance based on diff
#What age? What cluster do they fit best in (either at their current age or whole career)?
#Add appropriate diff to current OPS to predict next year. Add subsequent diffs to predict
#subsequent years
PredictPerformanceFromDiff = function(targetPlayerAge, targetPlayerOPS, predictionAge)
{
  if (is.null(targetPlayerOPS)) return(NA)
  if (identical(targetPlayerOPS, numeric(0))) return(NA)
  
  clust = 0
  minDist = 100
  i = 0
  for (clusterAVG in list(cluster1AVG, cluster2AVG, cluster3AVG, cluster4AVG, cluster5AVG))
  {
    newMin = abs(clusterAVG[targetPlayerAge-startAge + 1] - targetPlayerOPS)
    i = i+1
    if (!is.na(newMin))
    {
      if (newMin < minDist)
      {
        minDist = newMin
        clust = i
      }
    }
  }
  if (clust == 1) diff = diff1
  if (clust == 2) diff = diff2
  if (clust == 3) diff = diff3
  if (clust == 4) diff = diff4
  if (clust == 5) diff = diff5
  
  currentOPS = targetPlayerOPS
  numYears = predictionAge - targetPlayerAge
  
  while (numYears > 0)
  {
    currentOPS = currentOPS + diff[targetPlayerAge - startAge + 1]
    numYears = numYears - 1
    targetPlayerAge = targetPlayerAge + 1
  }
  return(currentOPS)
}

targetPlayerAge = 21
targetPlayerOPS = 0.7
predictionAge = 22
predictedOPS = PredictPerformanceFromDiff(targetPlayerAge, targetPlayerOPS, predictionAge)
predictedOPS

#error (prediciton distance is number of years to predict into future)
ComputeErrorForDiff = function(predicitonDistance = 1)
{
  errorList = c()
  for (targetPlayer in validPlayers)
  {
    for (currentAge in startAge:endAge)
    {
      targetAge = currentAge + predicitonDistance
      pred = PredictPerformanceFromDiff(currentAge, OPS[playerID == targetPlayer & age == currentAge], targetAge)
      actual = OPS[playerID == targetPlayer & age == targetAge]
      errorList = c(errorList,abs((pred - actual)/actual))
    }
  }
  
  return(errorList)
}

error1 = ComputeErrorForDiff(1)
error2 = ComputeErrorForDiff(2)
error3 = ComputeErrorForDiff(3)
error4 = ComputeErrorForDiff(4)

error5 = ComputeErrorForDiff(5)
error6 = ComputeErrorForDiff(6)
error7 = ComputeErrorForDiff(7)
error8 = ComputeErrorForDiff(8)
error9 = ComputeErrorForDiff(9)

error = error4
meanError = mean(error, na.rm = TRUE)
minError = min(error, na.rm = TRUE)
maxError = max(error, na.rm = TRUE)
meanError
minError
maxError


meanError = mean(error1, na.rm = TRUE)
meanError
maxError = max(error1, na.rm = TRUE)
maxError
meanError = mean(error2, na.rm = TRUE)
meanError
maxError = max(error2, na.rm = TRUE)
maxError
meanError = mean(error3, na.rm = TRUE)
meanError
maxError = max(error3, na.rm = TRUE)
maxError
meanError = mean(error4, na.rm = TRUE)
meanError
maxError = max(error4, na.rm = TRUE)
maxError

meanError = mean(error5, na.rm = TRUE)
meanError
maxError = max(error5, na.rm = TRUE)
maxError
meanError = mean(error6, na.rm = TRUE)
meanError
maxError = max(error6, na.rm = TRUE)
maxError
meanError = mean(error7, na.rm = TRUE)
meanError
maxError = max(error7, na.rm = TRUE)
maxError
meanError = mean(error8, na.rm = TRUE)
meanError
maxError = max(error8, na.rm = TRUE)
maxError
meanError = mean(error9, na.rm = TRUE)
meanError
maxError = max(error9, na.rm = TRUE)
maxError

targetPlayer = validPlayers[2]
currentAge = 27
targetAge = 28
pred = PredictPerformanceFromDiff(currentAge, OPS[playerID == targetPlayer & age == currentAge], targetAge)
actual = OPS[playerID == targetPlayer & age == targetAge]
error = (pred - actual)/actual
pred
actual
error



ComputeErrorForGuessSame = function(predicitonDistance = 1)
{
  errorList = c()
  for (targetPlayer in validPlayers)
  {
    for (currentAge in startAge:endAge)
    {
      targetAge = currentAge + predicitonDistance
      pred = OPS[playerID == targetPlayer & age == currentAge]
      actual = OPS[playerID == targetPlayer & age == targetAge]
      errorList = c(errorList,abs((pred - actual)/actual))
    }
  }
  
  return(errorList)
}


errorSame1 = ComputeErrorForGuessSame(1)
errorSame2 = ComputeErrorForGuessSame(2)
errorSame3 = ComputeErrorForGuessSame(3)
errorSame5 = ComputeErrorForGuessSame(5)
errorSame7 = ComputeErrorForGuessSame(7)
errorSame9 = ComputeErrorForGuessSame(9)

error = errorSame9
meanError = mean(error, na.rm = TRUE)
minError = min(error, na.rm = TRUE)
maxError = max(error, na.rm = TRUE)
meanError
minError
maxError

findSeasonAge = function(season_year, birth_year, birth_month) {
  effective_birth_year = ifelse(birth_month < 7, birth_year, birth_year - 1) 
  age = season_year - effective_birth_year
  return (age)
}

findTotalStrikeouts = function(start_year) {
  total = sum(teams$SOA[which(teams$yearID == start_year)])
  for (i in (start_year+1):2014) {
    total = c(total,sum(teams$SOA[which(teams$yearID == i)]))
  }
  #print(total)
  return (total)
}

findTotalOpponentEarnedRuns = function(start_year, end_year) {
  total = sum(teams$RA[which(teams$yearID == start_year)])
  for (i in (start_year+1):end_year) {
    total = c(total,sum(teams$RA[which(teams$yearID == i)]))
  }
  #print(total)
  return (total)
}

findTotalStrikeoutsAge = function(start_year, end_year, effective_age) {
  total = sum(pitchers$SO[which(pitchers$effective_age[which(pitchers$yearID == start_year)
                                                       ] == effective_age)])
  for(i in (start_year+1):end_year) {
    total = c(total, sum(pitchers$SO[which(pitchers$effective_age[
      which(pitchers$yearID == i)] == effective_age)]))
  }
  return (total)
}

findAverageStrikeoutsAge = function(start_year, end_year, effective_age) {
  total = mean(pitchers$SO[which(pitchers$effective_age[which(pitchers$yearID == start_year)
                                                       ] == effective_age)])
  for(i in (start_year+1):end_year) {
    total = c(total, mean(pitchers$SO[which(pitchers$effective_age[
      which(pitchers$yearID == i)] == effective_age)]))
  }
  return (total)
}

findAverageStrikeoutsAge = function(start_year, end_year, effective_age, min_innings) {
  pitchers_in_year = which(pitchers$yearID == start_year)
  pitchers_at_age = which(pitchers$effective_age[pitchers_in_year] == effective_age)
  pitchers_with_min_innings = which(pitchers$IP[pitchers_at_age] >= min_innings)
  total = mean(pitchers$SO[pitchers_with_min_innings])
  for(i in (start_year+1):end_year) {
    pitchers_in_year = which(pitchers$yearID == i)
    pitchers_at_age = which(pitchers$effective_age[pitchers_in_year] == effective_age)
    pitchers_with_min_innings = which(pitchers$IP[pitchers_at_age] >= min_innings)
    total = c(total, mean(pitchers$SO[pitchers_with_min_innings]))
  }
  return (total)
}

# Read in data from the Pitching, Team, and  Master csv files from Lahmann database
pitchers = read.csv(file = 'Baseball/Pitching.csv', header = TRUE)
teams = read.csv(file = '~/Documents/Baseball/Teams.csv', header = TRUE)
master = read.csv(file = '~/Documents/Baseball/Master.csv', header = TRUE)

# Find where all pitchers are listed in the Master csv file
pitcher_indices_in_master = match(pitchers$playerID, master$playerID)

# Determine the age of the pitcher during a given season
pitchers$effective_age = findSeasonAge(
  pitchers$yearID,
  master$birthYear[pitcher_indices_in_master],
  master$birthMonth[pitcher_indices_in_master]
)

# Convert IPouts to IP (innings pitched)
pitchers$IP = pitchers$IPouts/3

# Determine the trends in strikeouts since 1999
years_since_1998 = which(teams$yearID > 1998)

par(mfrow = c(1,2))
plot(x = 1900:2014, y = findTotalStrikeouts(1900), main = "Total Strikeouts in MLB per Year",
     xlab = "Year", ylab = "Total Number of Strikeouts by Pitchers")
plot(x = 1900:2014, y = findTotalOpponentEarnedRuns(1900, 2014), main = "Total Runs Allowed in MLB per Year", 
     xlab = "Year", ylab = "Total Runs Scored")

# Determine trends in strikeouts by age per year
par(mfrow = c(1,1))
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 20, 150), col = "blue")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 21, 150), col = "red")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 22, 150), col = "green")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 23, 150), col = "orange")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 24, 150), col = "purple")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 25, 150), col = "yellow")
plot(x = 1999:2014, y = findAverageStrikeoutsAge(1999, 2014, 26, 150), col = "coral")
plot(x = 1950:2014, y = findAverageStrikeoutsAge(1950, 2014, 27, 150))

findAverageStrikeoutsAge(2000, 2014, 26,150)


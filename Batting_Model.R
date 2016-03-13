
# Collaborators: Shane Sarnac, Ryan Smith, Zachary Collins
# Date: March 11, 2016
# Summary: This code aims to find trends and develop a model to predict hitters' performances. 

batters = read.csv(file = "Baseball/Batting.csv", header = TRUE)

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

# OPS = On base Plus Slugging
batters$OPS = batters$SLG + batters$OBP

############################################################################################

which(batters$yearID > 1990)

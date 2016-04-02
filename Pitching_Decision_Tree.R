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
eval_control = function() {
  control = NULL
  N = length(pitchers$playerID)
  for (i in 0:N) {
    
  }
}
pitchers = read.csv(file = 'Baseball/Pitching.csv', header = TRUE)

# Add evaluating statistics
pitchers$IP = pitchers$IPouts / 3
pitchers$SO_9 = pitchers$SO * 9 / pitchers$IP
pitchers$SO_Percentage = pitchers$SO / pitchers$BFP
pitchers$BB_9 = pitchers$BB *9 / pitchers$IP
pitchers$BB_Percentage = pitchers$BB / pitchers$BFP

pitchers$command = eval_command()
pitchers$command = as.factor(pitchers$command)




train = pitchers[which(pitchers$yearID >= 2000 & pitchers$yearID <= 2005), ]

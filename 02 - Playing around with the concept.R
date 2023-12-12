#------------------------------------------------------------------------------#
# 02 - Playing around with the concept and coding
# Simulation of "shut the box" dice game
#------------------------------------------------------------------------------#

# There are a few possible strategies that come to mind, but firstly I need
# to make the basic functions that will describe the game.

#------------------------------------------------------------------------------#
# Just playing around with the concept                                      ####
#------------------------------------------------------------------------------#


# We have a vector, initial empty, which will be populated as numbers are
# rolled

outcome <- c()

# This is the initial roll

roll1 <- sample(1:6,1)
roll2 <- sample(1:6,1)
sum = roll1 + roll2

# check if these numbers are in the outcome vector
check1 <- roll1 %in% outcome
check2 <- roll2 %in% outcome
checksum <- sum %in% outcome

# Then we need a decision point for the selection, based on strategy
strategy = 1 # I'll populate these later. For now 1 just maximises #matches

if(strategy == 1){ # Maximises number of matches
  
  if(check1 == TRUE & check2 == TRUE & checksum == TRUE){
    outcome = outcome
  } else if((check1 == TRUE | check2 == TRUE) & checksum == FALSE){
    outcome[length(outcome)+1] <- sum
  } else if(roll1 == roll2 & check1 == FALSE){
    outcome[length(outcome)+1] <- roll1
  } else {
    if(check1 == FALSE){outcome[length(outcome)+1] <- roll1}
    if(check2 == FALSE){outcome[length(outcome)+1] <- roll2}
  }
}

# More strategies will be added in the full function

# Now we check the length of the outcome vector: if it is 10, the game is over
outcome = sort(outcome)
if(length(outcome) == 10){print("The game is over!")}


#------------------------------------------------------------------------------#
# Putting this into a function                                              ####
#------------------------------------------------------------------------------#

# OK that works, now to put it into a function. I will have it return the
# number of runs it takes to complete.

# This will be updated in the next script as more strategies are added.

fn_ShutTheBox <- function(strategy){
  
    # Stop if the argument is not numeric
    stopifnot(is.numeric(strategy))
    
    # Initial outcome vector
    outcome <- c()
    
    # Initial index
    runs = 0
    
    # Now we begin the loop. We go until length(outcome) == 10
    
    for(runs in 1:1000){
    
        # This is the initial roll
        roll1 <- sample(1:6,1)
        roll2 <- sample(1:6,1)
        sum = roll1 + roll2
        
        # check if these numbers are in the outcome vector
        check1 <- roll1 %in% outcome
        check2 <- roll2 %in% outcome
        checksum <- sum %in% outcome
        
        # Then we need a decision point for the selection, based on strategy
        
        if(strategy == 1){ # Maximises number of matches
          
          if(check1 == TRUE & check2 == TRUE & checksum == TRUE){
            outcome = outcome
          } else if(check1 == FALSE & check2 == FALSE & (roll1 != roll2)){
            outcome[length(outcome)+1] <- roll1
            outcome[length(outcome)+1] <- roll2
          } else if(check1 == FALSE & check2 == FALSE & (roll1 == roll2)){
            outcome[length(outcome)+1] <- roll1
          } else if((check1 == TRUE | check2 == TRUE) & checksum == FALSE){
            outcome[length(outcome)+1] <- sum
          } else if(checksum == TRUE & (roll1 != roll2)){
            if(check1 == FALSE){outcome[length(outcome)+1] <- roll1}
            if(check2 == FALSE){outcome[length(outcome)+1] <- roll2}
          } else if(checksum == TRUE & (roll1 != roll2)){
            outcome[length(outcome)+1] <- roll1
          }
        }
        
        # Now we check the length of the outcome vector: if it is 10, the game is over
        outcome = sort(outcome)
        
        # Now for the check
        if(length(outcome)>=10){break}
        
        }
    return(runs)   
    }

# Now to test
test1 <- fn_ShutTheBox(1)
# Works a treat.

#------------------------------------------------------------------------------#
# Setting up the loop                                                       ####
#------------------------------------------------------------------------------#

# Let's do 1000 games to test the outcomes
result = c()
for(i in 1:1000){
  numruns <- fn_ShutTheBox(1)
  result[length(result)+1] <- numruns
}

hist(result)

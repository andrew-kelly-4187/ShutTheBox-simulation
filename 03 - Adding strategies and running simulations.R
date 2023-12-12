#------------------------------------------------------------------------------#
# 03 - Adding strategies and running simulations
# Simulation of "shut the box" dice game
#------------------------------------------------------------------------------#

library(magrittr) # This is in tidyverse but for some reason renv never includes 

# Now we update the function from file 02 with some strategies

# Strategy 1: Maximise the number of matches
# Strategy 2: Prioritise higher numbers 
# Strategy 3: Prioritise 1,2
# Strategy 4: Prioritise tails (combination of 2 and 3)
# Strategy 5: Prioritise centre (going with binomial dist)

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
    
    if(strategy == 2){ # Prioritise 8,9,10
      
      if(check1 == TRUE & check2 == TRUE & checksum == TRUE){
        outcome = outcome
      } else if(checksum == FALSE & sum > 7){
        outcome[length(outcome)+1] <- sum
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
    
    if(strategy == 3){ # Prioritise 1,2
      
      if(check1 == TRUE & check2 == TRUE & checksum == TRUE){
        outcome = outcome
      } else if(check1 == FALSE & roll1 < 3){
        outcome[length(outcome)+1] <- roll1
      } else if(check2 == FALSE & roll2 < 3){
        outcome[length(outcome)+1] <- roll2
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
    
    if(strategy == 4){ # Prioritise 1,2 and 8,9,10
      
      if(check1 == TRUE & check2 == TRUE & checksum == TRUE){
        outcome = outcome
      } else if(checksum == FALSE & sum > 7){
        outcome[length(outcome)+1] <- sum
      } else if(check1 == FALSE & roll1 < 3){
        outcome[length(outcome)+1] <- roll1
      } else if(check2 == FALSE & roll2 < 3){
        outcome[length(outcome)+1] <- roll2
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


# Testing
# Let's do 1000 games to test the outcomes
strategy = 4
result = c()
tictoc::tic()
for(i in 1:100000){
  numruns <- fn_ShutTheBox(strategy)
  result[length(result)+1] <- numruns
}
time = tictoc::toc()

# Assign these manually for now
result1 <- result
result2 <- result
result3 <- result
result4 <- result

comparison <- data.frame(result1, result2, result3, result4)

summary(result1)
summary(result2)
summary(result3)
summary(result4)

comparison %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "vector", values_to = "runs") %>% 
  dplyr::mutate(strategy = substr(vector,7,7)) %>% 
  dplyr::select(strategy, runs) %>% 
  ggplot2::ggplot(
    ggplot2::aes(x = runs, colour = strategy)
  ) + 
  ggplot2::geom_freqpoly(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), 
                          alpha = 0.3,
                          binwidth = 1)

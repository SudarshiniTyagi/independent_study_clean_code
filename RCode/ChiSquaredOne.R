######################################
# One Variable Chi-Squared Significance Test
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
# 
# Simulates 10,000 experiments, where each experiment is 60 tosses of a fair die.
# For each experiment we count the number of 1s, 2s, 3s, 4s, 5s, and 6s that occur. 
# We then compare our observed number of  1s, 2s, 3s, 4s, 5s, and 6s in 60 tosses 
# of a real die to this distribution, to determine the probability that our sample 
# came from this assumed distribution (to decide whether this die is fair).
# 
# Author: Manda Wilson
#
# Example of FASTA formatted input file: 
# >expected
# 10 10 10 10 10 10
# >observed
# 14 16 6 9 5 10
#
# Pseudocode:
#
# 1. Calculate chi-squared for the observed values (in our example it is 9.4).  
#    a. For each category (in this example 1, 2, 3, 4, 5, and 6 are the categories):
#       i. Subtract the expected count from the observed count
#       ii. Square the result of step (1ai)
#       iii. Divide the result of step (1aii) by the expected count
#    b. Sum the results of step (1a), this is r, our observed chi-squared value
#
# 2. Set a counter to 0, this will count the number of times we get a chi-squared
#    greater than or equal to 9.4.  
#
# 3. Do the following 10,000 times:
#    a. Create an array that is equal in length to the number of categories (in our example, 6).  
#       All values in this array should start as 0.  This array will be used to count the number
#       of observations for each category (the number of times we roll a 1, a 2, etc.).
#    b. Use the total sum of expected counts to determine how many times to do the following (in 
#       our example there are a total of 60 die rolls, so we do this 60 times):
#       i. Pick a random number and increment the count in the bin it corresponds to
#    c. Calculate chi-squared on the results from step (3b), just as we did in step (1).
#    d. If the result from step (3c) is greater than or equal to our observed chi-squared (9.4), increment our counter 
#       from step (2). 
#
# 3. counter / 10,000 equals the probability of getting a chi-squared greater than or equal to
#    9.4, if the die is in fact fair.
#
######################################


######################################
#
# Subroutines
#
######################################

# will draw num values, given
# probabilities given by counts in expected
drawfromcategories <- function(num, expected){
  # if we have two bins (two categories)
  # bin 1 and bin 2
  # and we expect bin 1 to get a hit 2 of 5 times
  # and bin 2 to get a hit 3 of 5
  # then we store 2 in bins[0], and 5 in bins[1]
  # then if we draw anything 2 or under we know it is for bin 1
  # else if we draw anything 5 or under we know it is for bin 2
  # the expected values give us our probabilities
  max <- 0
  bins = c()
  observed = c()
  # in this loop we weight each bin
  # and we initialize observed counts to 0
  for (b in c(1:length(expected))){
    max <- max + expected[b]
    bins <- c(bins, max)
    observed <- c(observed, 0)
    num_bins <- length(bins)
  }
  for (d in c(1:num)){
    draw <- sample(c(1:max),1)
    # which bin does this belong in?
    b <- 1
    while ((b < num_bins) && (draw > bins[b])){
      b <- b+1 # move to next bin
    }
    observed[b] <- observed[b] + 1 # this is the category that was drawn
  }
  return(observed)
}


  
chisquared <- function(expected, observed){
  count <- length(expected)
  total <- 0
  for(i in c(1:count)){
    total <- total + (((observed[i] - expected[i])**2) / expected[i])
  }
  return(total)
}
    



######################################
#
# Computations
#
######################################
library(rjson)

#Change this to wherever you are keeping the folder RCode
setwd("/Users/sudarshinityagi/PycharmProjects/independent_study_clean_code/independent_study_clean_code")

input_file <- "RCode/generated_files/ChiSquared.json"
# list of vectors
json_data <- fromJSON(file=input_file)

expected <- json_data[[1]]
observed <- json_data[[2]]

observed_chi_squared <- chisquared(expected, observed)

num_observations <- sum(observed)

count = 0
num_runs = 10000

for (i in c(1:num_runs)){
  # roll a fair die num_observations times, counting the results
  simulated_observed <- drawfromcategories(num_observations, expected)
  chi_squared <- chisquared(expected, simulated_observed)
  if (chi_squared >= observed_chi_squared){
    count = count + 1
  }
}

######################################
#
# Output
#
######################################

cat ("Observed chi-squared: ", observed_chi_squared, "\n")
cat (count, "out of 10000 experiments had a chi-squared difference greater than or equal to", observed_chi_squared, "\n")
cat ("Probability that chance alone gave us a chi-squared greater than or equal to ", observed_chi_squared, " is", (count / num_runs), "\n")

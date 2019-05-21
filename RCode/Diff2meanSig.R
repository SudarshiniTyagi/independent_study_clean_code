#!/usr/bin/python

######################################
# Difference between Two Means Significance Test
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
# 
# Assuming that there is no significant difference in the means of 
# the two samples, tests to see the probability of getting a difference 
# greater than or equal to the observed difference in the means by chance 
# alone.  Uses shuffling & bootstrapping to get a distribution to compare 
# to the observed statistic.
# 
# Author: Manda Wilson
#
# Example of FASTA formatted input file (this is only for 2 groups): 
# >placebo_vals
# 54 51 58 44 55 52 42 47 58 46
# >drug_vals
# 54 73 53 70 73 68 52 65 65
#
# Pseudocode:
#
# 1. Measure the difference between the two group means.  The difference in means is measured
#    by (sum(grpA) / len(grpA)) - (sum(grpB) / len(grpB)).  In this example the difference between
#    the two group means is 12.97.
#
# 2. Set a counter to 0, this will count the number of times we get a difference
#    between the means greater than or equal to 12.97.  
#
# 3. Do the following 10,000 times:
#    a. Shuffle the original measurements.  To do this:
#       i. put the values from all the groups into one array but remembering the start and end indexes of each group
#       ii. shuffle the values in the array, effectively reassigning the values to different groups
#    b. Measure the difference between the two group means, just as we did in step (1).
#    c. If the difference from step (3b) is greater than or equal to 12.97, increment our counter 
#       from step (2). Note: if our original difference between the means were a negative value 
#       we would check for values less than or equal to that value.
#
# 4. counter / 10,000 equals the probability of getting our observed difference of two means greater than
#    or equal to 12.97, if there is in fact no significant difference.
#
######################################

######################################
#
# Subroutines
#
######################################

# takes a list of groups (two or more)
# pools all values, shuffles them, and makes new groups 
# of same size as original groups
# returns these new groups
# example of shuffle with more than two groups: OneWayAnovaSig.py
shuffle_data <- function(grps){
  num_grps <- length(grps)
  pool <- c()
  
  # pool all values
  for (i in c(1 : num_grps)){
    pool <- c(pool, grps[[i]])
  }
  # mix them up
  pool <- sample(pool)
  # reassign to groups of same size as original groups
  new_grps <- list()
  start_index <- 1
  end_index <- 1
  for (j in c(1 : num_grps)){
    end_index <- start_index + length(grps[[j]]) - 1
    new_grps[[j]] <- pool[start_index:end_index]
    start_index <- end_index + 1
  }
  return(new_grps)
}


# subtracts group A mean from group B mean and returns result
meandiff <- function(grpA, grpB){
  return (mean(grpB) - mean(grpA))
}

######################################
#
# Computations
#
######################################
library(rjson)

#Change this to wherever you are keeping the folder RCode
setwd("/Users/sudarshinityagi/PycharmProjects/independent_study_clean_code/independent_study_clean_code")

input_file <- "RCode/generated_files/Diff2Mean.json"

json_data <- fromJSON(file=input_file)

a = 1
b = 2
observed_mean_diff <- meandiff(json_data[[a]], json_data[[b]])

count <- 0
num_shuffles = 10000

for (i in c(1: num_shuffles)){
  new_data <- shuffle_data(json_data)
  mean_diff <- meandiff(new_data[[a]], new_data[[b]])
  # if the observed difference is negative, look for differences that are smaller
  # if the observed difference is positive, look for differences that are greater
  if (observed_mean_diff < 0 && mean_diff <= observed_mean_diff){
    count <- count + 1
  }
  else if(observed_mean_diff >= 0 && mean_diff >= observed_mean_diff){
    count <- count + 1
  }
}

  
  ######################################
  #
  # Output
  #
  ######################################
  
cat ("Observed difference of two means: ",observed_mean_diff, "\n")

if(observed_mean_diff < 0){
  cat (count, "out of", num_shuffles, "experiments had a difference of two means less than or equal to ", observed_mean_diff, "\n")
} else{
  cat(count, "out of", num_shuffles, "experiments had a difference of two means greater than or equal to ", observed_mean_diff, "\n")
}


if(observed_mean_diff < 0){
  cat ("The chance of getting a difference of two means less than or equal to ", observed_mean_diff, "is", (count /
  num_shuffles), ".\n")
}else{
  cat ("The chance of getting a difference of two means greater than or equal to ", observed_mean_diff, "is", (count /
  num_shuffles), ".\n")
}


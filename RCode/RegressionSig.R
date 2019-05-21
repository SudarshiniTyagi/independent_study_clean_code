#!/usr/bin/python

######################################
# Regression Significance Test 
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
# 
# Assuming that x is not a good predictor of y, tests to see the probability
# of getting a slope by chance alone greater than or equal to the one observed (less
# than or equal to the one observed if the observed slope is negative).
# Uses shuffling to get a distribution to compare 
# to the observed f-statistic.
# 
# Author: Manda Wilson
#
# Example of FASTA formatted input file: 
# >grp_x
# 1350 1510 1420 1210 1250 1300 1580 1310 1290 1320 1490 1200 1360
# >grp_y
# 3.6 3.8 3.7 3.3 3.9 3.4 3.8 3.7 3.5 3.4 3.8 3.0 3.1
# 
# Pseudocode:
#
# 1. Calculate the regression line for the observed values (in our example it is y' = 0.0014 x +  1.6333).  
#    a. mean_x = sum of x values / number of x values
#    b. mean_y = sum of y values / number of y values
#    c. Calculate the sum of products:
#	i. Initialize total to 0
#	ii. For each pair of x, y values:
#           I. product = (x - mean_x) * (y - mean_y) 
#           II. total += product
#    d. Calculate the sum of squares for the x values: 
#       i. Initialize total to 0 
#       ii. For each x value:
#           I. diff_sq = (x - mean_x)^2
#           II. total += diff_sq
#    e. b = sum of products / sum of squares for the x values 
#    f. a = mean_y - (b * mean_x)
#    g. Line of best fit is: y' = bx + a
#
# 2. Set a counter to 0, this will count the number of times we get a slope (b) 
#    greater than or equal to 0.0014.  Note: if you have a negative slope, count
#    the number of times you get a slope less than or equal to the original negative slope.  
#
# 3. Do the following 10,000 times:
#    a. Shuffle the y values. 
#    b. Calculate regression line on the results from step (3a), just as we did in step (1)
#    c. If the slope (b)  from step (3b) is greater than or equal to our observed slope (0.0014), 
#       increment our counter from step (2). 
#
# 3. counter / 10,000 equals the probability of getting a slope greater than
#    or equal to 0.0014, assuming x does not predict y
#
######################################

######################################
#
# Subroutines
#
######################################

  
sumofsq <- function(vals, mean){
  # the sum of squares for a group is calculated
  # by getting the sum of the squared difference
  # of each value and the mean of the group that value belongs to
  count <- length(vals)
  total <- 0
  for(i in c(1:count)){
    diff_sq <- (vals[i] - mean)^2
    total = total + diff_sq
  }
  return (total)
}
  
sumofproducts <- function(x_vals, y_vals,  mean_x, mean_y){
  count <- length(x_vals)
  total <- 0
  for(i in c(1:count)){
    product <- (x_vals[i] - mean_x) * (y_vals[i] - mean_y)
    total = total + product
  }
  return(total)
}

  
regressionline <- function(grp_x, grp_y){
  sum_x <- sum(grp_x)
  sum_y <- sum(grp_y)
  
  count_x <- length(grp_x)
  count_y <- length(grp_y)
  
  mean_x <- sum_x / count_x
  mean_y <- sum_y / count_y
  
  # get the sum of products
  sum_of_prod <- sumofproducts(grp_x, grp_y, mean_x, mean_y)
  
  # get the sum of squares for x
  sum_of_sq_x <- sumofsq(grp_x, mean_x)
  
  b <- sum_of_prod / sum_of_sq_x
  a <- mean_y - (b * mean_x)
  return (c(a, b))
  
}
    
  
  
  
  
  ######################################
  #
  # Computations
  #
  ######################################
  
library(rjson)

#Change this to wherever you are keeping the folder RCode
setwd("/Users/sudarshinityagi/PycharmProjects/independent_study_clean_code/independent_study_clean_code")

input_file <- "RCode/generated_files/Correlation.json"
# list of vectors
json_data <- fromJSON(file=input_file)
  
  grp_x <- json_data[[1]]
  grp_y <- json_data[[2]]
  
  observed_result <- regressionline(grp_x, grp_y)
  observed_a <- observed_result[1]
  observed_b <- observed_result[2]
  
  count <- 0
  num_shuffles <- 10000
  
  for(i in c(1:num_shuffles)){
    new_y_values = sample(grp_y)
    result <- regressionline(grp_x, new_y_values)
    if((observed_b >= 0 && result[2] > observed_b) || (observed_b < 0 && result[2] < observed_b)){
      count = count+1
    }
  }
  

  
  ######################################
  #
  # Output
  #
  ######################################
  
  sign <- "+"
  
  if (observed_a < 0){
    sign <- "-"
  }
    
  
  observed_a <- abs(observed_a)
  
  cat ("Line of best fit for observed data: ")
  cat ("y = ", observed_b, "x", sign, observed_a, "\n")
 if (observed_b < 0){
   cat (count, "out of", num_shuffles, "experiments had a slope less than or equal to ", observed_b, ".\n")
 }else{
   cat (count, "out of", num_shuffles, "experiments had a slope greater than or equal to ", observed_b, ".\n")
 }
 


  if(observed_b < 0){
    cat ("The chance of getting a slope less than or equal to ", observed_b, "is", (count / num_shuffles), ".\n")
  }else{
    cat ("The chance of getting a slope greater than or equal to ", observed_b, "is", (count / num_shuffles), ".\n")
  }

  
  
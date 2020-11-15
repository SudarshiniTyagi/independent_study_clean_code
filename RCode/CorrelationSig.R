######################################
# Linear Correlation Significance Test
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
#
# Assuming that x is not a good predictor of y, tests to see the probability
# of getting a r by chance alone greater than or equal to the one observed (less
# than or equal to the one observed if the observed r is negative).
# Uses shuffling to get a distribution to compare
# to the observed r.
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
# 1. Calculate r for the observed values (in our example it is .58).
#    a. mean_x = sum of x values / number of x values
#    b. mean_y = sum of y values / number of y values
#    c. Calculate the sum of products:
#       i. Initialize total to 0
#       ii. For each pair of x, y values:
#           I. product = (x - mean_x) * (y - mean_y)
#           II. total += product
#    d. Calculate the sum of squares for the x values:
#       i. Initialize total to 0
#       ii. For each x value:
#           I. diff_sq = (x - mean_x)^2
#           II. total += diff_sq
#    e. Calculate the sum of squares for the y values as we did for the x values in step (1d) 
#    f. r = sum of products / square root(sum of squares x  * sum of squares y) 
#
# 2. Set a counter to 0, this will count the number of times we get a r 
#    greater than or equal to 0.58.  Note: if you have a negative r, count
#    the number of times you get a slope less than or equal to the original negative r.
#
# 3. Do the following 10,000 times:
#    a. Shuffle the y values.
#    b. Calculate r on the results from step (3a), just as we did in step (1)
#    c. If r from step (3b) is greater than or equal to our observed r (0.58),
#       increment our counter from step (2).
#
# 3. counter / 10,000 equals the probability of getting a r greater than or equal to
#    0.58, assuming there is no difference between the groups
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


corrcoef <- function(x, y){
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  # get the sum of products
  sum_of_prod = sumofproducts(grp_x, grp_y, mean_x, mean_y)
  
  # get the sum of squares for x and y
  sum_of_sq_x = sumofsq(grp_x, mean_x)
  sum_of_sq_y = sumofsq(grp_y, mean_y)
  
  return (sum_of_prod / sqrt(sum_of_sq_x * sum_of_sq_y))
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

observed_r <- corrcoef(grp_x, grp_y)

count <- 0
num_shuffles <- 10000

for (i in c(1: num_shuffles)){
  # we want to break the relationship between the pairs, so just shuffle one group
  grp_y <- sample(grp_y)
  r <- corrcoef(grp_x, grp_y)
  if ((observed_r > 0 && r >= observed_r) || (observed_r < 0 && r <= observed_r)){
    count = count + 1
  }
}
 

######################################
#
# Output
#
######################################

cat ("Observed r: ", observed_r, "\n")

if (observed_r > 0){
  cat (count, "out of 10000 experiments had a r greater than or equal to ", observed_r, "\n")  
}else{
  cat (count, "out of 10000 experiments had a r less than or equal to ", observed_r, "\n")  
}
if(observed_r > 0){
  cat ("Probability that chance alone gave us a r greater than or equal to ",observed_r, "is ",(count / num_shuffles), "\n")
}else{
  cat ("Probability that chance alone gave us a r less than or equal to ",observed_r, "is ",(count / num_shuffles), "\n")
}

       
       

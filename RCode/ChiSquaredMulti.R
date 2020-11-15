######################################
# Multi-Variable Chi-Squared Significance Test
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
# 
# Assuming that health does not influence wealth, tests to see the probability
# of getting a chi-squared by change alone greater than or equal to the one observed.
# Uses shuffling to get a distribution to compare 
# to the observed chi-squared.
# 
# Author: Manda Wilson
#
# Example of FASTA formatted input file: 
# >sick
# 20 18 8
# >healthy
# 24 24 16
#
# Pseudocode:
#
# 1. Calculate chi-squared for the observed values (in our example it is 0.97).  
#    a. Calculate the expected counts for each category (it is row total / total * column total)
#       Example: for the sick/poor category we expected (46 / 110) * 44 = 18.48 
#    b. For each category (in this example sick/poor, sick/middle, sick/rich, healthy/poor, 
#       healthy/middle, and healthy/rich are the categories):
#       i. Subtract the expected count from the observed count
#       ii. Square the result of step (1bi)
#       iii. Divide the result of step (1bii) by the expected count
#    c. Sum the results of step (1b), this is r, our observed chi-squared value
#
# 2. Set a counter to 0, this will count the number of times we get a chi-squared
#    greater than or equal to 0.97.  
#
# 3. Do the following 10,000 times:
#    a. Create a new matrix of counts, preserving the marginals from our original matrix
#       i.  While there are more rows:
#	       I. While there are more columns:
#		      * If we ARE NOT at the last element in the row 
#             * If we ARE NOT at the last element in the column
#                - Pick a random number between 0
#                   and min(available_in_this_row, 
#                      available_in_this_column) (inclusive)
#                - Store this value in our new maxtrix in the 
#                     current row, current column position
#             * If we ARE at the last element in the column
#                - Store whatever is available_in_this_column 
#                   in the current row, current column position
#             * If we ARE at the last element in the row
#                - Store whatever is available_in_this_row 
#                   in the current row, current column position
#             * Subtract whatever is stored in the current row, 
#                current column position from the available_in_this_column
#                as well as from available_in_this_row
#
#    b. Calculate chi-squared on the results from step (3a), just as we did in step (1).
#    c. If the result from step (3b) is greater than or equal to our observed chi-squared (0.97), 
#       increment our counter from step (2). 
#
# 4. counter / 10,000 equals the probability of getting a chi-squared greater tha or equal to
#    0.97, if wealth has no influence on health

######################################
#
# Subroutines
#
######################################

# takes a list of values, plus the number of rows and columns
# For an m x n matrix, values must be ordered in a 1 dimensional array:
# row 1, columns 1 - n, row 2, columns 1 - n, ... row m, columns 1 - n
# For example, for health (sick/healthy) and wealth (poor, middle, rich),
# (2 variables with 2 and 3 categories respectively)
# If input is:
#	Poor  	Middle  	Rich
# Sick 	20 	18 	8
# Healthy 	24 	24 	16
# We expect the following 1 dimensional array
# [20, 18, 8, 24, 24, 16]
# shuffles values, preserving row and column totals
shuffle <- function(observed, num_rows, num_cols){
  # this array will store values for the first variable
  # in our example, it will store health values
  # 0 = sick, 1 = healthy
  row_vals <- c()
  # this array will store values for the second variable
  # in our example, it will store wealth values
  # 0 = poor, 1 = middle, 2 = rich
  col_vals <- c()
  for(r in c(0:num_rows)){
    for(co in c(0:num_cols)){
      count_in_cell <- observed[r * (num_cols+1) + co +1]
      # there must be a better way...
      for(i in c(1:count_in_cell)){
        row_vals <- c(row_vals, r)
        col_vals <- c(col_vals, co)
      }
    }
  }
  # now shuffle one variable (breaking any association between the two)
  # in our example we are shuffling wealth lables
  col_vals <- sample(col_vals)
  
  # reassemble counts like original one-dimensional array
  new_counts <- c()
  
  for(r in c(0:num_rows)){
    for(co in c(0:num_cols)){
      new_counts<-c(new_counts, 0)
    }
  }
  
  for(i in c(1: length(row_vals))){
    index <- (row_vals[i] * (num_cols+1)) + col_vals[i] + 1
    # use values in row_vals and col_vals to insert into this array
    new_counts[index] = new_counts[index] + 1
  }
  
  return(new_counts)
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

observed <- c()
column_totals <- c()
row_totals <- c()
library(rjson)

#Change this to wherever you are keeping the folder RCode
setwd("/Users/sudarshinityagi/PycharmProjects/independent_study_clean_code/independent_study_clean_code")

input_file <- "RCode/generated_files/chisquaredmulti.json"
# list of vectors
json_data <- fromJSON(file=input_file)
for(i in c(1:length(json_data))){
  row_totals <- c(row_totals, sum(json_data[[i]]))
}

column_totals <- json_data[[1]]+json_data[[2]]

total <- sum(json_data[[1]])+sum(json_data[[2]])
expected <- c()

# calculate expected values based on row and column totals
for (r in c(1:length(row_totals))){
  for (c in c(1:length(column_totals))){
    expected <- c(expected, ((row_totals[r] / total * column_totals[c])))
  }
}
observed <- c(json_data[[1]], json_data[[2]])

observed_chi_squared <- chisquared(expected, observed)
count <- 0
num_runs <- 10000

for (i in c(1:num_runs)){
  shuffled_observed <- shuffle(observed, (length(row_totals)-1), (length(column_totals)-1))
  chi_squared <- chisquared(expected, shuffled_observed)
  if (chi_squared >= observed_chi_squared){
    count <- count + 1
  }
}


######################################
#
# Output
#
######################################

cat ("Observed chi-squared: ", observed_chi_squared, "\n")
cat (count, "out of", num_runs, "experiments had a chi-squared greater than or equal to ",observed_chi_squared, "\n")
cat ("Probability that chance alone gave us a chi-squared greater than or equal to")
cat (observed_chi_squared, "is", (count / num_runs), "\n")

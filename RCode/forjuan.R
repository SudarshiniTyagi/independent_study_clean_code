######################################
# Coin Toss Significance Test
# From: Statistics is Easy! By Dennis Shasha and Manda Wilson
# 
# Simulates 10,000 experiments, where each experiment is 17 tosses of a fair coin.
# For each experiment we count the number of heads that occur out of the 17 tosses. 
# We then compare our observed number of heads in 17 tosses of a real coin to this 
# distribution, to determine the probability that our sample came from this assumed 
# distribution (to decide whether this coin is fair).
# 
# Author: Manda Wilson
#
# Pseudocode:
# 
# 1. Set a counter to 0, this will count the number of times we get 15 or more
#    heads out of 17 tosses.
#
# 2. Do the following 10,000 times:
#    a. Create a drawspace (a big pool of numbers we will pick from).
#       In our example, p, the probability of "success", i.e., tossing a fair coin and 
#       getting a head, equals .5, and our drawspace will be from 0 to 2000.05.  We get 
#       2000.05 from ((1 / 0.5) * 1000) + 0.05.
#    b. Do the following 17 times, counting successes as we go:
#        i. Pick randomly from the drawspace.
#        ii. If the number we drew is less or equal to p * drawspace then this was a success.
#            p * drawspace is the proportion of values in the drawspace that count as successes.
#            In our example, p * drawspace equals 1000.025, so if we drew any number less than or equal to this
#            it counts as a success (success = we drew a head).
#    c. If the number of successes from step (2b) is greater than or equal to 15, increment our counter
#       from step (1).
#
# 3. counter / 10,000 equals the probability of getting an observed number of heads greater than or equal to 15
#    in 17 tosses if the coin is fair.
#
######################################

######################################
#
# Adjustable variables
#
######################################

observed_number_of_heads = 15# You change this JUAN change this to recall
# in one case and precision in the other
number_of_tosses = 17 # You change this JUAN change this to number of patients
probability_of_head = 0.5 # JUAN change this to fraction with yes response.

######################################
#
# Subroutines
#
######################################

# p = probability of some outcome for a trial
# n = number of trials
# returns number of times outcome of probability p occurred out of n trials
applyprob <- function(p, n){
  drawspace <- ((1 / p) * 1000) + 0.05
  success <- 0
  for(j in c(1: n)){
    outcome <- sample(c(0:drawspace), 1)
    if((p*drawspace) >= outcome){
      success <- success + 1
    }
  }
  return (success)
}


######################################
#
# Computations
#
######################################

countgood = 0
number_of_bootstraps = 10000
out = c()

for(i in c(1: number_of_bootstraps)){
  out <- c(out, applyprob(probability_of_head, number_of_tosses))
}

for(ele in out){
  if (ele>=observed_number_of_heads){
    countgood <- countgood + 1
  }
}

######################################
#
# Output
#
######################################

cat(countgood, "out of", number_of_bootstraps, "times we got at least")
cat(observed_number_of_heads, "heads in", number_of_tosses, "tosses.")
cat("Probability that chance alone gave us at least", observed_number_of_heads)
cat("heads in", number_of_tosses, "tosses is", countgood / number_of_bootstraps, ".")
    

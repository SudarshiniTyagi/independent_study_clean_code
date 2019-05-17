# Function get_slope
get_slope <- function(X, Y){
  my_line <- lm(Y ~ X)
  return (my_line$coefficients[2])
}

# Function get_slope_diff
get_slope_diff <- function(X1, Y1, X2, Y2){
  slope_1 <- get_slope(X1, Y1)
  slope_2 <- get_slope(X2, Y2)
  return (slope_1 - slope_2)
}

# Function shuffle_diet_labels
shuffle_diet_labels <- function(my_data){
  new_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(new_df) <- c("weight","Time","Chick","Diet")
  for(i in c(0:22)){
    temp <- my_data[which(my_data$Time == i),]
    count_row <- nrow(temp)
    if(count_row == 0){
      next
    }else{
      temp$Diet <- sample(temp$Diet)
      new_df <- rbind(new_df, temp)
    }
  }
  return(new_df)
}

# Function get_slope_diff_count
get_slope_diff_count <- function(my_data, observed_slope_diff, 
                                 diet_1, diet_2, num_shuffles){
  count <- 0
  for(i in c(1:num_shuffles)){
    # if(i>0 && i%%20 == 0){
    #   cat("\nDone: ", i)
    # }
    
    new_df <- shuffle_diet_labels(my_data)
    
    new_df_diet_1 <- new_df[which(new_df$Diet == diet_1),]
    new_df_diet_2 <- new_df[which(new_df$Diet == diet_2),]
    X1 <- new_df_diet_1$Time
    Y1 <- new_df_diet_1$weight
    X2 <- new_df_diet_2$Time
    Y2 <- new_df_diet_2$weight
    
    slope_diff <- get_slope_diff(X1, Y1, X2, Y2)
    
    if(observed_slope_diff < 0 && slope_diff <= observed_slope_diff){
      count = count + 1
    }else if(observed_slope_diff >= 0 && slope_diff >= observed_slope_diff){
      count = count+1
    }
  }
  return(count)
}


observed_diff <- c(-1.769, -4.5828, -2.874, -2.8137, -1.105, 1.7085)
names(observed_diff) <- c("1_2", "1_3", "1_4","2_3","2_4","3_4")

# Function slope_diff_sig_test
slope_diff_sig_test <- function(my_data){
  unique_diets <- unique(my_data$Diet)
  diet_comparisons <- c("1_2", "1_3", "1_4","2_3","2_4","3_4")
  num_shuffles <- 300
  for(ele in diet_comparisons){
    observed_slope_diff <- observed_diff[names(observed_diff) == ele]
    diets <- strsplit(ele, "_")[[1]]
    diet_1 <- diets[1]
    diet_2 <- diets[2]
    temp_data <- my_data[which(my_data$Diet == diet_1 | my_data$Diet == diet_2),]
    count <- get_slope_diff_count(temp_data, observed_slope_diff, 
                                  diet_1, diet_2, num_shuffles)
    ######################################
    #
    # Output
    #
    ######################################
    cat("\n**********Diet: ", diet_1, " vs Diet: ", diet_2,"**********")
    cat ("\nObserved difference of two slopes: ", observed_slope_diff)
    cat ("\n", count, "out of", num_shuffles, "experiments had a difference of two means ")
    if(observed_slope_diff < 0){
      cat("less than or equal to ", observed_slope_diff)
    }else{
      cat("greater than or equal to ", observed_slope_diff)
    }
      
    cat ("\nThe chance of getting a difference of two means ")
    if(observed_slope_diff < 0){
      cat("less than or equal to", observed_slope_diff, " is ", (count / num_shuffles))
    }else{
      cat ("less than or equal to", observed_slope_diff, " is ", (count / num_shuffles))
    }
  }
}


slope_diff_sig_test(ChickWeight)
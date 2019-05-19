remove_missing_data_row <- function(my_data){
  my_data <- my_data[!my_data$weight == 0, ]
  return(my_data)
}

get_slope_from_data <- function(my_data){
  X <- my_data$Time
  Y <- my_data$weight
  my_line <- lm(Y ~ X)
  return (my_line$coefficients[2])
}

get_slope_from_x_y <- function(X, Y){
  my_line <- lm(Y ~ X)
  return (my_line$coefficients[2])
}

get_slope_diff_from_data <- function(data_1, data_2){
  X1 <- data_1$Time
  Y1 <- data_1$weight
  X2 <- data_2$Time
  Y2 <- data_2$weight
  slope_1 <- get_slope_from_x_y(X1, Y1)
  slope_2 <- get_slope_from_x_y(X2, Y2)
  return (slope_1 - slope_2)
}

find_mean <- function(my_data){
  count <- 0
  for(ele in my_data){
    if(ele == 0){
      next
    }else{
      count = count+1
    }
  }
  return(sum(my_data)/count)
}

find_median <- function(my_data){
  return(median(my_data))
}

replace <- function(my_data, replacement){
  my_data[my_data==0] <- replacement
  return(my_data)
}

replace_missing_values <- function(my_data, replacement_parameter){
  new_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(new_df) <- c("weight","Time","Chick","Diet")
  for(i in c(0:22)){
    temp <- my_data[which(my_data$Time == i),]
    count_row <- nrow(temp)
    if(count_row == 0){
      next
    }else{
      if(replacement_parameter == "mean"){
        replacement <- find_mean(temp$weight)
      }else if(replacement_parameter == "median"){
        replacement <- find_median(temp$weight)
      }
      temp$weight <- replace(temp$weight, replacement)
      new_df <- rbind(new_df, temp)
      }
  }
  return(new_df)
}

get_new_slope <- function(my_data, replacement_parameter){
  if(replacement_parameter == "remove_missing"){
    temp <- remove_missing_data_row(my_data)
  }else{
    temp <- replace_missing_values(my_data, replacement_parameter)
  }
  slope <- get_slope_from_data(temp)
  return(slope)
}

get_data_with_missing_values <- function(my_data, portion_to_remove){
  my_copy <- data.frame(my_data)
  rows_to_remove <- floor(portion_to_remove * nrow(my_data))
  my_copy[sample(nrow(my_copy), rows_to_remove), ]$weight <- 0
  return(my_copy)
}

shuffle_missing_labels <- function(my_data){
  new_df <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(new_df) <- c("weight","Time","Chick","Diet", "missing")
  for(i in c(0:22)){
    temp <- my_data[which(my_data$Time == i),]
    count_row <- nrow(temp)
    if(count_row == 0){
      next
    }else{
      temp$missing <- sample(temp$missing)
      new_df <- rbind(new_df, temp)
    }
  }
  return(new_df)
}

# Function get_slope_diff_count
get_slope_diff_count <- function(my_data, observed_slope_diff, 
                                 portion_1, portion_2, num_shuffles){
  count <- 0
  for(i in c(1:num_shuffles)){
    # if(i>0 && i%%20 == 0){
    #   cat("\nDone: ", i)
    # }
    
    new_df <- shuffle_missing_labels(my_data)
    
    new_df_portion_1 <- new_df[which(new_df$missing == portion_1),]
    new_df_portion_2 <- new_df[which(new_df$missing == portion_2),]
    
    slope_diff <- get_slope_diff_from_data(new_df_portion_1, new_df_portion_2)
    
    if(observed_slope_diff < 0 && slope_diff < observed_slope_diff){
      count = count + 1
    }else if(observed_slope_diff >= 0 && slope_diff >= observed_slope_diff){
      count = count+1
    }
  }
  return(count)
}


significance_test_missing_data_slope <- function(my_data, portion, replacement_parameter){
  num_shuffles <- 100
  portion_1 <- 0.0
  portion_2 <- portion
  
  my_data_with_missing_values <- get_data_with_missing_values(my_data, 
                                                              portion_to_remove = portion)
  my_data_with_missing_values$missing <- rep(portion, nrow(my_data_with_missing_values))
  
  my_data_original <- data.frame(my_data)
  my_data_original$missing <- rep(0.0, nrow(my_data_original))
  
  observed_slope_diff <- get_new_slope(my_data_with_missing_values, 
                                       replacement_parameter) - get_slope_from_data(my_data_original)
  
  my_data_combined <- rbind(my_data_original, my_data_with_missing_values)
  
  count <- get_slope_diff_count(my_data_combined, observed_slope_diff, 
                                portion_1, portion_2, num_shuffles)
  ######################################
  #
  # Output
  #
  ######################################
  
  cat("\n\n********** Missing ", portion*100, "% of data****************")
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

portion_to_remove = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
cat("\n\n\n############ Removing rows with missing values ############")
for(portion in portion_to_remove){
  significance_test_missing_data_slope(ChickWeight, portion, "remove_missing")
}


cat("\n\n\n############ Replacing missing values with the mean ############")
for(portion in portion_to_remove){
  significance_test_missing_data_slope(ChickWeight, portion, "mean")
}


cat("\n\n\n############ Replacing missing values with the median ############")
for(portion in portion_to_remove){
  significance_test_missing_data_slope(ChickWeight, portion, "median")
}
  




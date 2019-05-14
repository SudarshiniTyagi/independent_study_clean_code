my_bootstrap <- function(x){
  samp_x <- c()
  for (i in c(1:length(x))){
    random_pick <- sample(x, 1)
    samp_x <- c(samp_x, random_pick)
  }
  return(samp_x)
}

my_bootstrap_data <- function(my_data){
  new_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(new_df) <- c("weight","Time","Chick","Diet")
  for(i in c(1:22)){
  temp <- my_data[which(my_data$Time == i),]
    count_row <- nrow(temp)
    if(count_row == 0){
      next
    }else{
      temp$weight <- my_bootstrap(temp$weight)
      new_df <- rbind(new_df, temp)
    }
  }
  return(new_df)
}

get_slope <- function(X, Y){
  my_line <- lm(Y ~ X)
  return (my_line$coefficients[2])
}


get_slopes <- function(my_data){
  unique_diets <- unique(ChickWeight$Diet)
  num_shuffles = 100
  slope_out_all_diets <- list()
  for(diet in unique_diets){
    slope_out <- c()
    for(i in c(1:num_shuffles)){
      if(i%%10 == 0){
        cat("\nDone: ", i)
      }
      df_this_diet <- ChickWeight[which(ChickWeight$Diet == diet),]
      bootstraped_df <- my_bootstrap_data(df_this_diet)
      slope <- get_slope(bootstraped_df$Time, bootstraped_df$weight)
      slope_out <- c(slope_out, slope)
    }
    slope_out_all_diets[diet] <- list(slope_out)
  }
  return(slope_out_all_diets)
}

observed_slope = c(6.8417972, 8.60913629, 11.42287097, 9.71436556)

slope_out <- get_slopes(ChickWeight)

conf_interval = 0.9

for(i in c(1:4)){
  diet <- i
  slopes <- slope_out[[i]][order(slope_out[[i]])]
  tails = (1-conf_interval) /2
  lower_bound <- ceiling(100*tails)
  upper_bound <- floor(100 * (1-tails))
  cat("\n**********Diet: ", diet,"**********")
  cat ("\nObserved slope: ",observed_slope[i])
  cat ("\nWe have", conf_interval * 100, "% confidence that the true slope is between: ",slopes[lower_bound], " and ", slopes[upper_bound])
}
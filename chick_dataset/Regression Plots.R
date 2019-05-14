unique_diets <- unique(ChickWeight$Diet)

for (ele in unique_diets){
  df_this_diet <- ChickWeight[which(ChickWeight$Diet == ele),]
  Time <- df_this_diet$Time
  Weight <- df_this_diet$weight
  title <- paste("Diet: ", ele, sep=" ")
  plot(Weight ~ Time, main = title)
  my_line <- lm(Weight ~ Time)
  cat("Diet: ", ele)
  print(my_line)
  abline(my_line)
}
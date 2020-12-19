# This script is used to generate the cleaned data for our application.

house_data <- read.csv("data.csv")

sort(apply(house_data, 2, function(x) sum(is.na(x))), decreasing = TRUE)[1:5]
 
clean_house_data <- house_data[complete.cases(house_data),]

write.csv(clean_house_data, "InteractiveLeastSquares/house.csv", row.names = FALSE)
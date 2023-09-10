#HW2
data <- read.csv("C:/Users/faiya/OneDrive - Texas Tech University/Texas tech course/fall 23/software analytics//HW1/auto.csv")

#the mean and standard deviation of 'mpg'

mean_mpg <- mean(data$mpg)
std_dev_mpg <- sd(data$mpg)

#the mean and standard deviation of 'weight'
mean_weight <- mean(data$weight)
std_dev_weight <- sd(data$weight)

# Print the results
cat("Mean mpg:", mean_mpg, "\n")
cat("Standard Deviation mpg:", std_dev_mpg, "\n")
cat("Mean weight:", mean_weight, "\n")
cat("Standard Deviation weight:", std_dev_weight, "\n")

#horsepower
mean_horsepower <- mean(data$horsepower)
std_dev_horsepower <- sd(data$horsepower)

get_outlier <- function(column, mean_value, std_dev_value) {
  outliers <- abs(column - mean_value) >= 3 * std_dev_value
  return(outliers)
}

# Identify outliers in 'mpg' and 'weight' and horsepower
mpg_outliers <- get_outlier(data$mpg, mean_mpg, std_dev_mpg)
weight_outliers <- get_outlier(data$weight, mean_weight, std_dev_weight)
horsepower_outliers <- get_outlier(data$horsepower, mean_horsepower, std_dev_horsepower)


# names of the cars that are outliers in each category
outliers_weight_cars <- data$name[weight_outliers]
outliers_mpg_cars <- data$name[mpg_outliers]
outliers_horsepower_cars <- data$name[horsepower_outliers]



# Print the names of the cars that are outliers in each category
cat("Outliers on Weight:")
if (length(outliers_weight_cars) == 0) {
  cat(" No factors\n")
} else {
  cat(outliers_weight_cars, "\n")
}

cat("Outliers on MPG:")
if (length(outliers_mpg_cars) == 0) {
  cat(" No factors\n")
} else {
  cat(outliers_mpg_cars, "\n")
}
cat("Outliers on horsepower:")
if (length(outliers_horsepower_cars) == 0) {
  cat(" No factors\n")
} else {
  cat(outliers_horsepower_cars, "\n")
}
# Calculate the PMF
calculate_pmf <- function(data) {
  pmf <- table(data) / length(data)
  return(pmf)
}

#cylinder 
cylinders_data <- data$cylinders
#origin
origin_data <- data$origin
# Calculate the PMF using the function
pmf_cylinders <- calculate_pmf(cylinders_data)
pmf_origin <-calculate_pmf(origin_data)

# Print the PMF
cat("Probability Mass Function (PMF) for 'cylinders':\n")
print(pmf_cylinders)
cat("Probability Mass Function (PMF) for 'origins':\n")
print(pmf_origin)
#q4



# Create a histogram for 'mpg'
hist(data$mpg, main = "Histogram of MPG", xlab = "MPG", ylab = "Frequency", col = "lightblue")

# Create a histogram for 'weight'
hist(data$weight, main = "Histogram of Weight", xlab = "Weight", ylab = "Frequency", col = "lightblue")


#HW3

data <- read.csv("C:/Users/faiya/OneDrive - Texas Tech University/Texas tech course/fall 23/software analytics//HW1/auto.csv")
correlation <- cor(data[, c('cylinders', 'displacement', 'horsepower', 'weight')])
print(correlation)

# Calculate the correlation
q2 <- cor(data[, c('acceleration', 'horsepower')])
print(q2)


#Calculate the correlation
q3 <- cor(data[, c('model_year', 'mpg')])
print(q3)

correlation_q3 <- cor(data[, c('model_year', 'cylinders', 'displacement', 'horsepower', 'weight')])
print(correlation_q3)
#q4
# Perform ANOVA
anova_result <- aov(mpg ~ origin, data = data)

# Print ANOVA summary
summary(anova_result)

# Create a boxplot
boxplot(data$mpg ~ data$origin, xlab = "Origin", ylab = "MPG", main = "Boxplot of MPG by Origin")

library(readr)
pde_data <- read.csv("C:/Users/faiya/OneDrive - Texas Tech University/Texas tech course/fall 23/software analytics/HW6/PDE.csv")
jdt_data <- read.csv("C:/Users/faiya/OneDrive - Texas Tech University/Texas tech course/fall 23/software analytics/HW6/JDT.csv")

#pde
pde_correlation <- cor(pde_data[, "bug"], pde_data[, -1], method = "spearman")
# Find the variable with the strongest absolute correlation to bug
abs_correlation <- abs(pde_correlation)
pde_max_corr_var <- names(pde_correlation)[which(abs_correlation == max(abs_correlation[-1]))]
pde_max_corr <- max(abs_correlation[-1])  # Excluding self-correlation with "bug"
cat("PDE Dataset - Spearman Correlation Matrix:\n")
print(pde_correlation)
cat("\nThe variable with the strongest correlation to 'bug' in the PDE dataset is:", pde_max_corr_var, "with a correlation of", pde_max_corr)

#jdt

jdt_correlation <- cor(jdt_data[, "bug"], jdt_data[, -1], method = "spearman")
# Find the variable with the strongest absolute correlation to bug in the JDT dataset
abs_jdt_correlation <- abs(jdt_correlation)
jdt_max_corr_var <- names(jdt_correlation)[which(abs_jdt_correlation == max(abs_jdt_correlation[-1]))]
jdt_max_corr <- max(abs_jdt_correlation[-1])  # Excluding self-correlation with "bug"

# Print the results for the JDT dataset
cat("\nJDT Dataset - Spearman Correlation Matrix:\n")
print(jdt_correlation)

cat("\nThe variable with the strongest correlation to 'bug' in the JDT dataset is:", jdt_max_corr_var, "with a correlation of", jdt_max_corr)


#q2
# Count the number of defect-free classes
defect_free_classes <- sum(jdt_data$bug == 0)

# Calculate the total number of classes
total_classes <- nrow(jdt_data)

# Calculate the percentage of defect-free classes
percentage_defect_free <- (defect_free_classes / total_classes) * 100

# Print the result
cat("Percentage of defect-free classes in the JDT dataset:", percentage_defect_free, "%\n")

# Sort the dataset by the number of bugs in descending order
sorted_jdt_data <- jdt_data[order(-jdt_data$bug), ]

# Calculate the number of classes in the top 10% and top 20%
top_10_percent <- round(0.10 * total_classes)
top_20_percent <- round(0.20 * total_classes)

# Select the top 10% and top 20% of classes
top_10_classes <- sorted_jdt_data[1:top_10_percent, ]
top_20_classes <- sorted_jdt_data[1:top_20_percent, ]

# Calculate the total number of bugs in all classes
total_bugs <- sum(jdt_data$bug)

# Calculate the total number of bugs in the top 10% and top 20% most defect-prone classes
bugs_in_top_10_percent <- sum(top_10_classes$bug)
bugs_in_top_20_percent <- sum(top_20_classes$bug)

# Calculate the percentages
percentage_bugs_in_top_10_percent <- (bugs_in_top_10_percent / total_bugs) * 100
percentage_bugs_in_top_20_percent <- (bugs_in_top_20_percent / total_bugs) * 100

# Print the results
cat("Percentage of bugs found in the top 10% most defect-prone classes:", percentage_bugs_in_top_10_percent, "%\n")
cat("Percentage of bugs found in the top 20% most defect-prone classes:", percentage_bugs_in_top_20_percent, "%\n")
#pde
# Percentage of Defect-Free Classes in the PDE Dataset
defect_free_classes_pde <- sum(pde_data$bug == 0)
total_classes_pde <- nrow(pde_data)
percentage_defect_free_pde <- (defect_free_classes_pde / total_classes_pde) * 100

cat("Percentage of defect-free classes in the PDE dataset:", percentage_defect_free_pde, "%\n")

# Percentage of Bugs in Top 10% and Top 20% Most Defect-Prone Classes in the PDE Dataset
sorted_pde_data <- pde_data[order(-pde_data$bug), ]
total_bugs_pde <- sum(pde_data$bug)

top_10_percent_pde <- round(0.10 * total_classes_pde)
top_20_percent_pde <- round(0.20 * total_classes_pde)

top_10_classes_pde <- sorted_pde_data[1:top_10_percent_pde, ]
top_20_classes_pde <- sorted_pde_data[1:top_20_percent_pde, ]

bugs_in_top_10_percent_pde <- sum(top_10_classes_pde$bug)
bugs_in_top_20_percent_pde <- sum(top_20_classes_pde$bug)

percentage_bugs_in_top_10_percent_pde <- (bugs_in_top_10_percent_pde / total_bugs_pde) * 100
percentage_bugs_in_top_20_percent_pde <- (bugs_in_top_20_percent_pde / total_bugs_pde) * 100

cat("Percentage of bugs found in the top 10% most defect-prone classes in the PDE dataset:", percentage_bugs_in_top_10_percent_pde, "%\n")
cat("Percentage of bugs found in the top 20% most defect-prone classes in the PDE dataset:", percentage_bugs_in_top_20_percent_pde, "%\n")


#q3
# Create a boxplot for the JDT dataset
boxplot(jdt_data$bug, col = "lightblue", main = "JDT Dataset - Bug Distribution", ylab = "Number of Bugs")

# Create a boxplot for the PDE dataset
boxplot(pde_data$bug, col = "lightgreen", main = "PDE Dataset - Bug Distribution", ylab = "Number of Bugs")


# Perform a t-test to compare the bug distributions between JDT and PDE
t_test_result <- t.test(jdt_data$bug, pde_data$bug)

# Print the t-test result
cat("T-test Result:\n")
print(t_test_result)


#q4
jdt_data$defect_density <- (jdt_data$bug + jdt_data$bf) / (jdt_data$loc / 1000)
pde_data$defect_density <- (pde_data$bug + pde_data$bf) / (pde_data$loc / 1000)

# Identify rows with infinite defect density in the JDT dataset
jdt_outliers <- which(!is.finite(jdt_data$defect_density))

# Identify rows with infinite defect density in the PDE dataset
pde_outliers <- which(!is.finite(pde_data$defect_density))

# Remove rows with infinite defect density in the JDT dataset
jdt_data <- jdt_data[-jdt_outliers, ]

# Remove rows with infinite defect density in the PDE dataset
pde_data <- pde_data[-pde_outliers, ]

# Create a boxplot for the JDT dataset's defect density
boxplot(jdt_data$defect_density, col = "lightblue", main = "JDT Dataset - Defect Density", ylab = "Defect Density (per KLOC)")

# Create a boxplot for the PDE dataset's defect density
boxplot(pde_data$defect_density, col = "lightgreen", main = "PDE Dataset - Defect Density", ylab = "Defect Density (per KLOC)")
# Perform a t-test to compare the defect density distributions between JDT and PDE
t_test_resultpo <- t.test(jdt_data$defect_density, pde_data$defect_density)

# Print the t-test result
cat("T-test Result:\n")
print(t_test_resultpo)



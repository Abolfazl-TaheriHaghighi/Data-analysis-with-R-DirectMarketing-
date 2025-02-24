# data call
DirectMarketing <- read.csv("DirectMarketing.csv", header = T)


#1- Summary of changes given with summary function
summary(DirectMarketing)










# 2- The percentage of female customers
num_females <- sum(DirectMarketing$Gender == "Female")
total_customers <- nrow(DirectMarketing)
percentage_females <- (num_females / total_customers) * 100
percentage_females 
print(paste("The number of female customers is equal to:",percentage_females, "%"))





# 3- The percentage of married men with salaries above 50 thousand dollars
num_married_males <- sum(DirectMarketing$Gender == "Male" & DirectMarketing$Married == "Married" & DirectMarketing$Salary > 50000)
total_married_males <- sum(DirectMarketing$Gender == "Male" & DirectMarketing$Married == "Married")
percentage_married_males <- (num_married_males / total_married_males) * 100
percentage_married_males
print(paste("The percentage of married men with salaries above 50 thousand dollars is equal to:", percentage_married_males, "%"))




# 4- Frequency chart of customers based on age category
library(ggplot2)
age_counts <- as.data.frame(table(DirectMarketing$Age))
age_counts <- age_counts[order(age_counts$Freq), ] # Sort by frequency

# Draw a diagram
ggplot(age_counts, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age Category", y = "Count") +
  ylim(0, 600) +
  theme_minimal()


# 5-Draw a cumulative chart
age_gender_counts <- as.data.frame(table(DirectMarketing$Age, DirectMarketing$Gender))
names(age_gender_counts) <- c("Age", "Gender", "Count")
age_gender_counts <- age_gender_counts[order(age_gender_counts$Count), ] # Sort by frequency

# Draw a cumulative chart
ggplot(age_gender_counts, aes(x = reorder(Age, Count), y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution by Age and Gender", x = "Age Category", y = "Count") +
  ylim(0, 600) +
  scale_fill_manual(values = c("Female" = "green", "Male" = "red")) +
  theme_minimal()

# 6- Density diagram and exponential distribution fit
ggplot(DirectMarketing, aes(x = AmountSpent)) +
  geom_density(fill = "blue", alpha = 0.2) +
  stat_function(fun = dexp, args = list(rate = 1 / mean(DirectMarketing$AmountSpent)), color = "red", linetype = "dashed") +
  labs(title = "Density Plot of Amount Spent with Exponential Fit", x = "Amount Spent", y = "Density") +
  theme_minimal()


# 7- Box diagram of the relationship between the number of catalogs received and the amount of purchases
ggplot(DirectMarketing, aes(x = as.factor(Catalogs), y = AmountSpent)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Box Plot of Amount Spent by Number of Catalogs Received", x = "Number of Catalogs", y = "Amount Spent") +
  theme_minimal()


# 8- Scatter diagram of the relationship between the level of income and the amount of purchases
ggplot(DirectMarketing, aes(x = Salary, y = AmountSpent)) +
  geom_point(color = "navyblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", size = 2) +
  labs(title = "Scatter Plot of Salary vs Amount Spent", x = "Salary", y = "Amount Spent") +
  theme_minimal()


# 9- Examining the effect of gender on the amount of purchase
# A) Box diagram of purchase rate by gender
ggplot(DirectMarketing, aes(x = Gender, y = AmountSpent, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot of Amount Spent by Gender", x = "Gender", y = "Amount Spent") +
  theme_minimal()

# Calculate the average amount of purchases based on gender
tapply(DirectMarketing$AmountSpent, DirectMarketing$Gender, mean)
# B) Examining the effect of the wage level on the amount of purchases
ggplot(DirectMarketing, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot of Salary by Gender", x = "Gender", y = "Salary") +
  theme_minimal()

# Comparison of the average salary between the two sexes
tapply(DirectMarketing$Salary, DirectMarketing$Gender, mean)

# C) Comparison of purchases between income groups
median_salary <- median(DirectMarketing$Salary)
DirectMarketing$IncomeGroup <- ifelse(DirectMarketing$Salary > median_salary, "High Income", "Low Income")

ggplot(DirectMarketing, aes(x = Gender, y = AmountSpent, fill = IncomeGroup)) +
  geom_boxplot() +
  labs(title = "Box Plot of Amount Spent by Gender and Income Group", x = "Gender", y = "Amount Spent") +
  theme_minimal()


#Abolfazl Taheri Haghighi
#Bachelor's student of statistics at Fasa University




#Libraraies used
library("reader")
library("dplyr")
library("ggplot2")


#C:/Users/user/Downloads/grc.csv
data<-read.csv("C:/Users/user/Downloads/grc.csv")

#Two duplicated records
sum(duplicated(data))

#Both functions remove duplicts
data<-unique(data)
data<-distinct(data)

#All are true
is.integer(data$count)
is.integer(data$total)
is.integer(data$rnd)
is.integer(data$age)

#No NA values
sum(is.na(data))

#Outliers only exist in count column
#but they do not seem like errors
#highest value is 32
boxplot(data$count)
boxplot(data$total)
boxplot(data$rnd)
boxplot(data$age)

#Compare cash and credit totals
c <- table(data$paymentType)

# Calculate percentages
percantage <- paste0(round(100 * c / sum(c), digits = 2), "%")

# Define colors for the pie chart
pie_colors <- c("#0072B2", "#009E73")

# Create pie chart with improved labels and colors
pie(c, labels = percantage,
    main = "Compare cash and credit totals",
    col = pie_colors)

# Add legend
legend("bottomright", legend = c("Cash","Credit"), fill = pie_colors)

# Group by age and calculate sum of total spending for each age group
age_spending <- aggregate(total ~ age, data = data, FUN = sum)

# Create a barplot
ggplot(age_spending, aes(x = age, y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Spending by Age Group",
       x = "Age Group",
       y = "Total Spending")

#Show each city total spending and arrange it by total descending.
# Group by city and calculate sum of total spending for each city
city_spending <- aggregate(total ~ city, data = data, FUN = sum)

# Arrange cities by total spending in descending order
city_spending <- city_spending[order(-city_spending$total), ]

# Create a barplot
ggplot(city_spending, aes(x = reorder(city, -total), y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Spending by City (Descending)",
       x = "City",
       y = "Total Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels to display in full

# Create a histogram
ggplot(data, aes(x = total)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Spending",
       x = "Total Spending",
       y = "Frequency")
# Libraraies used
library(arules)
library(reader)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(shiny)

# UI
ui <- fluidPage(
  titlePanel("GRC-grocery"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose Dataset", accept=".csv"),
      numericInput("n_clusters", "Number of Clusters", value = 3),
      sliderInput("min_support", "Minimum Support (%)", min = 0.1, max = 1, value = 0.2),
      sliderInput("min_confidence", "Minimum Confidence (%)", min = 0.1, max = 1, value = 0.5)
    ),
    mainPanel(
      h3("Results"),
      verbatimTextOutput("output")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$output <- reactive({
    if (!is.null(input$dataset)) {
      data <- read.csv(input$dataset$datapath)
      model <- apriori(data, minlen = 2, sampsize = input$min_support)
      rules <- filterRules(model, minlen = 2, metric = "confidence", threshold = input$min_confidence)
      summary <- paste(
        "Number of Rules:", nrow(rules),
        "\nSample Rules:",
        paste0(sep = "\n  - ", rules[, c("lhs", "rhs", "support", "confidence")][1:3,]),
        sep = "\n"
      )
      return(summary)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)






# C:/Users/user/Downloads/grc.csv
# readline("input path to csv file: ")
data<-read.csv("C:/Users/bodyq/Documents/grc.csv")

# Remove duplicts if exist
if (sum(duplicated(data)) > 0){
  data<-distinct(data) # Two duplicated records removed
}

# Checks if data structure is correct and corrects them
# All are false. No changes made.
if (!is.integer(data$total)){
  as.integer(data$total)
}
if (!is.integer(data$count)){
  as.integer(data$count)
}
if (!is.integer(data$rnd)){
  as.integer(data$rnd)
}
if (!is.integer(data$age)){
  as.integer(data$age)
}

# Checks if there are missing values (NA) and removes them
if (sum(is.na(data)) > 0){
  na.omit(data) # No NA values
}

# Checks if outliers exist and removes them

# Outliers only exist in count column
# but they do not seem like errors
# highest value is 32 so i will keep them
outlierT = boxplot(data$total)$out
outlierR = boxplot(data$rnd)$out
outlierA = boxplot(data$age)$out
if (sum(outlierT) > 0){
  data <- data[-which(data$total%in%outlierT)]
}
if (sum(outlierR) > 0){
  data <- data[-which(data$total%in%outlierR)]
}
if (sum(outlierA) > 0){
  data <- data[-which(data$total%in%outlierA)]
}

# Compare cash and credit totals
payment_counts <- table(data$paymentType)
total_payments <- sum(payment_counts)

# Calculate percentages
payment_percentages <- round(100 * payment_counts / total_payments, digits = 2)

# Create a data frame for plotting
payment_data <- data.frame(paymentType = names(payment_counts),
                           count = as.numeric(payment_counts),
                           percentage = payment_percentages)

# Define colors for the pie chart
pie_colors <- c("#0072B2", "#009E73")

# Create pie chart with improved labels and colors
pie_chart <- ggplot(payment_data, aes(x = "", y = count, fill = paymentType, label = paste0(paymentType, ": ", round(count / sum(count) * 100, 2), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Comparison of Cash and Credit Payments",
       fill = "Payment Type") +
  scale_fill_manual(values = pie_colors) +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(position = position_stack(vjust = 0.5))

# Group by age and calculate sum of total spending for each age group
ageSpending <- aggregate(total ~ age, data = data, FUN = sum)

# Create a new dataframe with reordered levels of the age variable
ageSpending <- within(data, age <- factor(age, levels = ageSpending$age))

# Create a barplot using the reordered_data dataframe
bar_plot_age <- ggplot(ageSpending, aes(x = age, y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Spending by Age Group",
       x = "Age Group",
       y = "Total Spending")

# Show each city total spending and arrange it by total descending
# Group by city and calculate sum of total spending for each city
citySpending <- aggregate(total ~ city, data = data, FUN = sum)

# Create a barplot
ggplot_city <- ggplot(citySpending, aes(x = reorder(city, -total), y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Spending by City (Descending)",
       x = "City",
       y = "Total Spending") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Format y-axis labels to display in full and show numbers every 500000
  scale_y_continuous(labels = scales::comma, breaks = seq(0, max(citySpending$total), by = 500000))

# Display the distribution of total spending
# Create a histogram
histogram <- ggplot(data, aes(x = total)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Spending",
       x = "Total Spending",
       y = "Frequency")

# Puts all previous plots in one dashboard
# Arrange the plots into a 2 by 2 grid
grid.arrange(pie_chart, bar_plot_age, ggplot_city, histogram,
             ncol = 2, nrow = 2,
             top = "Dashboard")

# Function to perform kmeans clustering
perform_clustering <- function(data, num_clusters) {
  
  # Group by customer and age, then calculate sum of total spending 
  customer_spending <- aggregate(total ~ customer + age, data = data, FUN = sum)
  
  # Perform kmeans clustering on total and age
  kmeans_result <- kmeans(customer_spending[, c("total", "age")], centers = num_clusters)
  
  # Add cluster numbers to the original data
  customer_spending$cluster <- kmeans_result$cluster
  
  return(customer_spending)
}

# Read the number of clusters from user input
num_clusters <- as.integer(2)

# Check if the input is within the specified range
if (num_clusters < 2 || num_clusters > 4) {
  stop("Number of clusters must be between 2 and 4.")
}

# Perform kmeans clustering
customerClusters <- perform_clustering(data, num_clusters)

# Function to generate association rules
generate_association_rules <- function(items_column, min_support, min_confidence) {
  # Split the items into a list of transactions
  transactions_list <- strsplit(items_column, split = ",")
  
  # Convert the list of transactions into a transactions object
  transactions <- as(transactions_list, "transactions")
  
  # Mine association rules
  rules <- apriori(transactions, parameter = list(support = min_support, confidence = min_confidence))
  
  return(rules)
}

# Read the user input for minimum support and confidence
min_support <- as.numeric(0.1)
min_confidence <- as.numeric(0.5)

# Check if the input is within the specified range
if (min_support < 0.001 || min_support > 1 || min_confidence < 0.001 || min_confidence > 1) {
  stop("Minimum support and confidence must be between 0.001 and 1.")
}

# Assuming 'data' is your dataframe containing the 'items' column
# Generate association rules
association_rules <- generate_association_rules(data$items, min_support, min_confidence)

# Print the association rules
print(association_rules)

# Inspect the association rules
inspect(association_rules)



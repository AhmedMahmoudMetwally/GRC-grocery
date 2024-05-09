library(shiny)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Customer Clustering"),
  sidebarLayout(
    sidebarPanel(
      numericInput("clusters", "Number of Clusters (2-4):", min = 2, max = 4, value = 2)
    ),
    mainPanel(
      tableOutput("cluster_table")
    )
  )
)

# Server
server <- function(input, output) {
  
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
  
  # Reactive expression for clustering
  clustered_data <- reactive({
    perform_clustering(data, input$clusters)
  })
  
  # Output cluster table
  output$cluster_table <- renderTable({
    clustered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

   




library(shiny)
library(arules)

# Define the UI
ui <- fluidPage(
  titlePanel("Association Rule Generation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("min_support", "Minimum Support:", value = 0.001, min = 0, max = 1, step = 0.001),
      numericInput("min_confidence", "Minimum Confidence:", value = 0.5, min = 0, max = 1, step = 0.01),
      actionButton("generate_rules", "Generate Rules")
    ),
    mainPanel(
      tableOutput("rules_table")
    )
  )
)



library(shiny)
library(arules)

# Define the UI
ui <- fluidPage(
  titlePanel("Association Rule Generation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("min_support", "Minimum Support:", value = 0.001, min = 0, max = 1, step = 0.001),
      numericInput("min_confidence", "Minimum Confidence:", value = 0.5, min = 0, max = 1, step = 0.01),
      actionButton("generate_rules", "Generate Rules")
    ),
    mainPanel(
      tableOutput("rules_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
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
  
  # Reactive expression to generate rules when the button is clicked
  association_rules <- eventReactive(input$generate_rules, {
    generate_association_rules(data$items, input$min_support, input$min_confidence)
  })
  
  # Output the generated association rules as a table
  output$rules_table <- renderTable({
    if (is.null(association_rules())) {
      return(NULL)
    } else {
      inspect(association_rules())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


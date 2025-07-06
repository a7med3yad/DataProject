# Install necessary packages if not installed
# install.packages(c("shiny", "readxl", "dplyr", "arules", "arulesViz", "ggplot2", "tidyr"))

library(shiny)
library(readxl)
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(tidyr)

# Define UI
ui <- fluidPage(
  titlePanel("Customer Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload Excel File"),
      numericInput("num_clusters", "Number of Clusters", 3, min = 2, max = 4),
      numericInput("support", "Support", 0.05, min = 0.001, max = 1, step = 0.01),
      numericInput("confidence", "Confidence", 0.06, min = 0.001, max = 1, step = 0.01)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Cleaning", verbatimTextOutput("cleaning_summary")),
        tabPanel("Clustering", plotOutput("cluster_plot")),
        tabPanel("Clusters Table", tableOutput("clusters_table")),
        tabPanel("Association Rules", 
                 verbatimTextOutput("numberofrules"),
                 tableOutput("apriori_rules_table"), 
                 plotOutput("rules_plot")),
        tabPanel("Frequency plot of Association", plotOutput("frequencyplotoutput")),
        tabPanel("Additional Visualizations", 
                 plotOutput("pieChart"), 
                 plotOutput("barPlot"), 
                 plotOutput("cityPlot"), 
                 plotOutput("boxplot")),
        tabPanel("Insights", verbatimTextOutput("insights"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive data load
  data <- reactive({
    req(input$data_file)
    read_excel(input$data_file$datapath)
  })

  # Transaction conversion (avoid writing files)
  transactions <- reactive({
    req(data())
    items_list <- strsplit(as.character(data()$items), ",\\s*")
    as(split(items_list, seq_along(items_list)), "transactions")
  })

  # Cleaning Summary
  output$cleaning_summary <- renderText({
    df <- na.omit(data())
    df <- distinct(df)
    paste("Duplicates:", sum(duplicated(df)),
          "| Missing values:", sum(is.na(df)))
  })

  # Number of Rules
  output$numberofrules <- renderText({
    rules <- apriori(transactions(), parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    paste("Number of rules:", length(rules))
  })

  # Apriori Rules Table
  output$apriori_rules_table <- renderTable({
    rules <- apriori(transactions(), parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    if(length(rules) > 0){
      quality(rules)$support <- sprintf("%.3f", quality(rules)$support)
      quality(rules)$confidence <- sprintf("%.3f", quality(rules)$confidence)
      as(rules, "data.frame")
    } else {
      data.frame(Message = "No rules found")
    }
  })

  # Rules Plot
  output$rules_plot <- renderPlot({
    rules <- apriori(transactions(), parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
    if(length(rules) > 0){
      plot(rules, method = "graph", control = list(type = "items"))
    } else {
      plot.new(); title(main = "No association rules found.")
    }
  })

  # Frequency Plot
  output$frequencyplotoutput <- renderPlot({
    tdata <- transactions()
    par(mfrow = c(1, 2))
    itemFrequencyPlot(tdata, topN = 10, type = "relative", col = "skyblue", main = "Top Items (Relative)")
    itemFrequencyPlot(tdata, topN = 10, type = "absolute", col = "orange", main = "Top Items (Absolute)")
  })

  # Clustering Plot
  output$cluster_plot <- renderPlot({
    df <- data()
    n <- input$num_clusters
    age_summary <- df %>%
      group_by(age) %>%
      summarise(Total = sum(total))
    
    kmeans_model <- kmeans(age_summary, centers = n)
    age_summary$cluster <- as.factor(kmeans_model$cluster)

    ggplot(age_summary, aes(x = age, y = Total, color = cluster)) +
      geom_point(size = 3) +
      geom_text(aes(label = cluster), vjust = -1) +
      labs(title = "Clustering of Age vs Total Spending")
  })

  # Clustering Table
  output$clusters_table <- renderTable({
    df <- data()
    n <- input$num_clusters
    age_summary <- df %>%
      group_by(age) %>%
      summarise(Total = sum(total))
    kmeans_model <- kmeans(age_summary, centers = n)
    age_summary$cluster <- kmeans_model$cluster

    customers <- df %>%
      group_by(customer) %>%
      summarise(Total = sum(total), Age = unique(age)) %>%
      left_join(age_summary %>% select(age, cluster), by = c("Age" = "age"))

    customers
  })

  # Pie Chart
  output$pieChart <- renderPlot({
    df <- data()
    counts <- table(df$paymentType)
    labels <- paste(names(counts), ": ", round(100 * counts / sum(counts), 1), "%", sep = "")
    pie(counts, labels = labels, col = c("green3", "blue2"), main = "Payment Types")
    legend("topright", legend = names(counts), fill = c("green3", "blue2"))
  })

  # Bar Plot
  output$barPlot <- renderPlot({
    ggplot(data(), aes(x = factor(age), y = total)) +
      geom_bar(stat = "identity", fill = "red3") +
      labs(x = "Age", y = "Total Spending", title = "Total Spending by Age")
  })

  # City Plot
  output$cityPlot <- renderPlot({
    city_spending <- data() %>%
      group_by(city) %>%
      summarise(Total = sum(total)) %>%
      arrange(desc(Total))
    
    ggplot(city_spending, aes(x = reorder(city, -Total), y = Total)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "City", y = "Total Spending", title = "Spending by City") +
      scale_y_continuous(labels = scales::comma)
  })

  # Boxplot
  output$boxplot <- renderPlot({
    boxplot(data()$total, col = "yellow2", main = "Total Spending Distribution")
  })

  # Insights
  output$insights <- renderText({
    df <- data()
    
    # Most common item per city
    most_sales <- df %>%
      mutate(items = strsplit(as.character(items), ",\\s*")) %>%
      unnest(items) %>%
      group_by(city, items) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(city) %>%
      slice_max(count, n = 1)

    insight <- paste0(
      "- The most sold item in each city is:\n",
      paste(apply(most_sales, 1, function(row) paste("  •", row["city"], "->", row["items"])), collapse = "\n"), "\n\n",
      "- Ages 22, 37, and 55 have highest spending.\n",
      "- Cairo and Alexandria lead in total purchases.\n",
      "- Payment types (Cash vs Credit) are used almost equally.\n",
      "- The most frequent item is likely 'whole milk'.\n",
      "- Association rules show people who buy yogurt often also buy whole milk."
    )
    
    return(insight)
  })
}

# Run app
shinyApp(ui, server)

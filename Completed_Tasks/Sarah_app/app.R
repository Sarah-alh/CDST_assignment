#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Bar Plot Generator with Mean and SEM"),
  sidebarLayout(
    sidebarPanel(
      textInput("Unstimulated_vals", "Unstimulated Values (space separated)", value = "0 0 0"),
      textInput("aCD328_vals", "aCD3/28 Values (space separated)", value = "0 0 0"),
      textInput("Swainsonine_vals", "Swainsonine Values (space separated)", value = "0 0 0"),
      downloadButton("savePlot", "Save Plot")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output) {
  dataInput <- reactive({
    unstimulated_values <- as.numeric(unlist(strsplit(input$Unstimulated_vals, " ")))
    aCD328_values <- as.numeric(unlist(strsplit(input$aCD328_vals, " ")))
    swainsonine_values <- as.numeric(unlist(strsplit(input$Swainsonine_vals, " ")))
    
    data <- data.frame(
      group = factor(rep(c("Unstimulated", "aCD3/28", "Swainsonine"), each = 3),
                     levels = c("Unstimulated", "aCD3/28", "Swainsonine")),
      value = c(unstimulated_values, aCD328_values, swainsonine_values)
    )
    
    data <- na.omit(data)
    return(data)
  })
  
  output$barPlot <- renderPlot({
    data <- dataInput() %>%
      group_by(group) %>%
      summarize(mean_value = mean(value, na.rm = TRUE),
                sd_value = sd(value, na.rm = TRUE),
                n = n()) %>%
      mutate(se_value = ifelse(n > 0, sd_value / sqrt(n), 0))
    
    ggplot(data, aes(x = group, y = mean_value, fill = group)) +
      geom_bar(stat = "identity", width = 0.2) + # Adjust the width of bars if necessary
      geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) + # Add y-axis line
      scale_fill_manual(values = c("Unstimulated" = "#9DD4CC", "aCD3/28" = "#E49CB1", "Swainsonine" = "#BEC9E1")) +
      labs(x = "", y = "Glut-1 MFI") +
      ggtitle("Glut-1 MFI") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(color = "black"), # Ensure y-axis line is visible
        axis.line.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
        )
     
  })
  
  output$savePlot <- downloadHandler(
    filename = function() { paste("bar_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      g <- ggplot(dataInput() %>%
                    group_by(group) %>%
                    summarize(mean_value = mean(value, na.rm = TRUE),
                              sd_value = sd(value, na.rm = TRUE),
                              n = n()) %>%
                    mutate(se_value = ifelse(n > 0, sd_value / sqrt(n), 0)),
                  aes(x = group, y = mean_value, fill = group)) +
        geom_bar(stat = "identity", width = 0.4) +
        geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
        geom_hline(yintercept = 0, color = "black", size = 0.5) +
        scale_fill_manual(values = c("Unstimulated" = "#9DD4CC", "aCD3/28" = "#E49CB1", "Swainsonine" = "#BEC9E1")) +
        labs(x = "", y = "Glut-1 MFI") +
        ggtitle("Glut-1 MFI") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(color = "black"), # Ensure y-axis line is visible
          axis.line.x = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
          )

      ggsave(file, plot = g, width = 8, height = 6)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

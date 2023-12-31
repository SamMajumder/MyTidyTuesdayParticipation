
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(shiny)
library(plotly)

life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv')
life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_different_ages.csv')
life_expectancy_female_male <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_female_male.csv')


# Assuming all datasets are loaded into R
combined_df <- life_expectancy %>%
  inner_join(life_expectancy_different_ages, by = c("Entity", "Code", "Year")) %>%
  inner_join(life_expectancy_female_male, by = c("Entity", "Code", "Year"))

## Building the Shiny App

ui <- fluidPage(
  titlePanel("Life Expectancy Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Select Country", choices = unique(combined_df$Entity)),
      selectInput("ageGroupInput", "Select Age Group", 
                  choices = c("At Birth" = "LifeExpectancy0", 
                              "Age 10" = "LifeExpectancy10", 
                              "Age 25" = "LifeExpectancy25", 
                              "Age 45" = "LifeExpectancy45", 
                              "Age 65" = "LifeExpectancy65", 
                              "Age 80" = "LifeExpectancy80"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Life Expectancy by Age Group", plotlyOutput("lifeExpPlot")),
        tabPanel("Gender Disparity in Life Expectancy", plotlyOutput("lifeExpDiffPlot"))
      )
    )
  )
)

server <- function(input, output) {
  output$lifeExpPlot <- renderPlotly({
    filtered_data <- combined_df %>%
      filter(Entity == input$countryInput) %>%
      select(Year, input$ageGroupInput)
    
    p <- ggplot(filtered_data, aes(x = Year, 
                                   y = get(input$ageGroupInput))) +
      geom_line(color = "blue") +
      labs(x = "Year",
           y = "Life Expectancy") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$lifeExpDiffPlot <- renderPlotly({
    filtered_data <- combined_df %>%
      filter(Entity == input$countryInput) %>%
      select(Year, LifeExpectancyDiffFM)
    
    p <- ggplot(filtered_data, aes(x = Year, 
                                   y = LifeExpectancyDiffFM)) +
      geom_line(color = "red") +
      labs(x = "Year",
           y = "Life Expectancy Difference") +
      theme_minimal()
    
    ggplotly(p)
  })
}






shinyApp(ui = ui, server = server)


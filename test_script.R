
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(shiny)

############
##### 

# Load the data
patient_risk_profiles <- patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

########## 
#### 

df <- patient_risk_profiles %>% 
  tidyr::pivot_longer(cols = starts_with("age group:"),
                      names_to = "age_group",
                      values_to = "value_age_group") %>%  
  tidyr::pivot_longer(cols = `Acetaminophen exposures in prior year`:`Antibiotics Tetracyclines in prior year`,
                      names_to = "record_of",
                      values_to = "value_record_of_group") %>% 
  tidyr::pivot_longer(cols = starts_with("predicted risk of"),
                      names_to = "Disease",
                      values_to = "Predicted_risk") %>% 
  tidyr::pivot_longer(cols = `Sex = FEMALE`:`Sex = MALE`,
                      names_to = "Sex",
                      values_to = "values_sex") %>% 
  dplyr::select(-c("value_age_group","values_sex","value_record_of_group","personId"))  %>% 
  dplyr::mutate(Sex = case_when(Sex == "Sex = FEMALE" ~ "F",
                                Sex == "Sex = MALE" ~ "M")) %>% 
  dplyr::mutate(Disease = str_remove_all(Disease, "predicted risk of "))


# Define the UI
ui <- fluidPage(
  selectInput("age_group", "Age group:", unique(df$age_group)),
  selectInput("record_of", "Record of:", unique(df$record_of)),
  selectInput("Disease", "Predicted risk:", unique(df$Disease)),
  plotOutput("barPlot")
) 

# Define the server
server <- function(input, output) {
  
  # Create a reactive object to filter the data based on user input
  filtered_data <- reactive({
    df %>%
      filter(age_group == input$age_group,
             record_of == input$record_of,
             Disease == input$Disease)
  })
  
  # Create the dynamic bar plot
  output$barPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Sex, y = Predicted_risk, fill = Sex)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = brewer.pal(2, "Set3")) +  # Set3 is a color-blind friendly palette
      labs(title = paste("Predicted Risk of", input$Disease,
                         "for Age Group", input$age_group,
                         "with Record of", input$record_of),
           y = "Predicted Risk",
           x = "Sex") +
      theme_minimal()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

  
  
 



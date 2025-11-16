library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(janitor)

# Load your datasets here or make sure they are loaded before running the app:

hospitals <- read_csv(
  "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/hospitals.csv")|>
  janitor::clean_names()
births_scotland <- read.csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/d534ae02-7890-4fbc-8cc7-f223d53fb11b/download/10.3_birthsbyhospital.csv")|>
  janitor::clean_names()


# Join births with hospital names for filtering and display
births_data <- births_scotland %>%
  left_join(hospitals %>% select(hospital_code, hospital_name),
            by = c("hospital" = "hospital_code"))

ui <- fluidPage(
  titlePanel("Live and Still Births in Scotland by Financial Year and Hospital since 2000"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_outcome",
        label = "Select Outcome:",
        choices = c("Live", "Still"),
        selected = "Live"
      ),
      selectInput(
        inputId = "selected_hospital",
        label = "Select Hospital:",
        choices = c("All", unique(na.omit(births_data$hospital_name))),
        selected = "All"
      )
    ),
    mainPanel(
      plotOutput("births_bar_chart")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(births_data)  # Ensure births_data is loaded
    
    data <- births_data %>%
      filter(outcome == input$selected_outcome) %>%  # Use input for outcome
      mutate(financial_year_start = as.numeric(str_sub(financial_year, 1, 4))) %>%
      filter(!is.na(financial_year_start) & financial_year_start >= 2000)
    
    if (input$selected_hospital != "All") {
      data <- data %>% filter(hospital_name == input$selected_hospital)
    }
    
    data %>%
      group_by(financial_year_start) %>%
      summarise(total_births = sum(as.numeric(smr02births), na.rm = TRUE)) %>%
      arrange(financial_year_start)
  })
  
  output$births_bar_chart <- renderPlot({
    plot_data <- filtered_data()
    ggplot(plot_data, aes(x = factor(financial_year_start), y = total_births)) +
      geom_bar(stat = "identity", fill = "#008080") +
      labs(x = "Financial Year Start", y = "Total Births",
           title = paste(input$selected_outcome, "Births in", input$selected_hospital, "by Financial Year")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)

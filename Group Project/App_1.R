library(tidyverse)   # For data manipulation and visualization
library(lubridate)   # For handling date variables
library(janitor)     # For data cleaning
library(shiny)       # For building interactive web applications
library(shinydashboard)  # For creating dashboard layouts
library(sf)          # For working with spatial data
library(ggmap)       # For accessing Google Maps API
library(leaflet) 

hospitals <- read_csv("data/healthcare_dataset.csv")  # Load dataset
hospitals <- clean_names(hospitals)  # Clean column names
hospitals <- hospitals %>%
  select(-blood_type, -room_number) 
hospitals_final <- hospitals %>%
  mutate(id = as.integer(factor(name))) %>%
  select(-doctor, -name) %>%
  arrange(id)  # Arrange by hospital ID for consistency

hospitals_location <- read_csv("data/hospital_locations.csv") %>%
  clean_names() %>%
  select(index, id, name, address, city, state, zip, type, status, latitude, longitude, website) %>%
  mutate(website = na_if(website, "NOT AVAILABLE"))

ui <- dashboardPage(
  dashboardHeader(title = "Best Hospitals for Care"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Table Settings", width = 3,
        selectInput("medical_condition", "Select Condition", choices = c("Arthritis", "Asthma", "Cancer", "Diabetes", "Hypertension", "Obesity"), 
                    selected = "Cancer"),
        selectInput("insurance_provider", "Select Insurance Provider", choices = c("Aetna", "Blue Cross", "Cigna", "Medicare", "UnitedHealthcare"),
                    selected = "Medicare")
      ),
      box(
        title = "Top 5 Hospitals",
        width = 7,
        dataTableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  # Define top5 as a reactive expression
  top5 <- reactive({
    hospitals_final %>%
      mutate(discharge_date = as.Date(discharge_date), 
             date_of_admission = as.Date(date_of_admission),
             time_spent = discharge_date - date_of_admission) %>%
      filter(!is.na(time_spent)) %>% 
      separate(time_spent, into = "length_of_stay", sep = " ") %>% 
      mutate(length_of_stay = as.numeric(length_of_stay)) %>% 
      mutate(hospital_quality = log10(length_of_stay * billing_amount))
  })
  
  output$table <- renderDataTable({
    data <- top5()
    
    # Filter the data based on selected medical condition
    data <- data[data$medical_condition == input$medical_condition,]
    
    # Filter the data based on selected insurance provider
    data <- data[data$insurance_provider == input$insurance_provider,]
    
    # Select top five hospitals based on hospital quality
    data <- data %>% 
      arrange(hospital_quality) %>% 
      head(5) %>%
      select(hospital, hospital_quality, admission_type, medical_condition)
    
    # Return the filtered data for rendering in the data table
    data
  })
}

shinyApp(ui, server)

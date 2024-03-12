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
  dashboardHeader(title = "Best Hospitals Around You"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Filter Options", width = 3, solidHeader = TRUE, status = "primary",
        selectInput("medical_condition", "Medical Condition", choices = c("Arthritis", "Asthma", "Cancer", "Diabetes", "Hypertension", "Obesity"), 
                    selected = "All"),
        selectInput("insurance_provider", "Insurance Provider", choices = c( "Aetna", "Blue Cross", "Cigna", "Medicare", "UnitedHealthcare"),
                    selected = "All")
      ),
      box(
        title = "Top 5 Hospitals",
        width = 9,
        status = "info",
        dataTableOutput("table")
      )
    ),
    fluidRow(
      box(
        title = "Matched Hospitals",
        width = 12,
        solidHeader = TRUE, status = "warning",
        dataTableOutput("matched_hospitals_table")
      )
    ),
    fluidRow(
      box(
        title = "Map of Matched Hospitals",
        width = 12,
        status = "success",
        leafletOutput("map_plot") #
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Define top5 as a reactive expression
  top5 <- reactive({
    filtered_data <- hospitals_final %>%
      filter(if (input$medical_condition != "All") medical_condition == input$medical_condition else TRUE,
             if (input$insurance_provider != "All") insurance_provider == input$insurance_provider else TRUE) # here we creat two drop down menues for the user to choose from
    
    top_hospitals <- filtered_data %>%
      mutate(discharge_date = as.Date(discharge_date), 
             date_of_admission = as.Date(date_of_admission),
             time_spent = discharge_date - date_of_admission) %>%
      filter(!is.na(time_spent)) %>% 
      separate(time_spent, into = "length_of_stay", sep = " ") %>% 
      mutate(length_of_stay = as.numeric(length_of_stay)) %>% 
      mutate(hospital_quality = log10(length_of_stay * billing_amount)) %>%
      arrange(hospital_quality) %>% 
      head(5) %>%
      select(hospital, hospital_quality, admission_type, medical_condition)
    
    top_hospitals
  })
  
  # Render top 5 hospitals table
  output$table <- renderDataTable({
    data <- top5()
    data
  })
  
  # Define the reactive function to find matched hospitals
  matched_hospitals <- reactive({
    top_hospitals <- top5()
    
    combined_results <- lapply(top_hospitals$hospital, function(hosp) {
      w <- strsplit(hosp, " ")[[1]][1]
      w <- gsub("[^[:alnum:]]", " ", w)
      hospitals_location[grep(w, tolower(hospitals_location$name), ignore.case = TRUE), c("name", "zip", "latitude", "longitude")]
    })
    
    result <- do.call(rbind, combined_results)
    result
  })
  
  # Render the matched hospitals datatable 
  output$matched_hospitals_table <- renderDataTable({
    matched_hospitals() #produce table with matched hospitals
  })
  
  # Render the map of matched hospitals
  output$map_plot <- renderLeaflet({
    result <- matched_hospitals() #we need to use this package to be able to add the map, because otherwise the app will not work if we use simple ggmap code 
    
    leaflet(result) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~name #adds names to points on map
      )
  })
}

# Run the application
shinyApp(ui, server)
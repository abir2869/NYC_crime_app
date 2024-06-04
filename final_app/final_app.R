library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(data.table)
library(leaflet)
library(RColorBrewer)


getSeason <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    "Winter"
  } else if (month %in% c(3, 4, 5)) {
    "Spring"
  } else if (month %in% c(6, 7, 8)) {
    "Summer"
  } else {
    "Fall"
  }
}



age_size <- function(age_group) {
  if (age_group == "18-24") return(8)
  if (age_group == "25-44") return(6)
  if (age_group == "45+") return(5)
  return(4)  
}

# UI starts here
ui <- dashboardPage(
  dashboardHeader(title = "NYC Crime Analysis App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      selectInput("yearInput", "Choose Year", choices = NULL),
      selectInput("ageInput", "Choose Age Group", choices = NULL),
      selectInput("offenseInput", "Choose Offense Category", choices = NULL),
      selectInput("raceInput", "Select Race", choices = NULL)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Filtered Data", status = "primary", solidHeader = TRUE, DTOutput("dataTable")),
                box(title = "Crime Chart", status = "primary", solidHeader = TRUE, plotOutput("dataPlot")),
                box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, tableOutput("summaryStats"))
              )
      ),
      tabItem(tabName = "maps",
              fluidRow(
                box(title = "Crime Map", status = "primary", solidHeader = TRUE, leafletOutput("crimeMap"), width = 12)
              )
      )
    )
  )
)

# Server starts here
server <- function(input, output, session) {
  data <- fread("data/NYPD_Arrests_Data_Historic_fully_cleaned.csv")
  data$ARREST_DATE <- as.Date(data$ARREST_DATE, format = "%m/%d/%Y")
  data$Year <- format(data$ARREST_DATE, "%Y")
  data$Season <- sapply(data$ARREST_DATE, getSeason)
  
  
  valid_age_groups <- c("18-24", "25-44", "45-64", "65+")
  
  data <- data %>%
    filter(AGE_GROUP %in% valid_age_groups) %>%
    mutate(Offense_Category = case_when(
      grepl("homicide|murder|manslaughter|rape|felony assault|kidnapping|robbery|arson", OFNS_DESC, ignore.case = TRUE) ~ "High",
      grepl("burglary|grand larceny|drug|felony|fraud|weapon|assault|sexual|prostitution|dangerous weapons|felony sex crimes", OFNS_DESC, ignore.case = TRUE) ~ "Medium",
      grepl("misdemeanor|harassment|trespass|disorderly conduct|administrative code|traffic|loitering|public intoxication|unclassified misdemeanor|petit larceny|parking offenses|alcoholic beverage control law", OFNS_DESC, ignore.case = TRUE) ~ "Low",
      TRUE ~ "Other"
    ))
  
  severity_colors <- colorFactor(palette = c("red", "orange", "green", "blue", "grey"), domain = unique(data$Offense_Category))
  
  observe({
    updateSelectInput(session, "yearInput", choices = c("Select All" = "All", unique(data$Year)))
    updateSelectInput(session, "ageInput", choices = c("All", unique(data$AGE_GROUP)))
    updateSelectInput(session, "offenseInput", choices = c("All", unique(data$Offense_Category)))
    updateSelectInput(session, "raceInput", choices = c("All", unique(data$PERP_RACE)))
  })
  
  filteredData <- reactive({
    temp_data <- data
    if (input$ageInput != "All") {
      temp_data <- temp_data %>% filter(AGE_GROUP == input$ageInput)
    }
    if (input$yearInput != "All") {
      temp_data <- temp_data %>% filter(Year == input$yearInput)
    }
    if (input$offenseInput != "All") {
      temp_data <- temp_data %>% filter(Offense_Category == input$offenseInput)
    }
    if (input$raceInput != "All") {
      temp_data <- temp_data %>% filter(PERP_RACE == input$raceInput)
    }
    temp_data
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData())
  })
  
  output$dataPlot <- renderPlot({
    req(nrow(filteredData()) > 0)
    ggplot(filteredData(), aes(x = Season, fill = PERP_RACE)) +
      geom_bar(stat = "count") +
      theme_minimal() +
      labs(title = "Crime Trends by Season for Selected Age Group and Year", x = "Season", y = "Number of Arrests") +
      scale_x_discrete(limits = c("Winter", "Spring", "Summer", "Fall"))
  })
  
  output$crimeMap <- renderLeaflet({
    req(filteredData())
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color = ~severity_colors(Offense_Category),
        radius = age_size(~AGE_GROUP),
        popup = ~paste("Date:", ARREST_DATE, "<br>",
                       "Charge:", OFNS_DESC, "<br>",
                       "Category:", Offense_Category, "<br>",
                       "Age Group:", AGE_GROUP, "<br>",
                       "Race:", PERP_RACE),
        fillOpacity = 0.85,
        stroke = FALSE
      ) %>%
      addLegend("bottomright", pal = severity_colors, values = ~Offense_Category,
                title = "Severity Category", opacity = 1)
  })
  
  #library(DT)  
  
  output$summaryStats <- renderTable({
    df <- filteredData()
    
    if (nrow(df) == 0) {
      return(data.frame(
        "Total Crimes" = NA,
        "High Severity" = NA,
        "Medium Severity" = NA,
        "Low Severity" = NA
      ))
    }
    
    summary_data <- data.frame(
      "Total Crimes" = nrow(df),
      "High Severity" = sum(df$Offense_Category == "High"),
      "Medium Severity" = sum(df$Offense_Category == "Medium"),
      "Low Severity" = sum(df$Offense_Category == "Low")
    )
    summary_data
  }, align = 'c')  
}



shinyApp(ui, server)

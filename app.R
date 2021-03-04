library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinyWidgets)
library(tidyverse)
library(bslib)

LAPD <- read_csv('LAPD_updated.csv')

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# App header & title -----------------------------------------------
header <- dashboardHeader(title='Arrests by the LAPD')

# Dashboard sidebar ------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Beginning of COVID-19", icon = icon("bar-chart"), tabName = "covid"),
    menuItem("Police Brutality Protests", icon = icon("bar-chart"), tabName = "protest"),
    
    # Select which areas to include ------------------------
    pickerInput(inputId = "selected_hood",
                label = "Select area(s):",
                choices = sort(unique(LAPD$`Area Name`)),
                options = list(`actions-box` = TRUE),
                multiple = TRUE, 
                selected = 'Central'),
    
    # Select variable to group by ----------------------------------
    selectInput(inputId = "group", 
                label = "Group Arrests by:",
                choices = c("Race and Ethnicity" = "`Descent Code`", 
                            "Sex" = "`Sex Code`", 
                            "Arrest Type" = "`Arrest Type Code`"))
    
    ### Daily vs weekly
  )
)

body <- dashboardBody(tabItems(
  # Plot page ----------------------------------------------
  tabItem("covid",
          
          # Input and Value Boxes ----------------------------------------------
          # fluidRow(
          #   infoBoxOutput("mass"),
          #   valueBoxOutput("height"),
          #   valueBoxOutput("height")
          # ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Over time", plotlyOutput("covid_time")),
                   tabPanel("Grouped", plotlyOutput("covid_group")),
                   tabPanel("By Offense", plotlyOutput("covid_offense")))
          )
          
          # Data table ----------------------------------------
          
          # fluidPage(
          #   box(title = "Arrests during early covid weeks", DT::dataTableOutput("covid_table"), width = 12))
          # 
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("protest",
          # Input and Value Boxes ----------------------------------------------
          # fluidRow(
          #   infoBoxOutput("mass"),
          #   valueBoxOutput("height"),
          #   valueBoxOutput("height")
          # ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Over time", plotlyOutput("protest_time")),
                   tabPanel("Grouped", plotlyOutput("protest_group")),
                   tabPanel("By Offense", plotlyOutput("protest_offense")))
          )
          
          # Data table ----------------------------------------
          
          # fluidPage(
          #   box(title = "Arrests during protest weeks", DT::dataTableOutput("protest_table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types ------
  LAPD_subset <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood)
  })

  # Covid tab --------------------------------------------------------
  output$covid_time <- renderPlotly({
    ggplot(data = LAPD_subset(), aes_string(x = 'date', group=input$group, color=input$group)) +
      geom_point(stat='count') +
      geom_line(stat='count', alpha=0.3) +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=5) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Date',
           y = 'Arrest Count',
           title = "Arrests over time"
      )
  })
  
  output$covid_group <- renderPlotly({
    ggplot(data = LAPD_subset(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$covid_offense <- renderPlotly({
    ggplot(data=LAPD_subset(), aes(x="",fill=`Charge Group Description`)) +
      geom_bar(width=1) +
      coord_polar("y", start=0) +
      labs(x = 'Percentage of Arrests',
           title = 'Breakdown of Offenses')
  })
  
  # Protest tab -------------------------------------------------------
  
  output$protest_time <- renderPlotly({
    ggplot(data = LAPD_subset(), aes_string(x = 'date', group=input$group, color=input$group)) +
      geom_point(stat='count') +
      geom_line(stat='count', alpha=0.3) +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=5) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Date',
           y = 'Arrest Count',
           title = "Arrests over time"
      )
  })
  
  output$protest_group <- renderPlotly({
    ggplot(data = LAPD_subset(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$protest_offense <- renderPlotly({
    ggplot(data=LAPD_subset(), aes(x="",fill=`Charge Group Description`)) +
      geom_bar(width=1) +
      coord_polar("y", start=0) +
      labs(x = 'Percentage of Arrests',
           title = 'Breakdown of Offenses')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


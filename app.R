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
library(plotly)

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
                   tabPanel("Total", plotlyOutput("covid_group")),
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
                   tabPanel("Total", plotlyOutput("protest_group")),
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
  LAPD_subset_COVID <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood & week %in% c(9:11))
  })
  
  LAPD_subset_PROTEST <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood & week %in% c(21:23))
  })

  # Covid tab --------------------------------------------------------
  output$covid_time <- renderPlotly({
    ggplot(data = LAPD_subset_COVID(), aes_string(x = 'date', group=input$group, color=input$group)) +
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
    ggplot(data = LAPD_subset_COVID(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$covid_offense <- renderPlotly({
    vals <- LAPD_subset_COVID() %>% group_by(`Charge Group Description`) %>% summarize(count=n())
    fig <- plot_ly(labels = ~vals$`Charge Group Description`, values = vals$count, type = 'pie')
    fig <- fig %>% layout(title = 'Arrests by Offense Type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
   
  })
  
  # Protest tab -------------------------------------------------------
  
  output$protest_time <- renderPlotly({
    ggplot(data = LAPD_subset_PROTEST(), aes_string(x = 'date', group=input$group, color=input$group)) +
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
    ggplot(data = LAPD_subset_PROTEST(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$protest_offense <- renderPlotly({
    vals <- LAPD_subset_PROTEST() %>% group_by(`Charge Group Description`) %>% summarize(count=n())
    fig <- plot_ly(labels = ~vals$`Charge Group Description`, values = vals$count, type = 'pie')
    fig <- fig %>% layout(title = 'Arrests by Offense Type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


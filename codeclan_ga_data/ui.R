library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = div("CodeClan website navigation analytics",
      style = "color: #1b3445; 
                                    font-size: 20px; 
                                    text-align: left"
    ),
    titleWidth = 350
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("dashboard")),
      menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("dashboard")),
      menuItem("User Journey", tabName = "user_journey", icon = icon("dashboard")),
      menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("dashboard")),
      # Adding blank menuItem to give space for dateRangeInput
      HTML("<br/><br/><br/>"),
      # Adding date picker that allows users to select specific date range and the plot automatically updates itself

      dateRangeInput(

        # InputId to link up date picker code within the server function
        inputId = "date_range",

        # Setting "Select date range" subheader to a h5 header and bold
        label = tags$b(tags$h5("Select date range")),

        # Start date in the date picker is by default is set to the first date that is found within the date column of clean_dashboard_data
        start = first(clean_dashboard_data$date),

        # End date in the date picker by default is set to the last date found within the date column of clean_dashboard_data
        end = last(clean_dashboard_data$date),

        #  min and max values are set to prevent users selecting values  in date picker that are outwith the dates available in clean_dashboard_data
        min = first(clean_dashboard_data$date),

        max = last(clean_dashboard_data$date)
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "goal_channel",
        # Keegan insert code here
        fluidRow(
          tabBox(
            title = "Goal Completions by Channel and Social Network",
            width = 12,
            height = "500px",
            tabPanel(
              "Channel",
              plotOutput("channel_conversions_plot", height = "400px"),
              radioButtons("downsampling_channel", 
                           "Summarise by Month or Day", 
                           c("Month", "Day"), 
                           selected = NULL)
            ),
            tabPanel(
              "Social Network",
              plotOutput("social_conversions_plot", height = "400px"),
              radioButtons("downsampling_social", 
                           "Summarise by Month or Day", 
                           c("Month", "Day"), 
                           selected = NULL)
            )
          )
        )
      ),

      tabItem(
        tabName = "goal_page",
        # Amber insert code here
      ),

      tabItem(
        tabName = "user_journey",
        # Stewart insert code here
      ),

      tabItem(
        tabName = "user_aquisitions",
        # Greg insert code here
      )
    )
  )
)

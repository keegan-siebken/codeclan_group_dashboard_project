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
      menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("futbol")),
      menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("futbol")),
      menuItem("User Journey", tabName = "user_journey", icon = icon("walking")),
      menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("users")),
      # Adding blank menuItem to give space for dateRangeInput
      menuItem(""),
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
          box(
            title = "Channel Conversions Graph",
            width = 9,
            #testing date picker
            DT::dataTableOutput("dashboard_date_test")
          ),
          box(
            title = "Channel Conversions Table",
            width = 3,
            "Channel Conversions Table Content"
          )
        ),
        fluidRow(
          box(
            title = "Social Media Conversions Graph",
            width = 9,
            #testing date picker
            DT::dataTableOutput("goal_date_test")
          ),
          box(
            title = "Social Media Conversions Table",
            width = 3,
            "Social Media Conversions Table Content"
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
        # first row
        fluidRow(
          infoBoxOutput("total_number_users_Box"),
          infoBoxOutput("total_number_sessions_Box"),
          infoBoxOutput("mean_bounce_rate_Box"),
          tabBox(
            title = tagList(shiny::icon("walking"), "How users arrive on the CodeClan website"), width = 12, height = "850px",
            tabPanel("by channel",
                     plotlyOutput("users_by_channel_by_day_plot", height = "370px"),
                     tags$br(),
                     plotlyOutput("users_by_channel_by_month_plot", height = "370px"),
                     # radioButtons("which_time_channel_plot",
                     #              inline = TRUE,
                     #              tags$i("Select daily or monthly views"),
                     #              choices = c("daily", "monthly")
                     # )
                     ),
            tabPanel("by device",
                    plotlyOutput("users_by_device_by_day_plot", height = "370px"),
                    tags$br(),
                    plotlyOutput("users_by_device_by_month_plot", height = "370px"),
                    # radioButtons("which_time_device_plot",
                    #              inline = TRUE,
                    #              tags$i("Select daily or monthly views"),
                    #              choices = c("daily", "monthly")
                    # )
          )
        )
      )
    )
  )
)
)

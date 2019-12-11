library(shiny)


server <- function(input, output) { 
  #---------------------------------------------------------------
  #filter api calls by user-selected dates:
  dashboard_data_filtered <- reactive({
    clean_dashboard_data %>%
      filter(
        date >= input$date_range[1] & 
        date <= input$date_range[2]
        )
  })
    
  goal_path_data_filtered <- reactive({
    clean_goal_path_data %>%
      filter(
        date >= input$date_range[1] & 
        date <= input$date_range[2]
      )
  })
  #---------------------------------------------------------------
  
  
  
  #---------------------------------------------------------------
  #goal_channel server code - Keegan insert code here:
  
  #testing date picker filtering
  output$dashboard_date_test <- DT::renderDataTable({
    dashboard_data_filtered()
  })
  
  output$goal_date_test <- DT::renderDataTable({
    goal_path_data_filtered()
  })
  
  #---------------------------------------------------------------
  
  ################################################################
  
  #---------------------------------------------------------------
  #goal_page server code - Amber insert code here:
  
  #---------------------------------------------------------------
  
  ################################################################
  
  #---------------------------------------------------------------
  #user_journey server code - Stewart insert code here:
  
  
  #---------------------------------------------------------------
  
  ################################################################
  
  #---------------------------------------------------------------
  #user_acquisitions server code - Greg insert code here:
  
  users_by_device_by_day <- clean_dashboard_data %>%
      group_by(month, day, device_category) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(
        time = make_date(day, month)
      )
  
  output$users_by_device_by_day_plot <- renderPlot({
  ggplot(users_by_device_by_day) +
    geom_line(aes(x = time, y = users, col = device_category)) +
    labs(
      x = "\nDay",
      y = "Number of users",
      title = "Number of users by device category",
      subtitle = "by day\n",
      col = "Device"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_codeclan(discrete = TRUE, palette = "main")
    
  })
  
  users_by_channel_by_day <- clean_dashboard_data %>%
      group_by(month, day, channel_grouping) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(
        time = make_date(day, month)
      )
  
  output$users_by_channel_by_day_plot <- renderPlot({
    ggplot(users_by_channel_by_day) +
      geom_line(aes(x = time, y = users, col = channel_grouping)) +
      labs(
        x = "\nDay",
        y = "Number of users",
        title = "Number of users by chanel grouping",
        subtitle = "by day\n",
        col = "Channel"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_codeclan(discrete = TRUE, palette = "mixed")
    
  })
  
  #---------------------------------------------------------------
}


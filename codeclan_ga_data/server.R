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
  #user_acquisitions server code
  
  total_users <- reactive ({
    dashboard_data_filtered() %>%
      summarise(
        users = sum(users)
      ) 
  })
  
  output$total_number_users_Box <- renderInfoBox({
    infoBox(
      "Total number of users: ", prettyNum(total_users(), big.mark = ","), tags$em(" (over date range selected)"), icon = icon("users"),
      color = "navy"
    )
  })
  
  total_sessions <- reactive ({
    dashboard_data_filtered() %>%
      summarise(
        sessions = sum(sessions)
      ) 
  })
  
  output$total_number_sessions_Box <- renderInfoBox({
    infoBox(
      "Total number of sessions: ", prettyNum(total_sessions(), big.mark = ","), tags$em(" (over date range selected)"), icon = icon("user-friends"),
      color = "blue"
    )
  })
  
  mean_bounce_rate <- reactive ({
    dashboard_data_filtered() %>%
      summarise(
        bounce = mean(bounce_rate_percentage)
      ) 
  })
  
  output$mean_bounce_rate_Box <- renderInfoBox({
    infoBox(
      "Mean bounce rate: ", round(mean_bounce_rate()), tags$em("% (over date range selected)"), icon = icon("door-open"),
      color = "light-blue"
    )
  })
  
  users_by_device_by_day <- reactive ({
    dashboard_data_filtered() %>%
      group_by(date, device_category) %>%
      summarise(
        users = sum(users)
      ) 
  })
    
  output$users_by_device_by_day_plot <- renderPlotly({
  ggplotly(ggplot(users_by_device_by_day()) +
    geom_line(aes(x = date, y = users, col = device_category)) +
    labs(
      x = "\nDate",
      y = "Number of users",
      title = "Number of users by device category by day",
      col = "Device"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(breaks= "1 month", labels = scales::date_format("%Y-%m-%d")) +
    scale_color_codeclan(discrete = TRUE, palette = "main")
  )
  })
  
  users_by_device_by_month <- reactive ({
    
    dashboard_data_filtered() %>%
      group_by(year, month, device_category) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(year_month = make_date(year, month))
  })
  
  output$users_by_device_by_month_plot <- renderPlotly({
    ggplotly(ggplot(users_by_device_by_month()) +
               geom_col(aes(x = year_month, y = users, fill = device_category)) +
               labs(
                 x = "\nMonth",
                 y = "Number of users",
                 title = "Number of users by channel grouping by month",
                 fill = "Device"
               ) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(labels = scales::comma) +
               scale_x_date(breaks= "1 month", labels = scales::date_format("%Y-%m")) +
               scale_fill_codeclan(discrete = TRUE, palette = "main")
    )
  })
  
  users_by_channel_by_day <- reactive ({

    dashboard_data_filtered() %>%
      group_by(date, channel_grouping) %>%
      summarise(
        users = sum(users)
      )
  })
  
  output$users_by_channel_by_day_plot <- renderPlotly({
    ggplotly(ggplot(users_by_channel_by_day()) +
      geom_line(aes(x = date, y = users, col = channel_grouping)) +
      labs(
        x = "\nDate",
        y = "Number of users",
        title = "Number of users by channel grouping by day",
        col = "Channel"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(breaks= "1 month", labels = scales::date_format("%Y-%m-%d")) +
      scale_color_codeclan(discrete = TRUE, palette = "mixed")
    )
  })
  
  users_by_channel_by_month <- reactive ({
    
    dashboard_data_filtered() %>%
      group_by(year, month, channel_grouping) %>%
      summarise(
        users = sum(users)
      ) %>%
      mutate(year_month = make_date(year, month))
  })
  
  output$users_by_channel_by_month_plot <- renderPlotly({
    ggplotly(ggplot(users_by_channel_by_month()) +
               geom_col(aes(x = year_month, y = users, fill = channel_grouping)) +
               labs(
                 x = "\nMonth",
                 y = "Number of users",
                 title = "Number of users by channel grouping by month",
                 fill = "Channel"
               ) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(labels = scales::comma) +
               scale_x_date(breaks= "1 month", labels = scales::date_format("%Y-%m")) +
               scale_fill_codeclan(discrete = TRUE, palette = "mixed")
    )
  })
  
  #---------------------------------------------------------------
}


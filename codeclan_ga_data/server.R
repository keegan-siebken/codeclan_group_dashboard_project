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
      subtitle = "by day\n",
      col = "Device"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
                 subtitle = "by month\n",
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
        subtitle = "by day\n",
        col = "Channel"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
                 subtitle = "by month\n",
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


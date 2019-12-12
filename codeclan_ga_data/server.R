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
  
  #channel conversions plot:
  output$channel_conversions_plot <- renderPlotly({
    #down-sample based on radio button input:
    if(input$downsampling_channel == "Month") {
      #combine goal completions, group channel and month, summarise
      channel_full_month <- dashboard_data_filtered() %>%
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(channel_grouping, year_month) %>%
        summarise(goal_total_channel = sum(goal_total)) %>%
        ungroup()
        
        #plot
        month_plot <- ggplot(channel_full_month) +
        geom_line(aes(
          x = year_month,
          y = goal_total_channel,
          group = channel_grouping,
          colour = channel_grouping)) +
          labs(
            x = "\nMonth",
            y = "Total Goal Completions",
            title = "Total Goal Completions by Channel",
            colour = "Channel"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_codeclan(discrete = TRUE, palette = "main")
        ggplotly(month_plot)
        
        
    } else{
      channel_day <- dashboard_data_filtered() %>%
        #combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        group_by(channel_grouping, date) %>%
        summarise(goal_total_channel = sum(goal_total))
        
        #plot
        day_plot <- ggplot(channel_day,
                      aes(x = date, 
                          y = goal_total_channel)) +
        geom_point(aes(colour = channel_grouping)) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Channel",
          colour = "Channel"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
        ggplotly(day_plot)
    }
  })

  # social media conversions plot:
  # first filter top-performing networks - too many small ones:
  goal_social_top <- reactive({
    dashboard_data_filtered() %>%
    filter(social_network != "(not set)") %>%
    mutate(goal_total = glas_info_session_click_completions +
             edin_info_session_click_completions) %>%
    group_by(social_network) %>%
    summarise(goal_total_social = sum(goal_total)) %>%
    arrange(desc(goal_total_social)) %>%
    slice(1:7)
    })

  
  #plot
  output$social_conversions_plot <- renderPlotly({
    # now filter by top-performers and group by date
    # down_sample based on radio button input
    if(input$downsampling_social == "Month") {
      social_month <- dashboard_data_filtered() %>%
        #filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%
        #combine goal completions, group social network and month, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(social_network, year_month) %>%
        summarise(goal_total_social = sum(goal_total)) %>%
        ungroup()
        
        #plot
        social_month_plot <- ggplot(social_month, 
                                    aes(x = year_month, 
                                        y = goal_total_social,
                                        group = social_network)) +
        geom_line(aes(colour = social_network)) +
        labs(
          x = "Month",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
        ggplotly(social_month_plot)
    } else{
      social_day <- dashboard_data_filtered() %>%
        #filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%
        #combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        group_by(social_network, date) %>%
        summarise(goal_total_social = sum(goal_total))
        
        #plot
        social_day_plot <- ggplot(social_day,
                                  aes(x = date, 
                                      y = goal_total_social)) +
        geom_point(aes(colour = social_network)) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_codeclan(discrete = TRUE, palette = "main")
        ggplotly(social_day_plot)
    }
    
    
    
    # dashboard_data_filtered() %>%
    #   filter(social_network %in% goal_social_top()$social_network) %>%
    #   mutate(goal_total = glas_info_session_click_completions +
    #            edin_info_session_click_completions) %>%
    #   group_by(social_network, date) %>%
    #   summarise(goal_total_social = sum(goal_total)) %>%
    #   ggplot(aes(x = date,
    #              y = goal_total_social)) +
    #   geom_line(aes(colour = social_network))
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
      "Mean bounce rate: ", paste0(round(mean_bounce_rate()), "%"), tags$em("(over date range selected)"), icon = icon("door-open"),
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


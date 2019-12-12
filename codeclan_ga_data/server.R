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
  output$channel_conversions_plot <- renderPlot({
    #down-sample based on radio button input:
    if(input$downsampling_channel == "Month") {
      #combine goal completions, group channel and month, summarise
      channel_full_month <- dashboard_data_filtered() %>%
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(channel_grouping, year_month) %>%
        summarise(goal_total_channel = sum(goal_total))
        
        #plot
        ggplotly(ggplot(channel_full_month, aes(x = year_month, 
                   y = goal_total_channel,
                   group = channel_grouping)) +
        geom_line(aes(colour = channel_grouping)) +
          labs(
            x = "Month",
            y = "Total Goal Completions",
            title = "Total Goal Completions by Channel",
            colour = "Channel"
          ))
    } else{
      dashboard_data_filtered() %>%
        #combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        group_by(channel_grouping, date) %>%
        summarise(goal_total_channel = sum(goal_total)) %>%
        
        #plot
        ggplot(aes(x = date, 
                   y = goal_total_channel)) +
        geom_line(aes(colour = channel_grouping)) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Channel",
          colour = "Channel"
        )
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
  output$social_conversions_plot <- renderPlot({
    # now filter by top-performers and group by date
    # down_sample based on radio button input
    if(input$downsampling_social == "Month") {
      dashboard_data_filtered() %>%
        #filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%
        #combine goal completions, group social network and month, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        mutate(year_month = substr(date, 1, 7)) %>%
        group_by(social_network, year_month) %>%
        summarise(goal_total_social = sum(goal_total)) %>%
        
        #plot
        ggplot(aes(x = year_month, 
                   y = goal_total_social,
                   group = social_network)) +
        geom_line(aes(colour = social_network)) +
        labs(
          x = "Month",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        )
    } else{
      dashboard_data_filtered() %>%
        #filter to only top-performing social networks
        filter(social_network %in% goal_social_top()$social_network) %>%
        #combine goal completions, group channel and date, summarise
        mutate(goal_total = glas_info_session_click_completions +
                 edin_info_session_click_completions) %>%
        group_by(social_network, date) %>%
        summarise(goal_total_social = sum(goal_total)) %>%
        
        #plot
        ggplot(aes(x = date, 
                   y = goal_total_social)) +
        geom_line(aes(colour = social_network)) +
        labs(
          x = "Date",
          y = "Total Goal Completions",
          title = "Total Goal Completions by Social Network",
          colour = "Social Network"
        )
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
  #user_acquisitions server code - Greg insert code here:
  
  
  #---------------------------------------------------------------
}


library(shiny)


server <- function(input, output) { 


# User Journey Dashboard Server Script ------------------------------------

  # behaviour flow datatable server script that allows user to filter by channel and device
  output$user_flow <- DT::renderDataTable({
    filtered_behaviour_flow <- behaviour_flow %>%
      filter(
        channel == input$channel,
        device == input$device,
      )
    
    # server script that allows user to check "Exclude (not set)" checkbox and removes (not set) values from second_page_path
    
    if_not_set <- if(input$not_set == TRUE) {
      filter(filtered_behaviour_flow, second_page_path != "(not set)")
    } else{
      filtered_behaviour_flow
    }
    
    # Column visibility button that allows user to include or exclude certain columns within datatable
    DT::datatable(if_not_set,
                  extensions = c("Buttons"),
                  rownames = FALSE,
                  options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
    
  })
  
  # entry page datatable server script that allows users to segment by channel and device 
  output$entry_page <- DT::renderDataTable({
    entry_page <- entry_page_user_flow %>%
      filter(
        channel == input$channel,
        device == input$device,
      )
    
    # Column visibility button that allows user to include or exclude certain columns within datatable
    DT::datatable(entry_page,
                  extensions = c("Buttons"),
                  rownames = FALSE,
                  options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
    
  })
  
  # second page datatable server script that allows users to segment by channel and device
  output$next_page <- DT::renderDataTable({
    next_page <- next_page_user_flow %>%
      filter(
        channel == input$channel,
        device == input$device,
        
      )
    # Column visibility button that allows user to include or exclude certain columns within datatable
    DT::datatable(next_page,
                  extensions = c("Buttons"),
                  rownames = FALSE,
                  options = list(dom = "Bfrtip", buttons = I("colvis"))
    )
    
  })
}


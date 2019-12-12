

library(shiny)


server <- function(input, output) {
  output$user_flow <- DT::renderDataTable({
    user_flow_data <- clean_dashboard_data %>%
      select(
        date,
        channel_grouping,
        device_category,
        sessions,
        landing_page_path,
        bounces,
        second_page_path,
        exits,
        pageviews,
        edin_info_session_click_completions,
        glas_info_session_click_completions
      ) %>%
      group_by(
        "date" = substring(date, 1, 7),
        "channel" = channel_grouping,
        "device" = device_category,
        "entry page" = landing_page_path,
        "next page" = second_page_path
      ) %>%
      summarise(
        sessions = sum(sessions),
        bounces = sum(bounces),
        exits = sum(exits),
        pageviews = sum(pageviews),
        "info session click (ed)" = sum(edin_info_session_click_completions),
        "info session click (gla)" = sum(glas_info_session_click_completions)
      ) %>%
      mutate(
        "bounce rate" = round((sessions / bounces) * 100, 0),
        "exit rate" = round((exits / pageviews) * 100, 0)
      ) %>%
      select(
        date,
        channel,
        device,
        sessions,
        "entry page",
        bounces,
        "bounce rate",
        "next page",
        exits,
        "exit rate",
      ) %>%
      arrange(desc(date)) %>%
      filter(
        channel == input$channel,
        device == input$device
      )

    DT::datatable(user_flow_data,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(dom = "Bfrtip", buttons = I("colvis"))
    )

  })
}

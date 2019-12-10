library(shiny)
library(shinydashboard)


source("global.R")

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = div("CodeClan website navigation analytics", 
                                                style = "color: #1b3445; 
                                    font-size: 20px; 
                                    text-align: left"),
                                    titleWidth = 350),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Goal completions by Channel", tabName = "goal_channel", icon = icon("dashboard")),
                            menuItem("Goal completions by Page", tabName = "goal_page", icon = icon("dashboard")),
                            menuItem("User Journey", tabName = "user_journey", icon = icon("dashboard")),
                            menuItem("User Aquisitions", tabName = "user_aquisitions", icon = icon("dashboard"),
                            )
                        )
                    ),
                    
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "goal_channel",
                                    # Keegan insert code here        
                            ),
                            
                            tabItem(tabName = "goal_page",
                                    # Amber insert code here        
                            ),
                            
                            tabItem(tabName = "user_journey",
                                    # Stewart insert code here
                            ),
                            
                            tabItem(tabName = "user_aquisitions",
                                    # Greg insert code here
                            )
                        )
                    )
)



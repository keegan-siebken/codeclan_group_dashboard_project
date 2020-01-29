# ShinyDashboard Group Project

The brief for my first group project at CodeClan was to design a shiny dashboard in R that would allow for easier analysis of Google Analytics data, and to complete this dashboard within one week. Our team worked together to understand the Google Analytics data, plan out exactly what needed to be visualised, how to visualise it, and how to assign the execution of those visualisations across the team. 

We used the Google Analytics API to gather the data for the dashboard, but have synthesized the data for this public GitHub repo. Therefore, the graphs are not representative of real data.

The dashboard is broken into four sections based on tracking users through to particular goal path pages. The first section provides graphs for tracking the sources from which users successfully reached the goal path pages, separated by channel and social network:

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/1_goal_completions_channel.png">

The second section tracks goal completions by previous page:  

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/2_goal_completions_page.png">

The third section provides tables for analysing more granular information about user journeys, with many options for filtering the data:  

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/3_user_journey.png">

The final section graphs arrivals to the site in general from various sources: 

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/4_user_acquisitions.png">


## Getting Started

To run the dashboard: download all folders and files in the repo, open global.R in RStudio and click the Run App button:

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/global_r_file.png">

<img src = "https://raw.githubusercontent.com/keegan-siebken/codeclan_group_dashboard_project/master/readme_images/r_studio_screenshot.png">


### Prerequisites

In order to run the app, you will need to have R and RStudio installed, along with the following R libraries:

* googleAnalyticsR
* keyring
* janitor
* ggplot2
* formattable
* tidyverse
* ggthemes
* lubridate
* DT
* plotly
* shiny
* shinydashboard

## Authors

* **Keegan Siebken** 
* **Stewart Donaldson** 
* **Amber Lawther** 
* **Greg Anderson** 




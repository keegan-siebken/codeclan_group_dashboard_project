# Section 1 - Loading libraries-------------------------------------

library(googleAnalyticsR)
library(keyring)
library(janitor)
library(ggplot2)
library(formattable)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(DT)
library(plotly)



# Section 2 - API---------------------------------------------------


keyring_unlock(keyring = "googleanalytics")
# Setting options(googleAuthR.client_id) and options(googleAuthR.client_secret) to activate API call
options(googleAuthR.client_id = keyring::key_get(service = "ga_client_id", keyring = "googleanalytics"))
options(googleAuthR.client_secret = keyring::key_get(service = "ga_client_secret", keyring = "googleanalytics"))

# reloading package
devtools::reload(pkg = devtools::inst("googleAnalyticsR"))

#Authenticating account

ga_auth()

keyring_lock(keyring = "googleanalytics")

# ----------------------------------------
# only need this section when sorting website initially - not necessary in production use
#Get a list of accounts you have access to
# account_list <- ga_account_list()
# 
# account_list
# 
# #ViewID is the way to access the account you want
# account_list$viewId
#------------------------------------------

#Select full CodeClan website data
my_ga_id <- 102407343

#Set today's date and year previous for flexible API call
today <- today()
year_previous <- today() - days(365)


#Call the API to access the data required for various dashboards
dashboard_data <- google_analytics(my_ga_id,
                 date_range = c(year_previous, today),
                 metrics = c(
                   "users",
                   "sessions",
                   "bounces",
                   "bounceRate",
                   "exitRate",
                   "avgTimeOnPage",
                   "goal3Completions",
                   "goal5Completions"),
                 dimensions = c(
                   "date",
                   "channelGrouping",
                   "source",
                   "deviceCategory",
                   "landingPagePath",
                   "secondPagePath",
                   "exitPagePath",
                   "socialNetwork"
                 ),
                 max = -1,
                 anti_sample = TRUE
)

# Had to set up a seperate API call for goal path dimensions because API doesn't work if goal path dimensions are called with channel and traffic source dimensions/metrics

goal_path_data <- google_analytics(my_ga_id,
                                   date_range = c(year_previous, today),
                                   metrics = c(
                                     "goal3Completions",
                                     "goal5Completions"
                                     ),
                                   dimensions = c(
                                     "date",
                                     "goalCompletionLocation",
                                     "goalPreviousStep1",
                                     "goalPreviousStep2"
                                   ),
                                   max = -1,
                                   anti_sample = TRUE
)



# data cleaning script ----------------------------------------------------

# Cleaning dashboard data

dashboard_data <- clean_names(dashboard_data)

clean_dashboard_data <- dashboard_data %>%
  # adding year, month, day columns based on date column
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
    ) %>%
  # converting avg_time_on_page from seconds into user friendly format of hours, minutes and seconds
  mutate(
    avg_time_on_page_period = as.period(avg_time_on_page)
    ) %>%
  # reordering data so year, month, day columns are beside the date column
  select(
    date,
    year,
    month,
    day,
    channel_grouping:avg_time_on_page,
    avg_time_on_page_period,
    goal3completions,
    goal5completions
         ) %>%
  # converting character vectors to lower case
  mutate(
    channel_grouping = str_to_lower(channel_grouping),
    source           = str_to_lower(source),
    bounce_rate      = round(bounce_rate, 2),
    exit_rate        = round(exit_rate, 2)
    ) %>%
  # renaming columns to help user better understand the data
  rename (
    edin_info_session_click_completions = goal5completions,
    glas_info_session_click_completions = goal3completions,
    bounce_rate_percentage = bounce_rate,
    exit_rate_percentage = exit_rate
  )


# Cleaning goal path data

goal_path_data <- clean_names(goal_path_data)

clean_goal_path_data <- goal_path_data %>%
  # adding year, month, day columns based on date column
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>%
  # reordering data so year, month, day columns are beside the date column
  select(
    date,
    year,
    month,
    day,
    goal_completion_location:goal5completions
    ) %>%
  rename (
    edin_info_session_click_completions = goal5completions,
    glas_info_session_click_completions = goal3completions
  )



##################################################################
#BUILDING GRAPHS AND TABLES FOR goal_channel - MAKE SURE TO REMOVE
#note: only using dashboard_data for this one
#start with channel source for successful conversions:

# channel_month <- clean_dashboard_data %>%
  #combine goal completions, group channel and month, summarise
  # mutate(goal_total = glas_info_session_click_completions +
  #          edin_info_session_click_completions) %>%
  # mutate(year_month = substr(date, 1, 7)) %>%
  # group_by(channel_grouping, year_month) %>%
  # summarise(goal_total_channel = sum(goal_total))
  
  #plot
# channel_month %>%
#   ggplot(aes(x = year_month, group = channel_grouping)) +
#   geom_line(aes(y = goal_total_channel,
#                 colour = channel_grouping))

#combine goal completions, group channel and date, summarise
# goal_channel <- clean_dashboard_data %>%
#   mutate(goal_total = glas_info_session_click_completions +
#            edin_info_session_click_completions) %>%
#   group_by(channel_grouping, month) %>%
#   summarise(goal_total_channel = sum(goal_total)) 

#plot
# goal_channel %>%
#   ggplot(aes(x = month, 
#              y = goal_total_channel)) +
#   geom_line(aes(colour = channel_grouping))

#definitely going to have to change summary based on user input
#also need selectors for channel grouping

#Now to see what social media looks like: 

# first filter top-performing networks - too many small ones:
# goal_social_top <- clean_dashboard_data %>%
#   filter(social_network != "(not set)") %>%
#   mutate(goal_total = glas_info_session_click_completions +
#            edin_info_session_click_completions) %>%
#   group_by(social_network) %>%
#   summarise(goal_total_social = sum(goal_total)) %>%
#   arrange(desc(goal_total_social)) %>%
#   slice(1:5)

#now filter by top-performers and group by date
# goal_social <- clean_dashboard_data %>%
#   filter(social_network %in% goal_social_top$social_network) %>%
#   mutate(goal_total = glas_info_session_click_completions +
#            edin_info_session_click_completions) %>%
#   group_by(social_network, date) %>%
#   summarise(goal_total_social = sum(goal_total)) %>%
#   filter(date >= today() - days(7))

#plot
# goal_social %>%
#   ggplot(aes(x = date,
#              y = goal_total_social)) +
#   geom_line(aes(colour = social_network))

#Again, definitely going to have to change summary based on user input
#also need selectors for channel grouping

##################################################################





# Section 3 - colour pallete---------------------------------------------------
# codeclan corporate colour pallete
codeclan_colours <- c(
  `codeclan dark blue` = "#1b3445",
  `codeclan light blue` = "#4da2cd",
  `codeclan other blue` = "#4ca1cd",
  `codeclan gold`     = "#e1bf76",
  `codeclan pink` = "#e74b7d",
  `codeclan red` = "red",
  `codeclan light grey` = "#cccccc",
  `codeclan dark grey`  = "#8c8c8c")

# Any changes to these colours, or addition of new colours, are done in the above vector.

# Tip: use back ticks to remove naming restrictions (e.g. to include spaces for `light grey` and `dark grey`).

# Function that extracts the hex codes from this vector by name.
codeclan_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (codeclan_colours)

  codeclan_colours[cols]
}

# This allows us to get hex colors in a robust and flexible way. For example, you can have all colours returned as they are, specify certain colours, in a particular order, add additional function arguments and checks, and so on:
# codeclan_cols()
# codeclan_cols("codeclan gold", "codeclan light blue")

# Combine colours into palettes

# Codeclan has a few main colours, but the full list (above) includes other official colours used for a variety of purposes. So we can now create palettes (various combinations) of these colours. Similar to how we deal with colours, first define a list like such:

codeclan_palettes <- list(
  `main`  = codeclan_cols("codeclan light blue", "codeclan dark blue", "codeclan gold"),
  `cool`  = codeclan_cols("codeclan dark blue", "codeclan light blue", "codeclan other blue"),
  `hot`   = codeclan_cols("codeclan gold", "codeclan red", "codeclan pink"),
  `mixed` = codeclan_cols("codeclan gold","codeclan light blue", "codeclan other blue","codeclan dark blue", "codeclan dark grey"),
  `grey`  = codeclan_cols("codeclan light grey", "codeclan dark grey")
)

# Changes or new colour palettes are added in this list. We write a function to access and interpolate them like so:

# Return function to interpolate a example color palette
#' @param palette Character name of palette in example_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()

codeclan_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- codeclan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# This function gets a pallete by name from the list ("main" by default), has a boolean condition determining whether to reverse the order or not, and additional arguments to pass on to colorRampPallete() (such as an alpha value). This returns another function:

# This returned function will interpolate the palette colours for a certain number of levels, making it possible to create shades between our original colours.

# This is what we need to create custom ggplot2 scales.

# Scales for ggplot2

# We can now create custom colour and fill scale functions for ggplot2.
# Colour scale constructor for codeclan colours

#' @param palette Character name of palette in example_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE

scale_color_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for codeclan colours

#' @param palette Character name of palette in codeclan_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE

scale_fill_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Each of these functions specifies a palette, whether the palette is being applied based on a discrete or numeric variable, whether to reverse the palette colors, and additional arguments to pass to the relevant ggplot2 function (which differs for discrete or numeric mapping).

# Examples.
#
# # Colour by discrete variable using default palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_codeclan()
#
# # Colour by numeric variable with cool palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_codeclan(discrete = FALSE, palette = "cool")
#
# # Fill by discrete variable with different palette + remove legend (guide)
# ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_codeclan(palette = "mixed", guide = "none")

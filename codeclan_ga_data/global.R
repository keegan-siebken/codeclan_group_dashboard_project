#Accessing libraries
library(googleAnalyticsR)
library(keyring)

# Setting options(googleAuthR.client_id) and options(googleAuthR.client_secret) to activate API call
options(googleAuthR.client_id = keyring::key_get(service = "ga_client_id", keyring = "googleanalytics"))
options(googleAuthR.client_secret = keyring::key_get(service = "ga_client_secret", keyring = "googleanalytics"))

# reloading package
devtools::reload(pkg = devtools::inst("googleAnalyticsR"))

#Authenticating account
ga_auth()

#Select all CodeClan website data
my_ga_id <- 102407343

#Call the API to access the data you require
google_analytics(my_ga_id, 
                 date_range = c("2019-01-01", "2019-03-01"), 
                 metrics = "sessions", 
                 dimensions = "date")
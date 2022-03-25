# Library for packages used in app

library(shiny)
library(tidyverse)
library(tidyr)
library(janitor)
library(DT)
library(kableExtra)
library(pROC)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(shinydashboard)



# Database info..
db_config <-  config::get()$db

#Create database connection
conn <- dbConnect(
  RSQLite::SQLite(),
  dbname = db_config$dbname
)

 df_auto <- conn %>% 
     tbl('auto') %>% 
     collect()
 df_auto <- as.data.frame(unclass(df_auto),
                          stringsAsFactors = TRUE)

# # Stop database connection when app stops
shiny::onStop(function(){
  dbDisconnect(conn)
})

#Get final model for prediction ----
model_final <- readRDS("./data/model_final.rds")


#Get ROC function for graphing ----

source("./R/ROC_function.R")

# # Turn off scientific notation
options(scipen = 999)

# # Set spinner type (for loading)
options(spinner.type = 8)




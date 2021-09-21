# load data from github to local server

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(bigrquery)
library(bigQueryR)
library(DBI)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "bgg"
)

# establish connection to AE Lab
# con <- dbConnect(odbc(),
#                  Driver = "ODBC Driver 17 for SQL Server",
#                  Server = "aelabdb.aebslab.local",
#                  Database = keyring::key_list("AE_LAB") %$% service,
#                  UID = keyring::key_list("AE_LAB") %$% username,
#                  PWD = keyring::key_get("AE_LAB", keyring::key_list("AE_LAB") %$% username),
#                  Port = 1433)

# source function for reading data 
source("functions/get_bgg_data_from_github.R")

# get todays data from bgg
#bgg_today<-get_bgg_data_from_github(Sys.Date())
bgg_today<-get_bgg_data_from_github(Sys.Date())

# # trim to load
bgg_load_today <- bgg_today %>%
        select(date,
               game_id,
             #  game_name,
               game_release_year,
               bgg_rank,
               bgg_average,
               bayes_average,
               users_rated)

### push to GCP 

# # authenticate
# bq_auth(path = keyring::key_get(service = "GCP"),
#         use_oob=F)
#         
# upload
# bq_table_upload(bq_bgg_today,
#                 values = bgg_load_today,
#                 write_disposition = "WRITE_APPEND",
#                 quiet=F)

#bq_table_download(bq_bgg_today)

# write
dbWriteTable(bigquerycon,
             name = "historical_game_rankings",
             append = T,
             value = bgg_load_today)

# wipe
rm(bgg_today, bgg_load_today)


# # load all data up until yesterday
# date_grid<-seq(as.Date("2016-10-12"),
#                        as.Date(Sys.Date() -1),
#                        by = 1)
# 
# 
# # get all from github
# library(foreach)
# github_files<-foreach(i = 1:length(date_grid),
#                       .combine = rbind.data.frame,
#                       .errorhandling = 'remove') %do% {
#                               
#                               bgg_day<-get_bgg_data_from_github(date_grid[i])
#                               
#                               print(date_grid[i])
#                                     
#                               bgg_day
#                       }
# 
# # trim down
# bgg_load<- github_files %>%
#         select(date,
#                game_id,
#                #  game_name,
#                game_release_year,
#                bgg_rank,
#                bgg_average,
#                bayes_average,
#                users_rated)
# 
# # bq object
# bq_bgg_load<-as_bq_table(list(project_id = PROJECT_ID,
#                                dataset_id = "bgg",
#                                table_id = "historical_game_rankings"))
# 
# bq_table_exists(bq_bgg_load)
# 
# # push
# dbWriteTable(bigquerycon,
#              name = "historical_game_rankings",
#              append = T,
#              value = bgg_load %>%
#                      filter(year(date) == '2021'))
# 
# # # # append to sql table
# # # bq_table_upload(bq_bgg_today,
# # #                 values = bgg_load,
# #                 write_disposition = "WRITE_APPEND",
# #                 quiet=F)
# # 
# 
# # pause
# Sys.sleep(1)
# 
# # rm
# rm(bgg_day, bgg_load_day)
# 
# print(paste(date_grid[i], "loaded"))

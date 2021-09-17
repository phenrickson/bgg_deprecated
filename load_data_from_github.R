# load data from github to local server

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)

# establish connection to AE Lab
con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "aelabdb.aebslab.local",
                 Database = keyring::key_list("AE_LAB") %$% service,
                 UID = keyring::key_list("AE_LAB") %$% username,
                 PWD = keyring::key_get("AE_LAB", keyring::key_list("AE_LAB") %$% username),
                 Port = 1433)

# source function for reading data 
source("functions/get_bgg_data_from_github.R")

# get todays data from bgg
bgg_today<-get_bgg_data_from_github(Sys.Date())

# append to sql table
dbWriteTable(conn = con, 
             name = DBI::SQL('DEV_AE_LAB.BGG.GAME_RANKING_HISTORICALS'),
             value = bgg_today,
             append=T)

# load all data up until yesterday
# date_grid<-seq(as.Date("2016-10-12"),
#                        as.Date(Sys.Date() -1),
#                        by = 1)

date_grid<-seq(as.Date("2018-03-22"),
                       as.Date(Sys.Date() -1),
                       by = 1)

# loop over, pushing each to the DB
library(foreach)
foreach(i = 1:length(date_grid),
        .errorhandling = 'stop') %do% {
                
                bgg_temp<-get_bgg_data_from_github(date_grid[i])
                
                # append to sql table
                dbWriteTable(conn = con,
                             name = DBI::SQL('DEV_AE_LAB.BGG.GAME_RANKING_HISTORICALS'),
                             value = bgg_temp,
                             append=T)
                
                rm(bgg_temp)
                
                Sys.sleep(3)
                
                print(paste(date_grid[i], "loaded"))
                
                # insert a pause
                
                
                }


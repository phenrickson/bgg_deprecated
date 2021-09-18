# push ids through bgg api

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(splitstackshape)

# load bgg analytics
library(bggAnalytics)

# load big query
library(bigrquery)

# get game ids from most recent day
# source function for reading data 
source("functions/get_bgg_data_from_github.R")

# get todays data from bgg
bgg_today<-get_bgg_data_from_github(Sys.Date())

# get games ids
game_ids<-bgg_today$game_id

# push through API
# takes about 5 min?
games_obj<-bggGames$new(ids = game_ids,
                    chunk_size=1000)

# expand the resulting pull from the API
# takes about 10 min?
games_obj$expand()

# get xml
games_xml<-games_obj$xml

### Getting data ready for model
# the flattened out data, which contains concatenated strings
games_data<-games_obj$data %>%
        as_tibble() %>%
        rename(game_id = objectid) %>%
        mutate(recplayers = gsub("\"", "", recplayers)) %>%
        mutate(timestamp = games_obj$timestamp)

# next, we want the constituent pieces flattened out to create our data model
games_list<-games_obj$fetch(c("mechanics",
                              "mechanicsid",
                              "category", 
                              "categoryid",
                              "publishers", 
                              "publishersid", 
                              "designers",
                              "designersid",
                              "artists",
                              "artistsid",
                              "expansions",
                              "expansionsid",
                              "recplayers",
                              "bestplayers"))

# convert to data frame of lists
# takes about 20 minutes
df_list<-as_tibble(do.call(cbind, games_list)) %>%
        mutate(game_id = game_ids$game_id) %>%
        rename(mechanic = mechanics,
               mechanic_id = mechanicsid,
               category = category,
               category_id = categoryid,
               publisher = publishers,
               publisher_id = publishersid,
               designer = designers,
               designer_id = designersid,
               artist = artists,
               artist_id = artistsid,
               expansion = expansions,
               expansion_id = expansionsid) %>%
        select(game_id, everything())

### daily pull of games data with timestamp
games_daily<-games_data %>%
        select(game_id, 
               name, 
               type, 
               yearpublished, 
               rank, 
               average, 
               baverage, 
               stddev, 
               usersrated, 
               avgweight, 
               minplayers, 
               maxplayers, 
               playingtime,
               minplaytime, 
               maxplaytime, 
               minage, 
               numtrading, 
               numwanting, 
               numwishing, 
               numcomments, 
               timestamp)

## games
game_ids<-games_data %>%
        select(game_id,
               name) %>%
        unique() %>%
        arrange(game_id)

## categories
category_ids<-df_list %>% 
        select(game_id, 
               category, 
               category_id) %>%
        unnest(cols = c("category", "category_id")) %>%
        select(category_id, category) %>%
        unique() %>%
        arrange(category_id)

## mechanics
mechanic_ids<-df_list %>% 
        select(game_id, 
               mechanic, 
               mechanic_id) %>%
        unnest(cols = c("mechanic", "mechanic_id")) %>%
        select(mechanic_id, mechanic) %>%
        unique() %>%
        arrange(mechanic_id)


## publishers
publisher_ids<-df_list %>% 
        select(game_id, 
               publisher,
               publisher_id) %>%
        unnest(cols = c("publisher", "publisher_id")) %>%
        select(publisher_id, publisher) %>%
        unique() %>%
        arrange(publisher_id)

## designers
designer_ids<-df_list %>% 
        select(game_id, 
               designer, 
               designer_id) %>%
        unnest(cols = c("designer", "designer_id")) %>%
        select(designer_id, designer) %>%
        unique() %>%
        arrange(designer_id)

## artists
artist_ids<-df_list %>% 
        select(game_id, 
               artist, 
               artist_id) %>%
        unnest(cols = c("artist", "artist_id")) %>%
        select(artist_id, artist) %>%
        unique() %>%
        arrange(artist_id)

## expansions
expansion_ids<-df_list %>% 
        select(game_id, expansion, expansion_id) %>%
        unnest(cols = c("expansion", "expansion_id")) %>%
        select(expansion_id, expansion) %>%
        unique() %>%
        arrange(expansion_id)

### flatten ids for a table that has everything
# # could then craete views off of this to define common tables
# games_flattened<-df_list %>%
#         select(game_id, 
#                category_id,
#                publisher_id,
#                designer_id,
#                artist_id,
#                expansion_id
#                ) %>%
#         unnest(cols = c("category_id")) %>%
#         unnest(cols = c("publisher_id")) %>% 
#         unnest(cols = c("designer_id")) %>% 
#         unnest(cols = c("artist_id")) %>%
#         unnest(cols = c("expansion_id"))

## or, flatten specific tables
# game and artists
game_artists <- df_list %>%
        select(game_id, artist_id) %>%
        unnest(cols = c("artist_id")) %>%
        arrange(game_id, artist_id)

# game and categories
game_categories <- df_list %>%
        select(game_id, category_id) %>%
        unnest(cols = c("category_id")) %>%
        arrange(game_id, category_id)

# game and mechanics
game_mechanics <- df_list %>%
        select(game_id, mechanic_id) %>%
        unnest(cols = c("mechanic_id")) %>%
        arrange(game_id, mechanic_id)

# game and designers
game_designers <- df_list %>%
        select(game_id, designer_id) %>%
        unnest(cols = c("designer_id")) %>%
        arrange(game_id, designer_id)

# game and publishers
game_publishers <- df_list %>%
        select(game_id, publisher_id) %>%
        unnest(cols = c("publisher_id")) %>%
        arrange(game_id, publisher_id)

# game and artists
game_artists <- df_list %>%
        select(game_id, artist_id) %>%
        unnest(cols = c("artist_id")) %>%
        arrange(game_id, artist_id)

# games and expansions
games_expansions <- df_list %>%
        select(game_id, expansion_id) %>%
        unnest(cols = c("expansion_id")) %>%
        arrange(game_id, expansion_id)

# recommended players
games_recplayers<-df_list %>% 
        select(game_id, 
               recplayers) %>%
        unnest(cols = "recplayers") %>%
        arrange(game_id)

# best players
games_bestplayers<-df_list %>% 
        select(game_id, 
               bestplayers) %>%
        unnest(cols = "bestplayers") %>%
        arrange(game_id)

              
### push to GCP 
# library bigrquery
library(bigrquery)
library(bigQueryR)
library(DBI)

# authenticate
bq_auth(path = keyring::key_get(service = "GCP"),
         use_oob=T)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# # establish connection
# con<-dbConnect(
#         bigrquery::bigquery(),
#         project = PROJECT_ID,
#         dataset = "bgg"
# )
# 
# # write
# dbWriteTable(con,
#              name = "games_daily",
#              append = T,
#              value = games_data_daily)


# # load
# insert_upload_job(PROJECT_ID,
#                   "bgg",
#                   "games_raw_daily",
#                   games_data
#                   )

### make bq table objects
## daily table
# bq_games_daily<-as_bq_table(list(project_id = PROJECT_ID,
#                                 dataset_id = "bgg",
#                                 table_id = "games_daily"))

# write
dbWriteTable(con,
             name = "games_daily",
             append = T,
             value = games_daily)


## id tables
bq_game_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "game_ids"))

bq_category_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                dataset_id = "bgg",
                                table_id = "category_ids"))

bq_mechanic_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "mechanic_ids"))

bq_artist_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "artist_ids"))

bq_designer_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "designer_ids"))

bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "publisher_ids"))

bq_expansion_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "expansion_ids"))

## linking tables
bq_game_bestplayers<-as_bq_table(list(project_id = PROJECT_ID,
                                      dataset_id = "bgg",
                                      table_id = "game_bestplayers"))

bq_game_categories<-as_bq_table(list(project_id = PROJECT_ID,
                                   dataset_id = "bgg",
                                   table_id = "game_categories"))

bq_game_artists<-as_bq_table(list(project_id = PROJECT_ID,
                                      dataset_id = "bgg",
                                      table_id = "game_artists"))

bq_game_designers<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "game_designers"))

bq_game_publishers<-as_bq_table(list(project_id = PROJECT_ID,
                                    dataset_id = "bgg",
                                    table_id = "game_publishers"))

bq_game_expansions<-as_bq_table(list(project_id = PROJECT_ID,
                                     dataset_id = "bgg",
                                     table_id = "game_expansions"))

bq_game_recplayerss<-as_bq_table(list(project_id = PROJECT_ID,
                                     dataset_id = "bgg",
                                     table_id = "game_recplayerss"))

### Upload tables
bq_table_upload(bq_games_data, 
                values = games_daily,
                quiet=F)

## id tables
bq_table_upload(bq_game_ids, 
                game_ids,
                quiet=F)

bq_table_upload(bq_publisher_ids, 
                publisher_ids,
                quiet=F)

bq_table_upload(bq_designer_ids, 
                designer_ids,
                quiet=F)

bq_table_upload(bq_artist_ids, 
                artist_ids,
                quiet=F)

bq_table_upload(bq_category_ids, 
                category_ids,
                quiet=F)

bq_table_upload(bq_mechanic_ids, 
                mechanic_ids,
                quiet=F)





# bqr_upload_data(projectId = PROJECT_ID,
#                 datasetId = "bgg",
#                 tableId = "games_raw_daily",
#                 upload_data = games_data,
#                 writeDisposition ="WRITE_APPEND")
# 
# # load
# bqr_(bq_games_data, 
#                games_data,
#               quiet = F)

## look ups
# game ids
bq_game_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                dataset_id = "bgg",
                                table_id = "game_ids"))

# category ids
bq_category_ids<-as_bq_table(list(project_id = PROJECT_ID,
                              dataset_id = "bgg",
                              table_id = "category_ids"))

# mechanic
bq_mechanic_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "mechanic_ids"))

# publisher
bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                  dataset_id = "bgg",
                                  table_id = "publisher_ids"))

# publisher
bq_publisher_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                dataset_id = "bgg",
                                table_id = "publisher_ids"))

# artist
bq_artist_ids<-as_bq_table(list(project_id = PROJECT_ID,
                                   dataset_id = "bgg",
                                   table_id = "artist_ids"))

## linking tables







# upload tables
bq_table_upload(bq_games_data, games_data, quiet=F)





# make bq table objects
bq_games_data<-as_bq_table(list(project_id = PROJECT_ID,
                                dataset_id = "bgg",
                                table_id = "games_raw_daily"))

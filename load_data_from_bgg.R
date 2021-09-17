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
                    chunk_size=500)

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
                              "expansionsid"))

# convert to data frame of lists
df_list<-as_tibble(do.call(cbind, games_list)) %>%
        mutate(game_id = games_data$game_id) %>%
        rename(category = category,
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

## games
game_table<-games_data %>%
        select(game_id, name) %>%
        unique() %>%
        arrange(game_id)

## categories
category_table<-df_list %>% 
        select(game_id, category, category_id) %>%
        unnest(cols = c("category", "category_id")) %>%
        select(category_id, category) %>%
        unique() %>%
        arrange(category_id)

## publishers
publisher_table<-df_list %>% 
        select(game_id, publisher, publisher_id) %>%
        unnest(cols = c("publisher", "publisher_id")) %>%
        select(publisher_id, publisher) %>%
        unique() %>%
        arrange(publisher_id)

## designers
designer_table<-df_list %>% 
        select(game_id, designer, designer_id) %>%
        unnest(cols = c("designer", "designer_id")) %>%
        select(designer_id, designer) %>%
        unique() %>%
        arrange(designer_id)

## artists
artist_table<-df_list %>% 
        select(game_id, artist, artist_id) %>%
        unnest(cols = c("artist", "artist_id")) %>%
        select(artist_id, artist) %>%
        unique() %>%
        arrange(artist_id)

## expansions
expansion_table<-df_list %>% 
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

# games and categories
games_categories <- df_list %>%
        select(game_id, category_id) %>%
        unnest(cols = c("category_id")) %>%
        arrange(game_id, category_id)

# games and designers
games_designers <- df_list %>%
        select(game_id, designer_id) %>%
        unnest(cols = c("designer_id")) %>%
        arrange(game_id, designer_id)

# games and publishers
games_publishers <- df_list %>%
        select(game_id, publisher_id) %>%
        unnest(cols = c("publisher_id")) %>%
        arrange(game_id, publisher_id)

# games and artists
games_artists <- df_list %>%
        select(game_id, artist_id) %>%
        unnest(cols = c("artist_id")) %>%
        arrange(game_id, artist_id)

# games and expansions
games_expansions <- df_list %>%
        select(game_id, expansion_id) %>%
        unnest(cols = c("expansion_id")) %>%
        arrange(game_id, expansion_id)


# load to GCP
library(bigrquery)

# authenticate
bq_auth(path = keyring::key_get(service = "GCP"),
        use_oob=T)

# project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# bq table object
bq_games_data<-as_bq_table(list(project_id = PROJECT_ID,
                                dataset_id = "bgg",
                                table_id = "raw_games_daily"))

# upload
bq_table_upload(bq_games_data, games_data, quiet=NA)


### create look up tables
# categories
category_check<-games_data %>%
        select(objectid, starts_with("category")) %>%
        cSplit(., splitCols = c("category", "categoryid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") 

# check for NAs
assertthat::see_if(sum(is.na(category_check)) ==0)
assertthat::assert_that(sum(is.na(category_check)) ==0)

# no NAs
category_table<-category_check %>%
        rename(category_id = categoryid) %>%
        select(category_id, category) %>%
        mutate(category_id = as.integer(category_id)) %>%
        unique() %>%
        arrange(category_id) %>%
        as_tibble()

# rm temp table
rm(category_check)

# mechanics
mechanics_check<-games_data %>%
        select(objectid, starts_with("mechanic")) %>%
        mutate(mechanics = gsub("Deck, Bag, and Pool Building", "Deck Back and Pool Building", mechanics)) %>%
        mutate(mechanics = gsub("I Cut, You Choose", "I Cut You Choose", mechanics)) %>%
        mutate(mechanics = gsub("Worker Placement, Different Worker Types", "Worker Placement: Different Worker Types", mechanics)) %>%
        mutate(mechanics = gsub("Worker Placement with Dice Workers", "Worker Placement: Dice Workers", mechanics)) %>%
        cSplit(., splitCols = c("mechanics", "mechanicsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")

# checks
assertthat::see_if(sum(is.na(mechanics_check)) ==0)
assertthat::assert_that(sum(is.na(mechanics_check)) ==0)

# no NAs
mechanics_table <- mechanics_check %>%
        rename(mechanic_id = mechanicsid,
               mechanic = mechanics) %>%
        select(mechanic_id, mechanic) %>%
        mutate(mechanic_id = as.integer(mechanic_id)) %>%
        unique() %>%
        arrange(mechanic_id) %>%
        as_tibble()

rm(mechanics_check)

# designers
designers_check<-games_data %>%
        select(objectid, starts_with("designers")) %>%
        mutate(designers = gsub("Drakes, Jarvis, Walsh, and Gluck, Ltd.", "Drakes Jarvis Walsh and Gluck Ltd.", designers)) %>%
        mutate(designers = gsub("CHEN,JHAO-RU", "CHEN JHAO-RU", designers)) %>%
        mutate(designers = gsub("\\)", "", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", Jr.", "Jr.", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", II", "II", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", M.C.", "M.C.", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", III", "III", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", IV", "IV", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", Ph.D.", "PhD", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", PhD", "PhD", gsub("\\(", "", designers))) %>%
        mutate(designers = gsub(", Inc.", "Inc.", designers)) %>%
        mutate(designers = gsub(", Ltd.", "Ltd.", designers)) %>%
        mutate(designers = gsub(", LLC", "LLC", designers)) %>%
        cSplit(., splitCols = c("designers", "designersid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")

# checks
assertthat::see_if(sum(is.na(designers_check)) ==0)
assertthat::assert_that(sum(is.na(designers_check)) ==0)

designers_table <- designers_check %>%
        rename(designer = designers,
               designer_id = designersid) %>%
        select(designer_id, designer) %>% 
        unique() %>%
        mutate(designer_id = as.integer(designer_id)) %>%
        arrange(designer_id) %>%
        as_tibble()


# publishers
publishers_check<-games_data %>%
        select(objectid, starts_with("publisher")) %>%
        mutate(publishers = gsub("Korea Boardgames Co., Ltd.", "Korea Boardgames Co Ltd", publishers, fixed=T)) %>%
        mutate(publishers = gsub("Inca Perudo", "Perudo Inca", publishers)) %>%
   #     mutate(publishers = gsub("Alortujou, Alsip and Co", "Alortujou Alsip and Co", publishers)) %>%
        mutate(publishers = gsub("Kölner Stadt Anzeiger Magazin", "Magazin Kölner Stadt Anzeiger", publishers)) %>%
        mutate(publishers = gsub("Soldiers, Sailors and Airmen's Families Association", "Soldiers Sailors and Airmen's Families Association", publishers)) %>%
        mutate(publishers = gsub("S\\.A\\. Derwik", "Derwik SA", publishers)) %>%
        mutate(publishers = gsub(", Köln", " Köln", publishers)) %>%
        mutate(publishers = gsub("Co Ma", " Games Co Ma", publishers)) %>%
        mutate(publishers = gsub("SAEC Games", "Games SAEC", publishers)) %>%
        mutate(publishers = gsub("SALO", "salo", publishers)) %>%
        mutate(publishers = gsub("Säuberlin & Pfeiffer Sa, Vevey", " Säuberlin & Pfeiffer Sa Vevey", publishers)) %>%
        mutate(publishers = gsub("Ministerium für Umwelt, Raumordnung und Landwirtschaft", "Ministerium für Umwelt, Raumordnung und Landwirtschaft", publishers)) %>%
        mutate(publishers = gsub("Sage, Sons & Co", "Sage Sons & Co", publishers)) %>%
        mutate(publishers = gsub("Aires, Fanha e Raposo", "Aires Fanha e Raposa", publishers)) %>%
        mutate(publishers = gsub("Mann, Ivanov, and Ferber", "Mann Ivanov and Ferber", publishers)) %>%
        mutate(publishers = gsub("Cayro, the games", "Cayro the games", publishers)) %>%
        mutate(publishers = gsub("Ministerium für Umwelt, Raumordnung und Landwirtschaft", "Ministerium für Umwelt Raumordnung und Landwirtschaft", publishers)) %>%
        mutate(publishers = gsub("Nienstaedt & Co\\., Ltd\\., Hong Kong", "Nienstaedt & Co Ltd Hong Kong", publishers)) %>%
        mutate(publishers = gsub("Blue & Red Box, UK Ltd\\.", "Blue & Red Box UK Ltd", publishers)) %>%
        mutate(publishers = gsub("Si/si, les femmes existent", "si si les femmes existent", publishers, fixed=T)) %>%
     #   mutate(publishers = gsub("Tantrix Games Ibérica, S\\. L\\.", "Tantrix Games Ibérica SL", publishers)) %>%
        mutate(publishers = gsub("\\)", "", gsub("\\(", "", publishers)))  %>%
        mutate(publishers = gsub(".", "", publishers, fixed=T)) %>%
        mutate(publishers = gsub(", Co ", " Co", publishers)) %>%
        mutate(publishers = gsub(", Inc",  " Inc", publishers)) %>%
        mutate(publishers = gsub(", Wien", " Wien", publishers)) %>%
        mutate(publishers = gsub(", Inc ",  " Inc", publishers)) %>%
        mutate(publishers = gsub(", INC",  " Inc", publishers)) %>%
        mutate(publishers = gsub(", Ltd", " Ltd", publishers)) %>%
        mutate(publishers = gsub(", LTD", " Ltd", publishers)) %>%
        mutate(publishers = gsub(", Lda", " Lda", publishers)) %>%
        mutate(publishers = gsub(", llc", " LLC", publishers)) %>%
        mutate(publishers = gsub(", LLC", " LLC", publishers)) %>%
        mutate(publishers = gsub(", PLC", " PLC", publishers)) %>%
        mutate(publishers = gsub(", LLP", " LLP", publishers)) %>%
        mutate(publishers = gsub(", SCP", " SCP", publishers)) %>%
        mutate(publishers = gsub(", SA", " SA", publishers)) %>%
        mutate(publishers = gsub(", SL", " SL", publishers)) %>%
        mutate(publishers = gsub(", S L", " SL", publishers)) %>%
        mutate(publishers = gsub(", S A", " SA", publishers)) %>%
        mutate(publishers = gsub(", Lda", " Lda", publishers)) %>%
        mutate(publishers = gsub(", Limited", " Limited", publishers)) %>%
        cSplit(., splitCols = c("publishers", "publishersid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")

# # problems
# foo<-publishers_check %>%
#         filter((!is.na(publishers) & is.na(publishersid)) | 
#                        (is.na(publishers) & !(is.na(publishersid))))
# 
# foo

# # # duplicated ids
# dupes<-publishers_check %>%
#                 filter(objectid %in% foo$objectid) %>%
#         group_by(publishersid) %>%
#         summarize(ids = n_distinct(publishers)) %>%
#         filter(ids > 1) %>%
#         mutate(ids = as.character(ids))
# 
# publishers_check %>%
#         filter(objectid %in% foo$objectid) %>%
#         left_join(., dupes %>%
#                           rename(problemsid = publishersid), by = c("publishersid" = "problemsid")) %>%
#         View()

assertthat::see_if(sum(is.na(publishers_check)) ==0)
assertthat::assert_that(sum(is.na(publishers_check)) ==0)

publishers_table <- publishers_check %>%
        rename(publisher = publishers,
               publisher_id = publishersid) %>%
        select(publisher_id, publisher) %>%
        unique() %>%
        mutate(publisher_id = as.integer(publisher_id)) %>%
        arrange(publisher_id) %>%
        as_tibble()


# artists
artists_check<-games_data %>%
        select(objectid, starts_with("artist")) %>%
        mutate(artists = gsub("Elizabeth Thompson, Lady Butler", "Elizabeth Thompson Lady Butler", artists)) %>%
        mutate(artists = gsub("Jr Casas", "Casas Jr", artists)) %>%
        mutate(artists = gsub("SHI,RU-YI", "SHI RU-YI", artists)) %>%
        mutate(artists = gsub(", the Elder", " the Elder", artists)) %>%
        mutate(artists = gsub(", the Younger", " the Younger", artists)) %>%
        mutate(artists = gsub(".", "", artists, fixed=T)) %>%
        mutate(artists = gsub("\\)", "", artists)) %>%
        mutate(artists = gsub("\\)", "", artists)) %>%
        mutate(artists = gsub(", Jr", "Jr", artists)) %>%
        mutate(artists = gsub(", II", "II", artists)) %>%
        mutate(artists = gsub(", MC", "MC", artists)) %>%
        mutate(artists = gsub(", III", "III", artists)) %>%
        mutate(artists = gsub(", IV", "IV", artists)) %>%
        mutate(artists = gsub(", PhD", "PhD", artists)) %>%
        mutate(artists = gsub(", Inc", " Inc", artists)) %>%
        mutate(artists = gsub(", LLC", " LLC", artists)) %>%
        cSplit(., splitCols = c("artists", "artistsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")

# foo<-artists_check %>%
#         filter((!is.na(artists) & is.na(artistsid)) |
#                        (is.na(artists) & !(is.na(artistsid))))
# 
# foo

assertthat::see_if(sum(is.na(artists_check)) ==0)
assertthat::assert_that(sum(is.na(artists_check)) ==0)

artists_table<-artists_check %>%
        rename(artist = artists,
               artist_id = artistsid) %>%
        select(artist_id, artist) %>%
        unique() %>%
        mutate(artist_id = as.integer(artist_id)) %>%
        arrange(artist_id) %>%
        as_tibble()


# expansions
expansions_check<-games_data %>%
        select(objectid, starts_with("expansion")) %>%
        mutate(expansions = gsub("\\)", "", gsub("\\(", "", expansions))) %>%
        mutate(expansions = gsub(", Inc.", "Inc.", expansions)) %>%
        mutate(expansions = gsub(", Ltd.", "Ltd.", expansions)) %>%
        mutate(expansions = gsub(", LLC", "LLC", expansions)) %>%
        cSplit(., splitCols = c("expansions", "expansionsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")


foo<-expansions_check %>%
        filter((!is.na(expansions) & is.na(expansionsid)) |
                       (is.na(expansions) & !(is.na(expansionsid))))

foo


assertthat::see_if(sum(is.na(expansions_check)) ==0)
assertthat::assert_that(sum(is.na(expansions_check)) ==0)


expansions_check<-games_data %>%
        select(objectid, starts_with("expansion")) %>%
        mutate(expansions = gsub("\\)", "", gsub("\\(", "", expansions))) %>%
        mutate(expansions = gsub(", Inc.", "Inc.", expansions)) %>%
        mutate(expansions = gsub(", Ltd.", "Ltd.", expansions)) %>%
        mutate(expansions = gsub(", LLC", "LLC", expansions)) %>%
        cSplit(., splitCols = c("expansions", "expansionsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long")


        rename(expansion = expansions,
               expansion_id = expansionsid) %>%
        select(expansion_id, expansion) %>%
        unique() %>%
        mutate(expansion_id = as.integer(expansion_id)) %>%
        arrange(expansion_id) %>%
        as_tibble()


### write to GCP
di

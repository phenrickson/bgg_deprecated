# push ids through bgg api

# connect to sql
library(tidyverse)
library(magrittr)
library(odbc)
library(splitstackshape)

# load bgg analytics
library(bggAnalytics)

# get game ids from most recent day
# source function for reading data 
source("functions/get_bgg_data_from_github.R")

# get todays data from bgg
bgg_today<-get_bgg_data_from_github(Sys.Date())

# get games ids
game_ids<-bgg_today$game_id

# push through API
games_obj<-bggGames$new(ids = game_ids,
                    chunk_size=500)

# expand
games_obj$expand()

# get xml
games_xml<-games_obj$xml

# get data
games_data<-games_obj$data %>%
        as_tibble()

### create look up tables
# categories
category_table<-games_data %>%
        select(objectid, starts_with("category")) %>%
        # mutate(categorys = gsub("Deck, Bag, and Pool Building", "Deck Back and Pool Building", categorys)) %>%
        # mutate(categorys = gsub("Worker Placement, Different Worker Types", "Worker Placement: Different Worker Types", categorys)) %>%
        # mutate(categorys = gsub("Worker Placement with Dice Workers", "Worker Placement: Dice Workers", categorys)) %>%
        cSplit(., splitCols = c("category", "categoryid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") 
        rename(category_id = categoryid,
               category = categorys) %>%
        select(category_id, category) %>%
        mutate(category_id = as.integer(category_id)) %>%
        unique() %>%
        arrange(category_id) %>%
        as_tibble()

# mechanics
mechanics_table<-games_data %>%
        select(objectid, starts_with("mechanic")) %>%
        mutate(mechanics = gsub("Deck, Bag, and Pool Building", "Deck Back and Pool Building", mechanics)) %>%
        mutate(mechanics = gsub("Worker Placement, Different Worker Types", "Worker Placement: Different Worker Types", mechanics)) %>%
        mutate(mechanics = gsub("Worker Placement with Dice Workers", "Worker Placement: Dice Workers", mechanics)) %>%
        cSplit(., splitCols = c("mechanics", "mechanicsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") %>%
        rename(mechanic_id = mechanicsid,
               mechanic = mechanics) %>%
        select(mechanic_id, mechanic) %>%
        mutate(mechanic_id = as.integer(mechanic_id)) %>%
        unique() %>%
        arrange(mechanic_id) %>%
        as_tibble()

# designers
designers_table<-games_data %>%
        select(objectid, starts_with("designer")) %>%
        mutate(designers = gsub("\\)", "", gsub("\\(", "", designers))) %>%
        cSplit(., splitCols = c("designers", "designersid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") %>%
        rename(designer = designers,
               designer_id = designersid) %>%
        select(designer_id, designer) %>% 
        unique() %>%
        mutate(designer_id = as.integer(designer_id)) %>%
        arrange(designer_id) %>%
        as_tibble()


# publishers
publishers_table<-games_data %>%
        select(objectid, starts_with("publisher")) %>%
        mutate(publishers = gsub("\\)", "", gsub("\\(", "", publishers))) %>%
        mutate(publishers = gsub(", Inc.", "Inc.", publishers)) %>%
        mutate(publishers = gsub(", Ltd.", "Ltd.", publishers)) %>%
        mutate(publishers = gsub(", LLC", "LLC", publishers)) %>%
        cSplit(., splitCols = c("publishers", "publishersid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") %>%
        rename(publisher = publishers,
               publisher_id = publishersid) %>%
        select(publisher_id, publisher) %>%
        unique() %>%
        mutate(publisher_id = as.integer(publisher_id)) %>%
        arrange(publisher_id) %>%
        as_tibble()


# artists
artists_table<-games_data %>%
select(objectid, starts_with("artist")) %>%
        mutate(artists = gsub("\\)", "", gsub("\\(", "", artists))) %>%
        mutate(artists = gsub(", Inc.", "Inc.", artists)) %>%
        mutate(artists = gsub(", Ltd.", "Ltd.", artists)) %>%
        mutate(artists = gsub(", LLC", "LLC", artists)) %>%
        cSplit(., splitCols = c("artists", "artistsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") %>%
        rename(artist = artists,
               artist_id = artistsid) %>%
        select(artist_id, artist) %>%
        unique() %>%
        mutate(artist_id = as.integer(artist_id)) %>%
        arrange(artist_id) %>%
        as_tibble()


# expansions
expansions_table<-games_data %>%
        select(objectid, starts_with("expansion")) %>%
        mutate(expansions = gsub("\\)", "", gsub("\\(", "", expansions))) %>%
        mutate(expansions = gsub(", Inc.", "Inc.", expansions)) %>%
        mutate(expansions = gsub(", Ltd.", "Ltd.", expansions)) %>%
        mutate(expansions = gsub(", LLC", "LLC", expansions)) %>%
        cSplit(., splitCols = c("expansions", "expansionsid"), sep = ",",  stripWhite = T, drop = T, type.convert = F, direction = "long") %>%
        rename(expansion = expansions,
               expansion_id = expansionsid) %>%
        select(expansion_id, expansion) %>%
        unique() %>%
        mutate(expansion_id = as.integer(expansion_id)) %>%
        arrange(expansion_id) %>%
        as_tibble()

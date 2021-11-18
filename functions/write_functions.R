# who: phil henrickson
# what: functions to be used in project

# function to read data from github repository
get_bgg_data_from_github<-function(input_date) {
        
        url = paste("https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/", input_date, ".csv", sep="")
        
        data <- read_csv(url,
                         show_col_types = F) %>%
                mutate(date = input_date,
                       ID = as.integer(ID),
                       github_url = url) %>%
                rename(game_id = ID,
                       game_name = Name,
                       game_release_year = Year,
                       bgg_rank = Rank,
                       bgg_average = Average,
                       bayes_average = `Bayes average`,
                       users_rated = `Users rated`,
                       bgg_url = URL,
                       thumbnail = Thumbnail) %>%
                select(date, everything())
        
        return(data)
        
}

# dump
dump("get_bgg_data_from_github", file="functions/get_bgg_data_from_github.R")

get_collection <-
        function(username_string) {
                
                # get collection data from specified users
                collection_obj<-bggCollection$new(username = username_string)
                
                # expand
                collection_obj$expand(variable_names = c("name",
                                                         "type",
                                                         "yearpublished",
                                                         "rating",
                                                         "numplays",
                                                         "own",
                                                         "preordered",
                                                         "prevowned",
                                                         "fortrade",
                                                         "want",
                                                         "wanttoplay",
                                                         "wanttobuy",
                                                         "wishlist",
                                                         "wishlistpriority"))
                
                # convert to dataframe
                collection_data<-collection_obj$data %>%
                        rename(game_id = objectid) %>%
                        mutate(username = username_string,
                               date = Sys.Date(),
                               name = gsub(",", " ", name, fixed = T)) %>%
                        mutate_if(is.logical, .funs = ~ case_when(. == T ~ 1,
                                                                  .== F ~ 0)) %>%
                        select(username,
                               date,
                               game_id,
                               type,
                               rating,
                               own,
                               preordered,
                               prevowned,
                               fortrade,
                               want,
                               wanttoplay,
                               wanttobuy,
                               wishlist,
                               wishlistpriority)
                
                # check for duplicates
                dupes = which(duplicated(collection_data$game_id)==T)
                
                if (length(dupes) > 0) {
                        collection_data_out = collection_data[-dupes]
                } else {
                        collection_data_out = collection_data
                }
                
                # convert to tibble
                collection_data_out = collection_data_out %>%
                        as_tibble()
                
                return(collection_data_out)
                
        }

# dump
dump("get_collection", file="functions/get_collection.R")


# function for getting game record
# function for grabbing one game and getting its data in one record for model
get_game_record<-function(insert_id) {
        
        if(!require(tidyverse) ){ cat("function requires tidyverse package")}
        if(!require(magrittr) ){ cat("function requires magrittr package")}
        if(!require(splitstackshape) ){ cat("function requires splitstackshape package")}
        if(!require(bggAnalytics) ){ cat("function requires bggAnalytics package")}
        
        # push ID through API
        games_obj<-bggGames$new(ids = insert_id,
                                chunk_size=500)
        
        # expand the resulting pull from the API
        # takes about 10 min?
        games_obj$expand(variable_names = c(
                "objectid",
                "name",
                "type",
                "rank",
                "yearpublished",
                "average",
                "baverage",
                "stddev",
                "usersrated",
                "avgweight",
                "weightvotes",
                "numtrading",
                "numwanting",
                "numwishing",
                "numcomments",
                "minplayers",
                "maxplayers",
                "recplayers",
                "bestplayers",
                "playingtime",
                "minplaytime",
                "maxplaytime",
                "minage",
                "description",
                "mechanics",
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
                "expansionsid")
        )
        
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
        games_list<-games_obj$fetch(c("objectid",
                                      "mechanics",
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
                rename(game_id = objectid,
                       mechanic = mechanics,
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
        
        # game and categors
        game_categories <- df_list %>%
                select(game_id, category_id, category) %>%
                unnest(cols = c("game_id", "category_id", "category")) %>%
                arrange(game_id, category_id)
        
        # game and mechanics
        game_mechanics <- df_list %>%
                select(game_id, mechanic_id, mechanic) %>%
                unnest(cols = c("game_id", "mechanic_id", "mechanic")) %>%
                arrange(game_id, mechanic_id)
        
        # game and designers
        game_designers <- df_list %>%
                select(game_id, designer_id, designer) %>%
                unnest(cols = c("game_id", "designer_id", "designer")) %>%
                arrange(game_id, designer_id, designer)
        
        # game and publishers
        game_publishers <- df_list %>%
                select(game_id, publisher_id, publisher) %>%
                unnest(cols = c("game_id", "publisher_id", "publisher")) %>%
                arrange(game_id, publisher_id)
        
        # game and publishers
        game_artists <- df_list %>%
                select(game_id, artist_id, artist) %>%
                unnest(cols = c("game_id", "artist_id", "artist")) %>%
                arrange(game_id, artist_id)
        
        ### daily pull of games data with timestamp
        game_daily<-games_data %>%
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
        # pivots
        # categories
        if (nrow(game_categories) == 0) {categories_pivot = data.frame(game_id = games_data$game_id)} else {
                
                categories_pivot <- game_categories %>%
                        mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                        mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                        mutate(has_category = 1) %>%
                        select(-category_id) %>%
                        pivot_wider(names_from = c("category"),
                                    values_from = c("has_category"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # mechanics
        if (nrow(game_mechanics) == 0) {mechanics_pivot = data.frame(game_id = games_data$game_id)} else {
                mechanics_pivot = game_mechanics %>%
                        mutate(mechanic = gsub("\\)", "", gsub("\\(", "", mechanic))) %>%
                        mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                        mutate(has_mechanic = 1) %>%
                        select(-mechanic_id) %>%
                        pivot_wider(names_from = c("mechanic"),
                                    values_from = c("has_mechanic"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # designers
        if (nrow(game_designers) == 0) {designers_pivot = data.frame(game_id = games_data$game_id)} else {
                designers_pivot = game_designers %>%
                        mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                        mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                        mutate(has_designer = 1) %>%
                        select(-designer_id) %>%
                        pivot_wider(names_from = c("designer"),
                                    values_from = c("has_designer"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # publishers
        if (nrow(game_publishers) == 0) {publishers_pivot = data.frame(game_id = games_data$game_id)} else {
                publishers_pivot = game_publishers %>%
                        mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                        mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                        mutate(has_publisher = 1) %>%
                        select(-publisher_id) %>%
                        pivot_wider(names_from = c("publisher"),
                                    values_from = c("has_publisher"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        # artists
        if (nrow(game_artists) == 0) {artists_pivot = data.frame(game_id = games_data$game_id)} else {
                artists_pivot = game_artists %>%
                        mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                        mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                        mutate(has_artist = 1) %>%
                        select(-artist_id) %>%
                        pivot_wider(names_from = c("artist"),
                                    values_from = c("has_artist"),
                                    id_cols = c("game_id"),
                                    names_sep = "_",
                                    values_fn = min,
                                    values_fill = 0)
        }
        
        
        # combine into one output
        game_out <- game_daily %>%
                left_join(., categories_pivot,
                          by = "game_id") %>%
                left_join(., mechanics_pivot,
                          by = "game_id") %>%
                left_join(., designers_pivot,
                          by = "game_id") %>%
                left_join(., publishers_pivot,
                          by = "game_id") %>%
                left_join(., artists_pivot,
                          by = "game_id")
        
        return(game_out)
        
}

# dump
dump("get_game_record", file="functions/get_game_record.R")

# function for creating training and test sets from gcp data model
combine_and_split_bgg_datasets = function(datasets_list,
                                   min_users,
                                   year_split,
                                   publisher_list,
                                   top_designers,
                                   top_artists
) {
        
        # combine all
        train = datasets_list$active_games %>%
                select(timestamp, game_id, name, average, baverage, usersrated) %>%
                filter(usersrated > min_users) %>%
                left_join(., games_info %>% # join game info
                                  select(game_id, yearpublished, avgweight, minage, minplayers, maxplayers, playingtime),
                          by = c("game_id")) %>%
                filter(yearpublished <= year_split) %>% # use games prior to 2020 as our training set
                left_join(., game_categories %>% # join categories
                                  mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                                  mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                                  mutate(has_category = 1) %>%
                                  select(-category_id) %>%
                                  pivot_wider(names_from = c("category"),
                                              values_from = c("has_category"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_mechanics %>% # join mechanics
                                  mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                                  mutate(has_mechanic = 1) %>%
                                  select(-mechanic_id) %>%
                                  pivot_wider(names_from = c("mechanic"),
                                              values_from = c("has_mechanic"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_designers %>% # join designers
                                  filter(designer_id %in% top_designers$designer_id) %>%
                                  mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                                  mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                                  mutate(has_designer = 1) %>%
                                  select(-designer_id) %>%
                                  pivot_wider(names_from = c("designer"),
                                              values_from = c("has_designer"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                # get number of designers
                left_join(., datasets_list$game_designers %>% 
                                  group_by(game_id) %>%
                                  summarize(number_designers = n_distinct(designer_id)),
                          by = c("game_id")) %>%
                mutate(number_designers = replace_na(number_designers, 0)) %>%
                left_join(., datasets_list$game_publishers %>% # join publishers
                                  filter(publisher_id %in% publisher_list) %>%
                                  mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                                  mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                                  mutate(has_publisher = 1) %>%
                                  select(-publisher_id) %>%
                                  pivot_wider(names_from = c("publisher"),
                                              values_from = c("has_publisher"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., 
                          datasets_list$game_artists %>%
                                  filter(artist_id %in% top_artists$artist_id) %>%
                                  mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                                  mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                                  mutate(has_artist = 1) %>%
                                  select(-artist_id) %>%
                                  pivot_wider(names_from = c("artist"),
                                              values_from = c("has_artist"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id"))
        
        
        # combine all
        test = datasets_list$active_games %>%
                select(timestamp, game_id, name, average, baverage, usersrated) %>%
                left_join(., games_info %>% # join game info
                                  select(game_id, yearpublished, avgweight, minage, minplayers, maxplayers, playingtime),
                          by = c("game_id")) %>%
                filter(yearpublished > year_split) %>% 
                left_join(., game_categories %>% # join categories
                                  mutate(category = gsub("\\)", "", gsub("\\(", "", category))) %>%
                                  mutate(category = tolower(paste("cat", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
                                  mutate(has_category = 1) %>%
                                  select(-category_id) %>%
                                  pivot_wider(names_from = c("category"),
                                              values_from = c("has_category"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_mechanics %>% # join mechanics
                                  mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
                                  mutate(has_mechanic = 1) %>%
                                  select(-mechanic_id) %>%
                                  pivot_wider(names_from = c("mechanic"),
                                              values_from = c("has_mechanic"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., datasets_list$game_designers %>% # join designers
                                  filter(designer_id %in% top_designers$designer_id) %>%
                                  mutate(designer = gsub("\\)", "", gsub("\\(", "", designer))) %>%
                                  mutate(designer = tolower(paste("des", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", designer))), sep="_"))) %>%
                                  mutate(has_designer = 1) %>%
                                  select(-designer_id) %>%
                                  pivot_wider(names_from = c("designer"),
                                              values_from = c("has_designer"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                # get number of designers
                left_join(., datasets_list$game_designers %>% 
                                  group_by(game_id) %>%
                                  summarize(number_designers = n_distinct(designer_id)),
                          by = c("game_id")) %>%
                mutate(number_designers = replace_na(number_designers, 0)) %>%
                left_join(., datasets_list$game_publishers %>% # join publishers
                                  filter(publisher_id %in% publisher_list) %>%
                                  mutate(publisher = gsub("\\)", "", gsub("\\(", "", publisher))) %>%
                                  mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
                                  mutate(has_publisher = 1) %>%
                                  select(-publisher_id) %>%
                                  pivot_wider(names_from = c("publisher"),
                                              values_from = c("has_publisher"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id")) %>%
                left_join(., 
                          datasets_list$game_artists %>%
                                  filter(artist_id %in% top_artists$artist_id) %>%
                                  mutate(artist = gsub("\\)", "", gsub("\\(", "", artist))) %>%
                                  mutate(artist = tolower(paste("art", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", artist))), sep="_"))) %>%
                                  mutate(has_artist = 1) %>%
                                  select(-artist_id) %>%
                                  pivot_wider(names_from = c("artist"),
                                              values_from = c("has_artist"),
                                              id_cols = c("game_id"),
                                              names_sep = "_",
                                              values_fn = min,
                                              values_fill = 0),
                          by = c("game_id"))
        
        out = list("train" = train,
                   "test" = test)
        
        return(out)
        
}
dump("combine_and_split_bgg_datasets", file="functions/combine_and_split_bgg_datasets.R")

# function for predicting and baking
bake_and_predict_ratings<- function(id,
                                    trained_model) {
        
        require(tidyverse)
        require(magrittr)
        require(tidyverse)
        require(broom)
        require(data.table)
        require(readr)
        require(jsonlite)
        require(rstan)
        require(rstanarm)
        require(recipes)
        require(lubridate)
        
        id = as.integer(id)
        
        source(here::here("deployment/get_game_record.R"))
        
        # get training set
        all_files = list.files(here::here("deployment"))
        files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
        
        # # get dataset
        most_recent_games = all_files[grepl("games_datasets", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get most recent recipe
        most_recent_recipe = all_files[grepl("recipe", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # use function to get record from bgg
        suppressMessages({
                raw_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
        })
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        record=   bind_rows(raw_record,
                            games_datasets$train[0,])
        
        rec <- readr::read_rds(here::here("deployment", most_recent_recipe))
        
        baked_record = bake(rec, record)
        
        req = toJSON(baked_record)
        
        #
        model =  enquo(trained_model)
        model = rlang::sym(paste(trained_model))
        
        # parse example from json
        parsed_example <- jsonlite::fromJSON(req) %>%
                mutate_if(is.integer, as.numeric) %>%
                mutate(timestamp = as_datetime(timestamp))
        
        most_recent_models = all_files[grepl("trained_models_obj", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # get most recent models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        # geek rating
        model_baverage <- models %>%
                filter(outcome_type == 'baverage') %>%
                select(!!model) %>%
                pull()
        
        # get first element
        model_baverage = model_baverage[[1]]
        
        # average rating
        model_average <- models %>%
                filter(outcome_type == 'average') %>%
                select(!!model) %>%
                pull()
        
        # get first element
        model_average = model_average[[1]]
        
        # now predict
        prediction_baverage <- predict(model_baverage, new_data = parsed_example) %>%
                as_tibble() %>%
                set_names("baverage")
        
        prediction_average <- predict(model_average, new_data = parsed_example) %>%
                as_tibble() %>%
                set_names("average")
        
        # now combine
        estimate = dplyr::bind_cols(parsed_example %>%
                                            select(yearpublished, game_id, name),
                                    prediction_baverage,
                                    prediction_average) %>%
                mutate_if(is.numeric, round, 2) %>%
                melt(., id.vars = c("yearpublished", "game_id", "name")) %>%
                rename(outcome = variable,
                       pred = value) %>%
                mutate(method = paste(trained_model)) %>%
                select(method, everything())
        
        out = list("estimate" = estimate,
                   "record" = req)
        
        out
        
}

dump("bake_and_predict_ratings", file="functions/bake_and_predict_ratings.R")

bake_and_predict_posterior <- function(id) {
        
        require(tidyverse)
        require(magrittr)
        require(tidyverse)
        require(broom)
        require(data.table)
        require(readr)
        require(jsonlite)
        require(rstan)
        require(rstanarm)
        require(recipes)
        require(lubridate)
        
        id = as.integer(id)
        
        source(here::here("deployment/get_game_record.R"))
        
        # get training set
        all_files = list.files(here::here("deployment"))
        files = all_files[grepl("games|oos|recipe|preds|models", all_files)]
        
        # # get dataset
        most_recent_games = all_files[grepl("games_datasets", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get most recent recipe
        most_recent_recipe = all_files[grepl("recipe", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # use function to get record from bgg
        suppressMessages({
                raw_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
        })
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        record=   bind_rows(raw_record,
                            games_datasets$train[0,])
        
        rec <- readr::read_rds(here::here("deployment", most_recent_recipe))
        
        baked_record = bake(rec, record)
        
        # to json
        req = toJSON(baked_record)
        
        ###### end getting record
        
        ###### start predicting
        # parse example from json
        # parsed_example <- jsonlite::fromJSON(req) %>%
        #         mutate_if(is.integer, as.numeric) %>%
        #         mutate(timestamp = as_datetime(timestamp))
        
        most_recent_models = all_files[grepl("trained_models_obj", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name3) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rds", ".Rds", path)) %>%
                pull(path)
        
        # get most recent models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        p <- c(0.05, .1, .2, 0.5, 0.8, .9, .95)
        p_names <- purrr::map_chr(p, ~paste0("perc_", .x*100))
        p_funs <- purrr::map(p, ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
                purrr::set_names(nm = p_names)
        
        # parsed_example <- jsonlite::fromJSON(req) %>%
        #         mutate_if(is.integer, as.numeric) %>%
        #         mutate(timestamp = as_datetime(timestamp))
        # 
        # load models
        models = readr::read_rds(here::here("deployment", most_recent_models))
        
        preds_record = models %>%
                select(outcome_type, 
                       stan_lm) %>%
                mutate(stan_lm_fit = map(stan_lm,
                                         ~ .x %>% extract_fit_parsnip())) %>%
                mutate(stan_lm_posterior_preds = map2(.x = stan_lm_fit,
                                                      .y = stan_lm,
                                                      ~ .x$fit %>%
                                                              posterior_predict(.y %>% 
                                                                                        extract_recipe() %>%
                                                                                        bake(baked_record),
                                                                                draws = 1000) %>%
                                                              tidybayes::tidy_draws() %>%
                                                              reshape2::melt(id.vars = c(".chain",
                                                                               ".iteration",
                                                                               ".draw")) %>%
                                                              mutate(.row = as.integer(variable))))
        
        # melted record
        melted_record = baked_record %>%
                select(yearpublished, game_id, name, baverage, average) %>%
                mutate(.row = row_number()) %>%
                reshape2::melt(id.vars = c(".row",
                                           "yearpublished",
                                           "game_id",
                                           "name")) %>%
                rename(outcome = value,
                       outcome_type = variable)
        
        # get sims
        suppressWarnings({
                sims = preds_record %>%
                        select(outcome_type, stan_lm_posterior_preds) %>%
                        unnest() %>%
                        left_join(., melted_record %>%
                                          select(.row, outcome_type, yearpublished, game_id, name, outcome),
                                  by = c("outcome_type", ".row"))

        })
        
        # # get extra info
        # melted_record = baked_record %>%
        #         select(outcome_type, yearpublished, game_id, name, baverage, average) %>%
        #         reshape2::melt(id.vars = c("outcome_type",
        #                                    "yearpublished",
        #                                    "game_id",
        #                                    "name")) %>%
        #         rename(outcome = value,
        #                outcome_type = variable)
        
        
        # sims = sims %>%
        #         nest(-outcome_type) %>%
        #         mutate(outcome = case_when(outcome_type == 'baverage' ~ baked_record$baverage,
        #                                    TRUE ~ baked_record$average)) %>%
        #         mutate(game_id = baked_record$game_id) %>%
        #         mutate(name = baked_record$name) %>%
        #         mutate(yearpublished = baked_record$yearpublished)
        
        out = list("sims" = sims,
                   "baked_record" = baked_record,
                   "melted_record" = melted_record,
                   "record" = req)
        
        return(out)
        
}

dump("bake_and_predict_posterior", file="functions/bake_and_predict_posterior.R")

plot_posterior = function(posterior_preds) {
        
        ### get background data for context
        # get training set
        all_files = list.files(here::here("deployment"))
        
        # # get dataset
        most_recent_games = all_files[grepl("games_datasets", all_files)] %>%
                as_tibble() %>%
                separate(value, c("name1", "name2", "date", "file"), sep = "([._])",
                         extra = "merge",
                         fill = "left") %>%
                unite(name, name1:name2) %>%
                mutate(date = as.Date(date)) %>%
                filter(date == max(date)) %>%
                unite(path, name:file) %>%
                mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
                pull(path)
        
        # get training set
        games_datasets = readr::read_rds(here::here("deployment", most_recent_games))
        
        # quantiles
        baverage_quantiles = quantile(games_datasets$train$baverage, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>%
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'baverage')
        
        # average quanties
        average_quantiles = quantile(games_datasets$train$average, seq(0, 1, .01)) %>%
                as.data.frame() %>%
                rownames_to_column("perc") %>% 
                set_names(., c("perc", "value")) %>%
                mutate(perc = gsub("%", "", perc)) %>%
                mutate(outcome_type = 'average')
        
        quantile_data = list("baverage" = baverage_quantiles,
                             "average" = average_quantiles)
        
        # make a dummy plot
        dummy = data.frame(outcome_type = "baverage",
                           min = 5,
                           max = 9) %>%
                bind_rows(data.frame(outcome_type = "average",
                                     min = 4,
                                     max = 9)) %>%
                melt(id.vars = c("outcome_type")) %>%
                set_names(., c("outcome_type", "variable", "range"))
        
        quantile_dummy = bind_rows(quantile_data$baverage %>%
                                           filter(perc %in% c(50, 90, 99)),
                                   quantile_data$average %>%
                                           filter(perc %in% c(50, 90, 99)))
        
        # make dummy plot
        dummy_plot = ggplot(dummy, aes(x=range))+
                facet_wrap(~outcome_type,
                           ncol = 1,
                           scales = "free_x")+
                theme_phil()+
                xlab("Predicted Value")+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 50),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 90),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_vline(data = quantile_dummy %>%
                                   filter(perc == 99),
                           aes(xintercept=value),
                           col = 'grey10',
                           linetype = 'dotted')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 50),
                          aes(x=value,
                              y = 60),
                          size =2,
                          label = 'median game on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 90),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 10% on bgg')+
                geom_text(data = quantile_dummy %>%
                                  filter(perc == 99),
                          aes(x=value,
                              y = 60),
                          size = 2,
                          label = 'top 1% on bgg')+
                #   coord_cartesian(xlim = c(4, 10))+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())
        
        
        #### add sims from game to plot
        plot_obj = posterior_preds$sims 
        # plot
        plot = dummy_plot + 
                geom_histogram(data = plot_obj,
                               aes(x=value),
                               bins = 100,
                               alpha = 0.7,
                               fill = 'grey60',
                               color = '#F0F0F0')+
                geom_vline(data = plot_obj,
                           aes(xintercept=outcome),
                           alpha=0.9,
                           col = "blue")+
                geom_text(data = plot_obj,
                          aes(x=outcome),
                          label = "current bgg rating",
                          size = 2,
                          y= 30,
                          alpha=0.9,
                          col = "blue")+
                facet_wrap(name~outcome_type,
                           ncol = 1)+
                xlab("Predicted Value")+
                ylab("# of Simulations")+
                coord_cartesian(xlim = c(4, 10),
                                default = T)
        
        
        return(plot)
        
}

dump("plot_posterior", file = "functions/plot_posterior.R")


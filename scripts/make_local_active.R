# load data to active folder

# connect to sql
source(here::here("load_packages.R"))

# push all most recent data to the active folder
# load in the locally saved files
# get unsupervised object previously run
all_files = c(list.files(here::here("deployment")),
              list.files(here::here("local")))
# load in files
most_recent_games = all_files[grepl("games_datasets_2", all_files)] %>%
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

# get most recent version trained
most_recent_unsupervised_obj = all_files[grepl("unsupervised_obj", all_files)] %>%
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

# get most recent neighbors
most_recent_unsupervised_neighbors = all_files[grepl("unsupervised_neighbors", all_files)] %>%
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

# get most recent lattened
most_recent_flattened = all_files[grepl("games_flattened", all_files)] %>%
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

# get most recent clusters
most_recent_clusters = all_files[grepl("unsupervised_clusters", all_files)] %>%
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

#### Models
# complexity
most_recent_complexity_models = all_files[grepl("models_obj_avgweight", all_files)] %>%
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

# ratings
# # get dataset used
most_recent_games_ratings = all_files[grepl("games_datasets_ratings", all_files)] %>%
        as_tibble() %>%
        separate(value, c("name1", "name2","name3", "date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        unite(name, name1:name2) %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, name:file) %>%
        mutate(path = gsub("_Rdata", ".Rdata", path)) %>%
        pull(path)

# get most recent recipe
most_recent_recipe_ratings = all_files[grepl("recipe_ratings", all_files)] %>%
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

# get most recent recipe
most_recent_models_ratings= all_files[grepl("trained_models_obj", all_files)] %>%
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

# get complexity adjusted ratings
adjusted_files = list.files(here::here("adjust_bgg_ratings_data/"))
most_recent_adjusted = adjusted_files %>%
        as_tibble() %>%
        separate(value, c("date", "file"), sep = "([._])",
                 extra = "merge",
                 fill = "left") %>%
        mutate(date = as.Date(date)) %>%
        filter(date == max(date)) %>%
        unite(path, date:file, sep=".") %>%
        pull(path)

# list = ls(pattern = "most_recent_")
# print(list)

### load all
games_datasets = readr::read_rds(here::here("local", most_recent_games))
games = bind_rows(games_datasets$train,
                  games_datasets$test) %>%
        select(-mech_realtime) %>%
        select(-mech_negotiation) %>%
        select(-mech_deduction) %>%
        mutate_if(is.numeric, replace_na, 0) %>%
        filter(avgweight > 0.99) %>%
        filter(!is.na(yearpublished)) %>%
        filter(cat_expansion_for_basegame !=1) %>%
        filter(cat_fan_expansion != 1) %>%
        select(-cat_expansion_for_basegame,
               -cat_fan_expansion)
unsupervised_obj = readr::read_rds(here::here("deployment", most_recent_unsupervised_obj)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
unsupervised_neighbors = readr::read_rds(here::here("deployment", most_recent_unsupervised_neighbors)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
unsupervised_clusters = readr::read_rds(here::here("deployment", most_recent_clusters)) %>%
        filter(dataset == "fundamentals, mechanics, and categories")
recipe_prep = readr::read_rds(here::here("deployment", "unsupervised_recipe_prep.Rdata"))
games_flattened = readr::read_rds(here::here("local", most_recent_flattened)) %>%
        select(game_id,
               name,
               yearpublished,
               average,
               baverage,
               avgweight,
               playingtime,
               usersrated,
               minplayers,
               maxplayers)
adjusted_ratings = fread(here::here("adjust_bgg_ratings_data", most_recent_adjusted)) %>%
        as_tibble()
playercounts = readr::read_rds(here::here("local", "playercounts.Rdata"))
models_complexity = readr::read_rds(here::here("deployment", most_recent_complexity_models))
models_ratings = readr::read_rds(here::here("deployment", most_recent_models_ratings))
recipe_ratings = readr::read_rds(here::here("deployment", most_recent_recipe_ratings))
games_datasets_ratings = readr::read_rds(here::here("deployment", most_recent_games_ratings))

# load others
publisher_list = readr::read_rds("local/publisher_list.Rdata")
top_designers = readr::read_rds("local/top_designers.Rdata")
top_artists = readr::read_rds("local/top_artists.Rdata")
playercounts = readr::read_rds("local/playercounts.Rdata")

# trim down to only what is needed
unsupervised_obj_light = unsupervised_obj %>%
        select(dataset, pca_with_data) %>%
        unnest()

# trim down to component loadings
unsupervised_obj_components = unsupervised_obj %>% 
        mutate(pca_components = map(pca_trained, ~ .x %>% 
                                            tidy(id = "pca"))) %>%
        select(dataset, pca_components) %>%
        unnest()

# now write all to local
readr::write_rds(games, file = here::here("active/games.Rdata"))
readr::write_rds(adjusted_ratings, file = here::here("active/adjusted_ratings.Rdata"))
readr::write_rds(games_datasets, file = here::here("active/games_datasets.Rdata"))
readr::write_rds(games_flattened, file = here::here("active/games_flattened.Rdata"))
readr::write_rds(unsupervised_obj, file = here::here("active/unsupervised_obj.Rdata"))
readr::write_rds(unsupervised_clusters, file = here::here("active/unsupervised_clusters.Rdata"))
readr::write_rds(unsupervised_obj_light, file = here::here("active/unsupervised_obj_light.Rdata"))
readr::write_rds(unsupervised_obj_components, file = here::here("active/unsupervised_obj_components.Rdata"))
readr::write_rds(unsupervised_neighbors, file = here::here("active/unsupervised_neighbors.Rdata"))
readr::write_rds(recipe_prep,  file = here::here("active/unsupervised_recipe_prep.Rdata"))
readr::write_rds(playercounts, file = here::here("active/publisher_list.Rdata"))
readr::write_rds(top_designers, file = here::here("active/top_designers.Rdata"))
readr::write_rds(top_artists, file = here::here("active/top_artists.Rdata"))
readr::write_rds(playercounts, file = here::here("active/playercounts.Rdata"))
readr::write_rds(models_complexity, file = here::here("active/models_complexity.Rds"))
readr::write_rds(models_ratings, file = here::here("active/models_ratings.Rds"))
readr::write_rds(recipe_ratings, file = here::here("active/recipe_ratings.Rdata"))
readr::write_rds(games_datasets_ratings, file = here::here("active/games_datasets_ratings.Rdata"))


print("files loaded to local")

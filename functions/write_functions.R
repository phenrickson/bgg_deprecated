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

# function for pulling collection and making a data frame
get_collection<-function(username_string) {
        
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
        
        return(collection_data)
        
}

# dump
dump("get_collection", file="functions/get_collection.R")

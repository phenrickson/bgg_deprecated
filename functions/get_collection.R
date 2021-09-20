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
        
        return(collection_data)
        
}

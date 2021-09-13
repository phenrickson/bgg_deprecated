# who: phil henrickson
# what: functions to be used in project

# function to read data from github repository
get_bgg_data_from_github<-function(input_date) {
        
        require(tidyverse)
        
        url = paste("https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/", input_date, ".csv", sep="")
        
        data <- read_csv(url,
                         show_col_types = F) %>%
                mutate(date = input_date,
                       ID = as.integer(ID),
                       github_url = url) %>%
                rename(bgg_game_id = ID,
                       game_name = Name,
                       game_release_year = Year,
                       bgg_rank = Rank,
                       bgg_average = Average,
                       bgg_bayes_average = `Bayes average`,
                       bgg_users_rated = `Users rated`,
                       bgg_url = URL,
                       thumbnail = Thumbnail) %>%
                select(date, everything())
        
        return(data)
        
}

# dump
dump("get_bgg_data_from_github", file="get_bgg_data_from_github.R")

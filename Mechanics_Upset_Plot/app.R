#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(UpSetR)
library(rsconnect)
source("load_packages.R")
source("theme_phil.R")

### Connect to BigQuery
library(bigrquery)

# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# establish connection
bigquerycon<-dbConnect(
    bigrquery::bigquery(),
    project = PROJECT_ID,
    dataset = "bgg"
)

# query table
active_games<-DBI::dbGetQuery(bigquerycon, 
                              'SELECT * FROM bgg.active_games_daily')

game_info<-DBI::dbGetQuery(bigquerycon,
                           'SELECT 
                          a.game_id,
                          a.name,
                          b.yearpublished
                          FROM bgg.game_ids a
                          LEFT JOIN bgg.game_yearpublished b
                          ON a.game_id = b.game_id')

game_mechanics<-DBI::dbGetQuery(bigquerycon, 
                                'SELECT 
                              a.game_id,
                              b.mechanic_id,
                              b.mechanic
                              FROM bgg.game_mechanics a
                               LEFT JOIN bgg.mechanic_ids b 
                               ON a.mechanic_id = b.mechanic_id')

game_publishers<-DBI::dbGetQuery(bigquerycon, 
                                 'SELECT 
                              a.game_id,
                              b.publisher_id,
                              b.publisher
                              FROM bgg.game_publishers a
                               LEFT JOIN bgg.publisher_ids b 
                               ON a.publisher_id = b.publisher_id')

game_designers<-DBI::dbGetQuery(bigquerycon, 
                                'SELECT 
                              a.game_id,
                              b.designer_id,
                              b.designer
                              FROM bgg.game_designers a
                               LEFT JOIN bgg.designer_ids b 
                               ON a.designer_id = b.designer_id')

game_categories<-DBI::dbGetQuery(bigquerycon, 
                                'SELECT 
                              a.game_id,
                              b.category_id,
                              b.category
                              FROM bgg.game_categories a
                               LEFT JOIN bgg.category_ids b 
                               ON a.category_id = b.category_id')


### Pivot Tables for Upset Plot
publishers_pivot<-game_publishers %>%
    # filter(game_id == 124361) %>%
 #   mutate(publisher = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", publisher))), sep="_"))) %>%
    mutate(has_publisher = 1) %>%
    select(-publisher_id) %>%
    pivot_wider(names_from = c("publisher"),
                values_from = c("has_publisher"),
                id_cols = c("game_id"),
                names_sep = "_",
                values_fn = min,
                values_fill = 0)

mechanics_pivot<-game_mechanics %>%
    #       filter(game_id == 124361) %>%
    #   mutate(mechanic = tolower(paste("mech", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", mechanic))), sep="_"))) %>%
    mutate(has_mechanic = 1) %>%
    select(-mechanic_id) %>%
    pivot_wider(names_from = c("mechanic"),
                values_from = c("has_mechanic"),
                id_cols = c("game_id"),
                names_sep = "_",
                values_fill = 0) %>%
    as.data.frame()

categories_pivot<-game_categories %>%
    # filter(game_id == 124361) %>%
    #   mutate(category = tolower(paste("pub", gsub("[[:space:]]", "_", gsub("\\s+", " ", gsub("[[:punct:]]","", category))), sep="_"))) %>%
    mutate(has_category = 1) %>%
    select(-category_id) %>%
    pivot_wider(names_from = c("category"),
                values_from = c("has_category"),
                id_cols = c("game_id"),
                names_sep = "_",
                values_fn = min,
                values_fill = 0)
### names
# category name inputs
category_names<-game_categories %>%
    group_by(category, category_id) %>%
    summarize(games = n_distinct(game_id),
              .groups = 'drop') %>%
    arrange(desc(games)) %>%
    filter(!grepl("(", fixed=T, category)) %>%
    pull(category)

# mechanic name inputs
mechanic_names<-game_mechanics %>%
    pull(mechanic) %>%
    unique()

# publisher name inputs
publisher_names<-game_publishers %>%
    group_by(publisher, publisher_id) %>%
    summarize(games = n_distinct(game_id),
              .groups = 'drop') %>%
    arrange(desc(games)) %>%
    filter(!grepl("(", fixed=T, publisher)) %>%
    pull(publisher)


# input data
input_data <- mechanics_pivot %>%
    left_join(., publishers_pivot) %>%
    left_join(., active_games %>%
                  select(game_id,  yearpublished, rank, usersrated))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Board Game Mechanic Combinations"),
    
    # Input: Slider for the number of bins ----
    sliderInput(inputId = "sets",
                label = "Number of Mechanics",
                min = 5,
                step = 5,
                max = 25,
                value = 10),
    
    # # select category
    # selectInput(inputId = "category",
    #             label = "Category of Games",
    #             #     selected = category_names[3],
    #             choices = category_names,
    #             multiple = F,
    #             tableOutput("data")),
    # 
    # select publisher
    selectInput(inputId = "publisher",
                   label = "Publisher:",
           #     selected = publisher_names[3],
                 choices = publisher_names,
                multiple = F,
                tableOutput("data")),
    
        # Show a plot of the generated distribution
        # 
        # 
        mainPanel(
           plotOutput("upsetPlot",
                      height = '800px',
                      width = '1600px')
        ),
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$upsetPlot <- renderPlot({
        
        # get filtered data
        filtered_data<-input_data %>%
            filter(!!rlang::sym(input$publisher) == 1)
        
        # generate bins based on input$bins from ui.R
        upset(filtered_data %>% 
                  select(one_of(mechanic_names)),
              #           sets=mechanic_names,
              nsets = input$sets,
              show.numbers = F,
                 text.scale = 3,
                 point.size = 4,
                  line.size=1.5,
              #    nintersects = 50,
              mb.ratio = c(.4, .6),
              mainbar.y.label = "# of Games with \n Combination of Mechanics",
              sets.x.label = "# of Games with Mechanic",
              order.by = "freq")
    })
    
    # updateSelectizeInput(session,
    #                      'publisher', 
    #                      choices = publisher_names, 
    #                      server = TRUE)
    # 

}

# Run the application 
shinyApp(ui = ui, server = server)

#deployApp()


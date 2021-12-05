get_game_comparables <-
function(id) {
        
        # load function
        source("functions/get_game_record.R")
        
        # get unsupervised object previously run
        all_files = list.files(here::here("deployment"))
        files = all_files[grepl("unsupervised", all_files)]
        
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
        
        # load
        unsupervised_obj = readr::read_rds(here::here("deployment", most_recent_unsupervised_obj))
        unsupervised_neighbors = readr::read_rds(here::here("deployment", most_recent_unsupervised_neighbors))
        recipe_prep = readr::read_rds(here::here("deployment", "unsupervised_recipe_prep.Rdata"))
        
        # check to see if id is in the unsupervised object
        check_obs = unsupervised_obj %>%
                select(dataset, pca_with_data) %>%
                unnest(c(dataset, pca_with_data)) %>%
                filter(game_id == id) %>%
                nrow()
        
        # get game name
        game = unsupervised_neighbors %>%
                filter(game_id == id) %>%
                pull(name) %>%
                unique()
        
        # get table of games
        active_games = unsupervised_obj[1,] %>% select(pca_with_data) %>% unnest() %>%
                arrange(desc(baverage)) %>%
                mutate(rank = row_number())
        
        ### if game record is in our previously run analysis, we can just look it up
        
        if(check_obs > 1) {
                
                neighbors_table = unsupervised_neighbors %>%
                        filter(game_id == id) %>%
                        #    filter(dataset == 'fundamentals, mechanics, and categories') %>%
                        select(dataset, game_id, name, neighbor_game_id, neighbor_name, similarity, dist, perc, yearpublished, rank, average, baverage, avgweight) %>%
                        filter(yearpublished < 2021) %>%
                        mutate(game_id = as.character(game_id),
                               neighbor_game_id = as.character(neighbor_game_id),
                               yearpublished = as.character(yearpublished)) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage) %>%
                        group_by(dataset) %>%
                        arrange(dataset, dist) %>%
                        mutate(rank = row_number()) %>%
                        ungroup() %>%
                        filter(rank <=25) %>%
                        rename(Comparing_By = dataset,
                               ID = neighbor_game_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor_name,
                               Rank = rank) %>%
                        ungroup() %>%
                        select(Game, Comparing_By, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2) %>%
                        arrange(desc(Comparing_By)) 
                
        } else {
                
                paste("game not in existing dataset; pulling game info from BGG and calculating...")
                
                # if the game isn't present, we need to go grab it and then add it to our existing games
                game_record = get_game_record(id) %>%
                        mutate(number_designers = rowSums(across(starts_with("des_"))))
                
                game = game_record %>%
                        pull(name)
                
                # nest for placemen
                # bak
                baked_game = recipe_prep %>%
                        prep(recipe_prep$template, strings_as_factor = F) %>%
                        bake(new_data = bind_rows(game_record, recipe_prep$template[0,]))
                
                # nest
                nested_game_data<- baked_game %>%
                        mutate(dataset = "fundamentals, mechanics, and categories") %>%
                        nest(-dataset) %>%
                        bind_rows(., baked_game %>%
                                          mutate(dataset = "fundamentals and mechanics") %>%
                                          select(-starts_with("cat_"),
                                                 -number_categories) %>%
                                          nest(-dataset))
                
                ### get comparables for game
                comps_obj = unsupervised_obj %>%
                        select(dataset, pca_trained) %>% # get pca
                        left_join(., nested_game_data,
                                  by = "dataset") %>%
                        mutate(pca_rotation = map2(.x = pca_trained,
                                                   .y = data,
                                                   ~ .x %>% bake(new_data = .y))) %>%
                        mutate(pca_with_data = map2(.x = pca_rotation,
                                                    .y = data,
                                                    ~ .x %>%
                                                            select(game_id, starts_with("PC")) %>%
                                                            left_join(., .y,
                                                                      by = "game_id"))) %>%
                        mutate(type = "game") %>%
                        select(dataset, pca_with_data, type) %>%
                        bind_rows(., unsupervised_obj %>%
                                          select(dataset, pca_with_data) %>%
                                          unnest() %>%
                                          filter(game_id != id) %>%
                                          nest(-dataset, -type) %>%
                                          rename(pca_with_data = data)) %>%
                        mutate(pca_with_data = map(pca_with_data, ~.x %>%
                                                           rename_all(funs(gsub("PC0", "PC", gsub("PC00", "PC", make.names(names(.x)))))))) %>%
                        unnest() %>%
                        nest(-dataset) %>%
                        rename(pca_with_data = data)
                
                # now combine
                game_comps = comps_obj %>%
                        left_join(., 
                                  unsupervised_obj %>%
                                          select(dataset, kmeans, norm_trained, pca_trained),
                                  by = "dataset") %>%
                        mutate(pca_dist = map(.x = pca_with_data, 
                                              ~ dist(.x %>%
                                                             select(PC1:PC10) %>%
                                                             as.matrix(), 
                                                     method="euclidean") %>%
                                                      as.matrix() %>%
                                                      as.data.frame() %>%
                                                      magrittr::set_rownames(.x$game_id) %>%
                                                      magrittr::set_colnames(.x$game_id))) %>%
                        mutate(obs_dist = map(pca_dist, ~ .x %>%
                                                      rownames_to_column("game_id") %>%
                                                      filter(game_id == id) %>%
                                                      gather('closest','dist',-game_id) %>%
                                                      filter(dist > 0) %>%
                                                      filter(!is.na(dist)) %>% 
                                                      group_by(game_id) %>% 
                                                      arrange(dist) %>% 
                                                      slice_min(dist, n=50, with_ties = T) %>%
                                                      mutate(dist_rank=row_number()))) %>%
                        mutate(neighbors = map(obs_dist,
                                               ~ left_join(.x, game_record %>%
                                                                   mutate(game_id = as.character(game_id)),
                                                           by = c("game_id")) %>%
                                                       select(game_id, name, closest, dist, dist_rank) %>% 
                                                       left_join(., active_games %>%
                                                                         mutate(game_id = as.character(game_id)) %>%
                                                                         rename(neighbor_game_id = game_id,
                                                                                neighbor_name = name),
                                                                 by = c("closest" = "neighbor_game_id")))) %>%
                        mutate(scale_data = map2(.x = norm_trained,
                                                 .y = pca_with_data,
                                                 ~ .x %>% bake(new_data = .y %>%
                                                                       select(-starts_with("PC")) %>%
                                                                       filter(game_id == id)) %>%
                                                         select(-timestamp,
                                                                -game_id,
                                                                -name,
                                                                -average,
                                                                -baverage,
                                                                -usersrated,
                                                                -yearpublished))) %>%
                        mutate(clusters = map2(.x = kmeans,
                                               .y = scale_data,
                                               ~ clue::cl_predict(.x, 
                                                                  newdata = .y)))
                
                # extract neighbors and report
                neighbors_table= game_comps %>%
                        select(dataset, neighbors) %>% 
                        unnest() %>%
                        rename(neighbor = neighbor_name,
                               neighbor_id = closest) %>%
                        mutate(similarity = 100*1/(1+ sqrt(dist))) %>%
                        select(dataset, game_id, name, neighbor, neighbor_id, similarity, dist, dist_rank) %>%
                        left_join(., active_games %>%
                                          mutate(game_id = as.character(game_id)) %>%
                                          rename(neighbor_id = game_id,
                                                 neighbor = name),
                                  by = c("neighbor_id", "neighbor")) %>%
                        rename(BGGRank = rank,
                               BGGRating = average,
                               GeekRating = baverage)  %>%
                        filter(yearpublished < 2022) %>%
                        group_by(dataset) %>%
                        mutate(rank = row_number()) %>%
                        filter(rank <=25) %>%
                        ungroup() %>%
                        mutate_if(is.numeric, round, 2) %>%
                        #  select(-game_id, -similarity, -dist, -dist_rank) %>%
                        mutate(yearpublished = as.character(yearpublished),
                               neighbor_id = as.character(neighbor_id)) %>%
                        rename(Comparing_By = dataset,
                               ID = neighbor_id,
                               Complexity = avgweight,
                               Game = name,
                               Published = yearpublished,
                               Neighbor = neighbor,
                               Rank = rank) %>%
                        select(Comparing_By, Rank, ID, Published, Neighbor, BGGRating, GeekRating, Complexity) %>%
                        mutate_if(is.numeric, round, 2) %>%
                        arrange(desc(Comparing_By))
                
        }
        
        # color functions for flextable
        # geek rating
        baverage_func<- function(x) {
                
                breaks = seq(5, 8.6, 0.1)
                colorRamp=colorRampPalette(c("white", "deepskyblue1"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=T,
                             label = FALSE)
                col_palette[mycut]
                
        }
        
        # avg rating
        average_func<- function(x) {
                
                breaks = seq(4, 9.9, 0.1)
                colorRamp=colorRampPalette(c("white", "deepskyblue1"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=T,
                             label = FALSE)
                col_palette[mycut]
                
        }
        
        # avgweight
        avgweight_func<- function(x) {
                
                breaks<-seq(1, 5, 0.1)
                #  breaks = weight_deciles
                colorRamp=colorRampPalette(c("white", "red"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=T,
                             label = FALSE)
                col_palette[mycut]
                
        }
        
        # convert to flextable
        neighbors_table_ft = neighbors_table %>%
                filter(Rank <=10) %>%
                flextable() %>%
                flextable::autofit() %>%
                set_caption(paste("Comparables games to", game, sep=" ")) %>%
                bg(., i = ~ Comparing_By =='fundamentals, mechanics, and categories',
                   bg = 'grey100') %>%
                bg(., i = ~ Comparing_By == 'fundamentals and mechanics',
                   bg = 'grey90') %>%
                bg(., j = c("GeekRating"),
                   bg = baverage_func) %>%
                bg(., j = c("BGGRating"),
                   bg = average_func) %>%
                bg(., j = c("Complexity"),
                   bg = avgweight_func) 
        
        
        # now make plot
        # make visualization to compare observations on the principal components
        df = unsupervised_obj[1,]$pca_with_data[[1]] %>%
                select(game_id, name, PC1:PC10)
        
        plot_df = df %>%
                melt(., id.vars = c("game_id", "name")) %>%
                mutate(variable = case_when(variable == 'PC1' ~ 'PC1_Complexity',
                                            variable == 'PC2' ~ 'PC2_Thematic',
                                            variable == 'PC3' ~ 'PC3_Economy',
                                            variable == 'PC4' ~ 'PC4_Cooperation')) %>%
                filter(!is.na(variable))
        
        # jitter
        pos <- position_jitter(width = 0.15, seed = 2)
        pos2 <- position_jitter(width = 0.075, seed = 2)
        
        # make background plot
        background = plot_df %>%
                ggplot(., aes(x=variable,
                              y = value))+
                geom_jitter(alpha=0.05,
                            col = 'grey60',
                            position = pos)+
                theme_phil()+
                geom_hline(yintercept = 0,
                           linetype = 'dashed',
                           alpha = 0.8)+
                theme(legend.position = 'top',
                      legend.title = element_blank())+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())+
                ggtitle(paste("Which games are similar to ", game, "?", sep= ""),
                        subtitle = str_wrap("Placing games on first four principal components of variation: complexity, theme, economy, and coooperation.", 125))
        
        compare = c(neighbors_table %>%
                            filter(Rank < 5) %>%
                            pull(ID))
        
        
        compare_plot = background + 
                # geom_jitter(data = plot_df %>%
                #                           filter(game_id %in% id),
                #                   aes(x = variable,
                #                       size = highlight,
                #                       y = value),
                #             color = "black",
                #             size = 2.5,
                #             position = pos2)+
                geom_jitter(data = plot_df %>%
                                    filter(game_id %in% c(id, compare)),
                            aes(x = variable,
                                color = name,
                                y = value),
                            size = 2,
                            position = pos2)+
                geom_label_repel(data = plot_df %>%
                                         filter(game_id %in% c(id,compare)),             
                                 aes(x = variable,
                                     color = name,
                                     y=value,
                                     label = name),
                                 position = pos2,
                                 max.overlaps=25,
                                 show.legend=F,
                                 size = 3)+
                guides(label = "none",
                       color = "none",
                       size = "none")+
                scale_color_viridis_d(option = "plasma",
                                      begin = 0.05,
                                      end = 0.8)
        
        out = list("neighbors_table" = neighbors_table_ft,
                   "neighbors_data" = neighbors_table,
                   "neighbors_plot" = compare_plot)
        
        return(out)
        
}

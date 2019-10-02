library(mongolite)
library(gridExtra)
library(ggthemes)
library(flexdashboard)
library(grid)
library(ggpubr)
library(scales)
library(bbplot)
library(Hmisc)
library(tidyverse)
library(knitr)
library(DecisionAnalysis)


get.products <-  function(database,
                          mongo_uri,
                          filter = {
                            
                          },
                          datafile = '/tmp/data.csv') {
  products.df <- tryCatch({
    products.col <- mongo("products", database, url = mongo_uri)
    products.df <- products.col$find(filter) %>% as_tibble()
    products.df %>% write_csv(datafile)
  },
  error = function(e) {
    return(read_csv(datafile))
  })
  
  
  
  products.df <-
    products.df %>% mutate(
      price_start = replace_na(price_start, 0),
      score_num = replace_na(score_num, 0),
      revenue = price_start * score_num,
      title_length = nchar(title)
    ) %>%
    mutate(
      gig_id = as.integer(gig_id),
      seller_level_2 = if_else(
        seller_level == 'Top Rated Seller' |
          seller_level == 'Level 2 Seller' |
          seller_level == 'Level 1 Seller',
        "Rated Seller",
        "Unrated Seller",
        "Unrated Seller"
      ),
      seller_level_3 = if_else(seller_level == 'New Arrival' , "New", "Old", "Old")
    )
  
  return(products.df)
}

get.seller.levels <-  function(database,
                               mongo_uri,
                               filter = {
                                 
                               },
                               datafile = '/tmp/data.csv') {
  seller.levels.df <-
    get.products(database, mongo_uri, filter, datafile) %>% select(keyword, seller_level) %>%
    mutate(seller_level = if_else(is.na(seller_level), 'None', seller_level)) %>%
    group_by(keyword, seller_level) %>%
    tally() %>%
    spread(seller_level, n) %>%
    replace(., is.na(.), 0)
  return(seller.levels.df)
  
}

get.market.entry.data.1 <-
  function(database,
           mongo_uri,
           filter = {
             
           },
           datafile = '/tmp/data.csv') {
    products.df <- get.products(database, mongo_uri, filter, datafile)
    
    market.entry.barrier.data.1 <- products.df %>%
      mutate(seller_level = factor(replace_na(seller_level, 'None'),
                                   levels = rev(
                                     c(
                                       'None',
                                       'New Arrival',
                                       'Level 1 Seller',
                                       'Level 2 Seller',
                                       'Top Rated Seller'
                                     )
                                   ))) %>%
      group_by(keyword) %>%
      mutate(total = n()) %>%
      group_by(keyword, seller_level, total) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(pct = n / total) %>%
      select(keyword, seller_level, pct)
    return(market.entry.barrier.data.1)
  }


get.market.entry.data.2 <-
  function(database,
           mongo_uri,
           filter = {
             
           },
           datafile = '/tmp/data.csv') {
    products.df <- get.products(database, mongo_uri, filter, datafile)
    market.entry.barrier.data.2 <- products.df %>%
      mutate(seller_level_2 = factor(seller_level_2,
                                     levels = rev(c(
                                       'Unrated Seller', 'Rated Seller'
                                     )))) %>%
      group_by(keyword) %>%
      mutate(total = n()) %>%
      group_by(keyword, seller_level_2, total) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(pct = n / total)  %>%
      
      select(keyword, seller_level_2, pct)
    return(market.entry.barrier.data.2)
    
  }

get.market.entry.data.3 <-
  function(database,
           mongo_uri,
           filter = {
             
           },
           datafile = '/tmp/data.csv') {
    products.df <- get.products(database, mongo_uri, filter, datafile)
    market.entry.barrier.data.3 <- products.df %>%
      mutate(seller_level_3 = factor(seller_level_3,
                                     levels = rev(c('New', 'Old')))) %>%
      group_by(keyword) %>%
      mutate(total = n()) %>%
      group_by(keyword, seller_level_3, total) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(pct = n / total)  %>%
      select(keyword, seller_level_3, pct)
    return(market.entry.barrier.data.3)
  }

get.niches <- function(database,
                       mongo_uri,
                       filter = {
                         
                       },
                       datafile = '/tmp/data.csv') {
  niches.df <-
    get.products(database, mongo_uri, filter, datafile) %>% select(keyword,
                                                                   price_start,
                                                                   score,
                                                                   score_num,
                                                                   seller_level,
                                                                   results_count,
                                                                   revenue) %>%
    group_by(keyword) %>%
    summarise(
      price_start_median = median(price_start, na.rm = T),
      price_start_mean = mean(price_start, na.rm = T),
      results_count = first(results_count),
      revenue_sum = sum(revenue),
      revenue_median = median(revenue),
      revenue_mean = mean(revenue),
      score_median = median(score, na.rm = T),
      score_sum = sum(score_num, na.rm = T),
      score_num_median = median(score_num, na.rm = T)
    ) %>%
    mutate(
      demand_supply_index = score_sum / results_count,
      competition_index = results_count / score_sum
    )  %>%
    arrange(desc(demand_supply_index)) %>%
    inner_join(get.seller.levels(database, mongo_uri, datafile = datafile),
               by = 'keyword') %>%
    inner_join(
      get.market.entry.data.2(database, mongo_uri, datafile = datafile) %>% filter(seller_level_2 == 'Unrated Seller') %>% select(keyword, pct) %>% rename(unrated_seller_ratio =
                                                                                                                                                             pct)
    ) %>%
    mutate(
      competition_score = SAVF_exp_score(
        competition_index,
        min(competition_index),
        mean(competition_index),
        max(competition_index),
        F
      ),
      revenue_score = SAVF_exp_score(
        revenue_sum,
        min(revenue_sum),
        mean(revenue_sum),
        max(revenue_sum),
        T
      ),
      market_entry_barrier_score = SAVF_linear_score(
        unrated_seller_ratio,
        min(unrated_seller_ratio),
        mean(unrated_seller_ratio),
        max(unrated_seller_ratio),
        T
      )
    )
  
  overall.score.data <- MAVF_Scores(
    niches.df %>%
      select(
        competition_score,
        revenue_score,
        market_entry_barrier_score
      ) %>% as.matrix(),
    c(0.3, 0.5, 0.2),
    niches.df %>% select(keyword) %>% as.matrix()
  ) %>%
    as_tibble() %>% rename(overall_score = Score, keyword = Name) %>% mutate(overall_score = overall_score *
                                                                               100)
  
  niches.df <- niches.df %>% inner_join(overall.score.data)
  rm(overall.score.data)
  rm(seller.levels.df)
  return(niches.df)
}
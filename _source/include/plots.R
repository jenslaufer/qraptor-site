extra.theme <-
  bbc_style() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.justification = "right",
    plot.caption = element_text(size = 16)
  )



get.demand.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    demand <- get.niches(database, db_uri, filter, datafile) %>%
      ggplot() +
      geom_bar(aes(x = reorder(keyword, -score_sum), y = score_sum),
               fill = '#4E79A7',
               stat = 'identity') +
      labs(
        title = 'Demand',
        subtitle = 'Total number of Reviews on page one',
        y = '',
        x = ''
      ) +
      geom_label(
        aes(
          x = reorder(keyword, score_sum),
          y = score_sum,
          label = score_sum
        ),
        hjust = 0.5,
        vjust = 0,
        colour = "black",
        fill = NA,
        label.size = NA,
        family = "Helvetica",
        size = 7
      ) + extra.theme
    
    if (!is.null(filename)) {
      demand %>% ggsave(filename = filename)
    }
    
    return(demand)
  }

get.supply.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    supply <- get.niches(database, db_uri, filter, datafile) %>%
      ggplot() +
      geom_bar(aes(x = reorder(keyword, results_count), y = results_count),
               fill = '#4E79A7',
               stat = 'identity') +
      labs(
        title = 'Supply',
        subtitle = 'Total number of Fiver gigs in each niche',
        y = '',
        x = ''
      ) +
      geom_label(
        aes(
          x = reorder(keyword, results_count),
          y = results_count,
          label = results_count
        ),
        hjust = 0.5,
        vjust = 0,
        colour = "black",
        fill = NA,
        label.size = NA,
        family = "Helvetica",
        size = 7
      ) +
      annotation_custom(
        textGrob(cpation.text, gp = gpar(col = "blue")),
        xmin = max(mtcars$mpg),
        xmax = max(mtcars$mpg),
        ymin = 0.6 * min(mtcars$wt),
        ymax = 0.6 * min(mtcars$wt)
      ) + extra.theme
    
    if (!is.null(filename)) {
      supply %>% ggsave(filename = filename)
    }
    
    return(supply)
  }


get.competition.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    competition <- get.niches(database, db_uri, filter, datafile) %>%
      ggplot() +
      geom_bar(aes(x = reorder(keyword, competition_index), y = competition_index),
               fill = '#4E79A7',
               stat = 'identity') +
      labs(
        title = 'Competition',
        subtitle = 'Ratio between Number Of Gigs and Total Reviews on page one',
        y = '',
        x = ''
      ) +
      geom_label(
        aes(
          x = reorder(keyword, competition_index),
          y = competition_index,
          label = round(competition_index, 2)
        ),
        hjust = 0.5,
        vjust = 0,
        colour = "black",
        fill = NA,
        label.size = NA,
        family = "Helvetica",
        size = 7
      ) + extra.theme
    
    if (!is.null(filename)) {
      competition %>% ggsave(filename = filename)
    }
    
    return(competition)
  }

get.market.entry.data.1.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    market.entry.barrier.1 <-
      get.market.entry.data.1(database, db_uri, filter, datafile) %>%
      ggplot(aes(
        x = keyword,
        y = pct,
        fill = seller_level,
        label = paste(round(pct * 100, 1), "%", sep = '')
      )) +
      geom_bar(stat = 'identity') +
      geom_text(size = 8, position = position_stack(vjust = 0.5)) +
      scale_fill_tableau() +
      coord_flip() +
      labs(title = 'Market Entry Barrier', subtitle = 'Percentage of sellers for the different seller levels') +
      guides(fill = guide_legend(reverse = TRUE)) + extra.theme +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank()
      )
    
    if (!is.null(filename)) {
      market.entry.barrier.1 %>% ggsave(filename = filename)
    }
    
    return(market.entry.barrier.1)
  }

get.market.entry.data.2.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    market.entry.barrier.data.2 <-
      get.market.entry.data.2(database, db_uri, filter, datafile)
    lev <-
      (
        market.entry.barrier.data.2 %>% filter(seller_level_2 == 'Unrated Seller') %>% arrange(pct) %>% select(keyword)
      )$keyword
    
    market.entry.barrier.2 <- market.entry.barrier.data.2   %>%
      ggplot(aes(
        x = keyword,
        y = pct,
        fill = seller_level_2,
        label = paste(round(pct * 100, 1), "%", sep = '')
      )) +
      geom_bar(stat = 'identity') +
      geom_text(size = 8, position = position_stack(vjust = 0.5)) +
      scale_fill_tableau() +
      scale_x_discrete(limits = lev) +
      coord_flip() +
      labs(title = 'Market Entry Barrier', subtitle = 'Unrated sellers vs rated sellers on page one') +
      guides(fill = guide_legend(reverse = TRUE)) + extra.theme +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank()
      )
    
    
    if (!is.null(filename)) {
      market.entry.barrier.2 %>% ggsave(filename = filename)
    }
    
    return(market.entry.barrier.2)
  }


get.market.entry.data.3.plot <-
  function(database,
           db_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    market.entry.barrier.data.3 <-
      get.market.entry.data.3(database, db_uri, filter, datafile)
    lev <-
      (
        market.entry.barrier.data.3 %>% filter(seller_level_3 == 'Old') %>% arrange(desc(pct)) %>% select(keyword)
      )$keyword
    
    market.entry.barrier.3 <- market.entry.barrier.data.3   %>%
      ggplot(aes(
        x = keyword,
        y = pct,
        fill = seller_level_3,
        label = paste(round(pct * 100, 1), "%", sep = '')
      )) +
      geom_bar(stat = 'identity') +
      geom_text(size = 8, position = position_stack(vjust = 0.5)) +
      scale_fill_tableau() +
      scale_x_discrete(limits = lev) +
      coord_flip() +
      labs(title = 'Market Entry Barrier', subtitle = 'New Gigs vs Old Gigs on page one') +
      guides(fill = guide_legend(reverse = TRUE)) + extra.theme +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank()
      )
    
    if (!is.null(filename)) {
      market.entry.barrier.3 %>% ggsave(filename = filename)
    }
    
    return(market.entry.barrier.3)
  }

get.revenue.potential.plot <-
  function(database,
           mongo_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    revenue.potential <-
      get.niches(database , mongo_uri , filter, datafile) %>%
      ggplot() +
      geom_bar(aes(x = reorder(keyword, -revenue_mean), y = revenue_mean),
               fill = '#4E79A7',
               stat = 'identity') +
      labs(
        title = 'Revenue potential',
        subtitle = 'Average reviewed revenue in each niche in €',
        y = '',
        x = ''
      ) +
      geom_label(
        aes(
          x = reorder(keyword, revenue_mean),
          y = revenue_mean,
          label = round(revenue_mean, 0)
        ),
        hjust = 0.5,
        vjust = 0,
        colour = "black",
        fill = NA,
        label.size = NA,
        family = "Helvetica",
        size = 7
      ) + extra.theme
    
    if (!is.null(filename)) {
      revenue.potential %>% ggsave(filename = filename)
    }
    
    return(revenue.potential)
  }

get.overall.score.plot <-
  function(database,
           mongo_uri,
           filter,
           filename = NULL,
           datafile = '/tmp/data.csv') {
    overall.score <-
      get.niches(database , mongo_uri , filter, datafile) %>%
      ggplot() +
      geom_bar(aes(x = reorder(keyword, -overall_score), y = overall_score),
               fill = '#4E79A7',
               stat = 'identity') +
      labs(
        title = 'Overall Score',
        subtitle = 'Combined score of revenue potential, competition & market entry barrier',
        y = '',
        x = ''
      ) +
      geom_label(
        aes(
          x = reorder(keyword, overall_score),
          y = overall_score,
          label = round(overall_score, 1)
        ),
        hjust = 0.5,
        vjust = 0,
        colour = "black",
        fill = NA,
        label.size = NA,
        family = "Helvetica",
        size = 7
      ) + extra.theme
    
    if (!is.null(filename)) {
      overall.score %>% ggsave(filename = filename)
    }
    
    return(overall.score)
  }

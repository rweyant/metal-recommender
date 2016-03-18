library(dplyr)
library(plyr);library(dplyr)
library(cluster)
library(magrittr)
library(stringr)
library(parallel)
library(tools)
library(ggplot2)
library(reshape2)
library(maps)
library(lazyeval)
library(RJSONIO)
library(jsonlite)
library(memoise)
library(tidyr)
library(DT)

library(shiny)
library(shinydashboard)
library(shinysky)

source('../helpers.R')
source('../tfidf-helpers.R')
source('../last-fm-helpers.R')

load('../RData/metal-example.RData')
load('../RData/artist_cols.RData')


zero <- function(x) 0
profile_of_interest <- trim_metal_artists[1,] %>% mutate_each(funs(zero)) %>% select(-artist)
dt_trim_metal_artists <- as.data.table(trim_metal_artists)
current_artists <- trim_metal_artists


vote_weights <- data_frame(like=1,dislike=-1,na=0)
genre_map <- list(
  'Thrash/Speed Metal' = c('genre_thrash_metal'),
  'Death Metal' = c('genre_death_metal'),
  'Black Metal' = c('genre_black_metal'),
  'Doom/Sludge/Stoner Metal' = c('genre_doom_metal', 'genre_sludge_metal', 'genre_stoner_metal'),
  'Power Metal' = c('genre_power_metal'), 
  'Post-metal' = c('genre_post_metal', 'genre_postmetal'))

origin_map <- list(
  'North America' = c('origin_united_states','origin_canada','lfm_united_states_of_america'),
  'UK/Ireland' = c('origin_united_kingdom','origin_northern_ireland','origin_ireland','lfm_united_kingdom', 'lfm_united_states'),
  'Sweden' = c('origin_sweden'),
  'Finland' = c('origin_finland'),
  'Norway' = c('origin_norway',''),
  'Germany/Austria' = c('origin_germany','origin_austria'), 
  'France' = c('origin_france'), 
  'Italy' = c('origin_italy'),
  'Asia' = c('origin_japan'),
  'Latin America' = c('origin_brazil')
  )


ui <- dashboardPage(
  dashboardHeader(
    title = 'Heavy Metal Recommender',
    titleWidth = 450
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(6,
             fluidRow(
               box(
                 selectInput(inputId = 'in6', 
                             label = 'Artists',
                             choices = current_artists$artist,
                             multiple=TRUE,
                             selectize=TRUE),
                 uiOutput('bandVoting'),
                 actionButton('SubmitButton','Submit')
               )
             ),
             fluidRow(
               box(
                 checkboxGroupInput(inputId = 'genres',
                                    label = 'Sub-Genre',
                                    choices = c('Thrash/Speed Metal',
                                                'Death Metal',
                                                'Black Metal',
                                                'Doom/Sludge/Stoner Metal',
                                                'Power Metal', 
                                                'Post-metal')
                 )
               ),
               box(
                 checkboxGroupInput(inputId = 'modifiers',
                                    label = 'Modifiers',
                                    choices = c('Blackened',
                                                'Progressive',
                                                'Technical',
                                                'Atmospheric',
                                                'Funeral',
                                                'Drone', 
                                                'Crossover',
                                                'Groove',
                                                'Old School')
                 )
               )
             ),
             fluidRow(
               box(
                 checkboxGroupInput(inputId = 'origin',
                                    label = 'Country',
                                    choices = c('North America', 
                                                'UK/Ireland',
                                                'Sweden',
                                                'Finland',
                                                'Norway',
                                                'Germany/Austria', 
                                                'France', 
                                                'Asia', 
                                                'Latin America')
                 )
               ),
               box(
                 radioButtons(inputId = 'vocals',
                              label = 'Vocal Style',
                              choices = c('Harsh','Clean','In-between','Any'),
                              selected = 'Any',
                              inline =  FALSE
                 )
               )
             )
      ),
      box(
        title = "Recommended Artists",
        div(style = 'overflow-y: scroll', DT::dataTableOutput('like_table'))
      )
    )
  ),
  skin = 'black'
)



server <- function(input, output) { 
  
  update_profile <- function(){
    
    # Update Artist tags
    if( length(input$in6) > 0 ) {
      dt_trim_metal_artists <- as.data.table(trim_metal_artists)
      setkey(dt_trim_metal_artists, artist)
      in_cols <- colnames(dt_trim_metal_artists)[colnames(dt_trim_metal_artists) != 'artist']
      profile_of_interest <<- as.tbl(dt_trim_metal_artists[input$in6, lapply(.SD, mean), .SDcols = in_cols ])
    }

    message("Updating: ")
    message(length(input$in6))
    message(cat(input$in6))

  }
  
  update_genres_nr <-function(){
    message('----------------')
    # lapply(names(input), function(x) message( paste0(x, ': ', paste(input[[x]], collapse=', ')) ) )
    message('----------------')
    
    # Restrict current_artists to those who are of the selected genres
    current_artists <<- find_current_artists(c(genre_map[input$genres],origin_map[input$origin]))
    
    # Update TF-IDF to the new subset of bands
    current_tfidf <<- calc_tfidf(df = current_artists)
    
    message(dim(current_artists)[1])
    # head(get_similarity_vector(current_tfidf, profile_of_interest),8)
  }
  
  find_current_artists <- function(columns){
    is_artist_relevant <- 
      trim_metal_artists %>%
      select(which(colnames(trim_metal_artists) %in% columns)) %>% 
      mutate(relevant = rowSums(.))
    
    trim_metal_artists %>% filter(is_artist_relevant$relevant > 0)  
    
#     tmp_trim <- copy(dt_trim_metal_artists)
#     as.tbl(tmp_trim[, relevant := Reduce(`+`, .SD), .SDcols = columns, by = artist][relevant > 0])
  }

  output$like_table <- DT::renderDataTable({
    if(length(input$in6) > 0){
      message('like_table: ')
      message(cat(input$in6))
      
      update_profile()
      update_genres_nr()
      
      print(max(profile_of_interest))
      
      DT::datatable(
        head(get_similarity_vector(current_tfidf, profile_of_interest),20),
        options = list('dom' = 't')
      )
    }
  })

  

  
  
  
  #   like_table <- eventReactive(input$in6,{
  #     message('like_table: ')
  #     message(cat(input$in6))
  #     
  #     # layered_tfidf_weights <<- layered_tfidf_weights %>% mutate(weight = ifelse( artist %in% input$in6, 1, 0 ))
  #     
  #     # update_similarity()
  #     # update_profile()
  #     print(max(profile_of_interest))
  #     # similarity %>% filter(! ( artist %in% input$in6) ) %>% head(15)
  #     head(get_similarity_vector(current_tfidf, profile_of_interest),20)
  #   })
  
  
  #   update_genres <- eventReactive({
  #     input$genres
  #     input$origin
  #     input$vocals
  #     input$modifiers
  #     },
  #     {
  #       message('----------------')
  #       lapply(names(input), function(x) message( paste0(x, ': ', paste(input[[x]], collapse=', ')) ) )
  #       message('----------------')
  #       
  #       # Restrict current_artists to those who are of the selected genres
  #       current_artists <<- find_current_artists(c(genre_map[input$genres],origin_map[input$origin]))
  #       
  #       # Update TF-IDF to the new subset of bands
  #       current_tfidf <<- calc_tfidf(df = current_artists)
  #       
  #       message(dim(current_artists)[1])
  #       head(get_similarity_vector(current_tfidf, profile_of_interest),8)
  #   })
  
#   
#   output$update_genres <- renderTable({
#     message('renderTable genre')
#     update_genres()
#   })
#   
#   output$update_origin <- renderTable({
#     message('renderTable origin')
#     update_origin()
#   })
  
  
  
  
}



# 
# update_artist_lists <- function(){
#   unvoted <<- test_function %>% dplyr::filter(voted==0) %>% select(artist) %>% extract2(1) 
#   liked <<- test_function %>% dplyr::filter(weight>0) %>% select(artist) %>% extract2(1) 
#   disliked <<- test_function %>% dplyr::filter(weight<0) %>% select(artist) %>% extract2(1) 
#   vote_skipped <<- test_function %>% dplyr::filter(weight==0,voted==1) %>% select(artist) %>% extract2(1) 
# }

# update_similarity <- function(){
#   similarity <<- 
#     get_similarity_vector(
#       current_tfidf, 
#       profile_of_interest
#     ) 
# }

# trim_metal_artists <-
#   metal_artists %>%
#   select(-artist) %>% 
#   select(which(colSums(., na.rm = TRUE) > 0 ))
# trim_metal_artists$artist <- metal_artists$artist



shinyApp(ui, server)
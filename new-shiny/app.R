
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
library(microbenchmark)
library(data.table)

library(shiny)
library(shinydashboard)
library(shinysky)

source('../helpers.R')
source('../tfidf-helpers.R')
source('../last-fm-helpers.R')

load_time1 <- 
  microbenchmark(
    load('../RData/metal-example.RData'),
    times = 1L)

load_time2 <- 
  microbenchmark(
    load('../RData/artist_cols.RData'),
    times = 1L)
message('Load metal-example.RData:\t\t\t', round(load_time1$time / 1e9, 2), ' s')
message('Load artist_cols.RData:\t\t\t', round(load_time2$time / 1e9, 2), ' s')

zero <- function(x) 0
### SLOW DOWN HERE
profile_of_interest <- trim_metal_artists[1,] %>% mutate_each(funs(zero)) %>% select(-artist)
dt_trim_metal_artists <- as.data.table(trim_metal_artists)
# setkey(dt_trim_metal_artists)
current_artists <- trim_metal_artists


vote_weights <- data_frame(like=1,dislike=-1,na=0)
# genre_mapX <- list(
#   'Thrash/Speed Metal' = c('genre_thrash_metal'),
#   'Death Metal' = c('genre_death_metal'),
#   'Black Metal' = c('genre_black_metal'),
#   'Doom/Sludge/Stoner Metal' = c('genre_doom_metal', 'genre_sludge_metal', 'genre_stoner_metal'),
#   'Power Metal' = c('genre_power_metal'), 
#   'Post-metal' = c('genre_post_metal', 'genre_postmetal'))

genre_map <- list(
  'Thrash/Speed Metal' = paste(c('thrash', 'speed'), collapse = '|'),
  'Death Metal' = c('death'),
  'Black Metal' = c('black'),
  'Doom/Sludge/Stoner Metal' = paste(c('doom', 'sludge', 'stoner'), collapse = '|'),
  'Power Metal' = c('power'), 
  'Post-metal' = paste(c('post', 'postmetal'), collapse = '|'))

origin_map <- list(
  'North America' = paste(c('united_states','canada','united_states_of_america', 'usa'), collapse = '|'),
  'UK/Ireland' = paste(c('united_kingdom','northern_ireland','ireland'), collapse = '|'),
  'Sweden' = c('sweden'),
  'Finland' = c('finland'),
  'Norway' = c('norway'),
  'Germany/Austria' = paste(c('germany','austria'), collapse = '|'), 
  'France' = c('france'), 
  'Italy' = c('italy'),
  'Asia' = paste(c('japan','china','thai','cambodia','india','bangladesh','pakistan','chinese','japanese','asian','korea','mongolia','vietnamese','singapor','malay'), collapse = '|'),
  'Latin America' = paste(c('brazil', 'south_america','central_america','latin_america','argentina','chile','colombia','hondura','panama','peru', 'latin'), collapse = '|')
  )

modifier_map <- list(
  'Blackened' = c('blackened'),
  'Progressive' = c('progressive'),
  'Technical' = c('technical'),
  'Atmospheric' = c('atmospheric'),
  'Funeral' = c('funeral'),
  'Drone' = c('drone'), 
  'Crossover' = c('crossover'),
  'Groove' = c('groove'),
  'Old School' = paste(c('oldschool','old_school'), collapse = '|')
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
                 selectInput(inputId = 'liked_artists', 
                             label = 'Artists like:',
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
                                    label = 'Include Sub-Genre',
                                    choices = c('Thrash/Speed Metal',
                                                'Death Metal',
                                                'Black Metal',
                                                'Doom/Sludge/Stoner Metal',
                                                'Power Metal', 
                                                'Post-metal')
                 )
               ),
               box(
                 checkboxGroupInput(inputId = 'exclude_genres',
                                    label = 'Exclude Sub-Genre',
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
                                    label = 'Include Modifiers',
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
               ),
               box(
                 checkboxGroupInput(inputId = 'exclude_modifiers',
                                    label = 'Exclude Modifiers',
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
                                    label = 'Include Country',
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
                 checkboxGroupInput(inputId = 'exclude_origin',
                                    label = 'Exclude Country',
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
                              choices = c('Harsh','Clean','In-between','Both','Any'),
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
    
    
    update_profile_time <- 
      microbenchmark(
        
        # Update Artist tags
        if( length(input$liked_artists) > 0 ) {
          dt_trim_metal_artists <- as.data.table(trim_metal_artists)
          setkey(dt_trim_metal_artists, artist)
          in_cols <- colnames(dt_trim_metal_artists)[colnames(dt_trim_metal_artists) != 'artist']
          profile_of_interest <<- as.tbl(dt_trim_metal_artists[input$liked_artists, lapply(.SD, mean), .SDcols = in_cols ])
        },
        times = 1L)
    message('Tag update time:\t\t\t', round(update_profile_time$time / 1e9, 2), ' s')
  }
  
  update_genres_nr <-function(){
    
    # Restrict current_artists to those who are of the selected genres
    find_artists_time <- 
      microbenchmark(
        current_artists <<- 
          find_current_artists(genre = c(genre_map[input$genres]),
                               origin = c(origin_map[input$origin]), 
                               modifiers = c(modifier_map[input$modifiers]),
                               genre_exclusion = c(genre_map[input$exclude_genres]),
                               origin_exclusion = c(origin_map[input$exclude_origin]), 
                               modifier_exclusion = c(modifier_map[input$exclude_modifiers])
                              ),
        times = 1L)
    
    # Update TF-IDF to the new subset of bands
    tfidf_time <- 
      microbenchmark(current_tfidf <<- calc_tfidf(df = current_artists),
                     times = 1L)
    
    message('Size of Artist Set:\t\t\t', dim(current_artists)[1])
    message('Find Artists time:\t\t\t', round(find_artists_time$time[1] / 1e9, 2), ' s')
    message('TFIDF time:\t\t\t\t', round(tfidf_time$time[1] / 1e9, 2), ' s')
  }
  
  match_columns <- function(patterns, exclusions, column_names) {
    if( length(patterns) > 0 ) which( grepl( paste(patterns, collapse = '|'), column_names) &  !( grepl( paste(c(exclusions, 'artist'), collapse = '|'), column_names) ) )
    else c()
  }
  match_relevant_artists <- function(dt, columns, threshold = 0.3){

    if(!is.null(columns)){
      as.tbl( 
        as.data.frame( 
          dt[, 
                   relevant := Reduce(`+`, .SD),
                   .SDcols = columns,
                   by = artist][relevant > threshold]
        )) %>%
        select(artist) %>% extract2(1)
    } else {
      c()
    }
  }
  
  generate_inclusions_list <- function(genre, origin, modifier){
    return_list <- list()
    if(!is.null(genre)) return_list[['genre']] <- genre
    if(!is.null(genre)) return_list[['origin']] <- origin
    if(!is.null(genre)) return_list[['modifier']] <- modifier
    return_list
  }
  
  
  find_current_artists <- function(columns = c(),
                                   genre = c(),
                                   genre_exclusion = c(),
                                   origin = c(),
                                   origin_exclusion = c(),
                                   modifiers = c(),
                                   modifier_exclusion = c(),
                                   vocals = c()){

    find_relevant_artists_time <- 
      microbenchmark({
        
        tmp_trim <- copy(dt_trim_metal_artists)
        setkey(tmp_trim,artist)
        
        genre_match <- match_columns(genre, c('origin_', 'mood_'), colnames(tmp_trim) )
        origin_match <- match_columns(origin, c('genre_', 'mood_'), colnames(tmp_trim) )
        modifiers_match <- match_columns(modifiers, c('genre_', 'origins_', 'mood_'), colnames(tmp_trim) )
        
        genre_exclusion_columns <- match_columns(genre_exclusion, c('origin_', 'mood_'), colnames(tmp_trim) )
        origin_exclusion_columns <- match_columns(origin_exclusion, c('genre_', 'mood_'), colnames(tmp_trim) )
        modifier_exclusion_columns <- match_columns(modifier_exclusion, c('genre_', 'origins_', 'mood_'), colnames(tmp_trim) )
        
        message(paste(modifiers, collapse = '|'))
        message('Genre Exclusions: ', length(genre_exclusion_columns))
        
        relevant_genre <- match_relevant_artists(tmp_trim, genre_match)
        relevant_origin <- match_relevant_artists(tmp_trim, origin_match)
        relevant_modifier <- match_relevant_artists(tmp_trim, modifiers_match)
        
        genre_exclusion_artists <- match_relevant_artists(tmp_trim, genre_exclusion_columns)
        origin_exclusion_artists <- match_relevant_artists(tmp_trim, origin_exclusion_columns)
        modifier_exclusion_artists <- match_relevant_artists(tmp_trim, modifier_exclusion_columns)
        
        inclusions_list <- generate_inclusions_list( relevant_genre, relevant_origin, relevant_modifier )
        exclusions_list <- generate_inclusions_list( genre_exclusion_artists, origin_exclusion_artists, modifier_exclusion_artists )
        
        # return_dt <- tmp_trim[artist %in% relevant_genre & artist %in% relevant_origin & artist %in% relevant_modifier ]
        inclusions <- Reduce(intersect, inclusions_list)
        exclusions <- Reduce(union, exclusions_list)

        final_inclusions <- inclusions[ !(inclusions %in% exclusions) ]
        
        message('I: ', length(inclusions), '\tE: ', length(exclusions), '\tF: ', length(final_inclusions))
        
        return_dt <- tmp_trim[ final_inclusions, , ]
      },
      times = 1L)
    message('Find Relevant Artists time:\t\t', round(find_relevant_artists_time$time / 1e9, 2), ' s')
    message(paste(genre, collapse = '|'), '-|-',paste(origin, collapse = '|') )
    return_dt
  }

  output$like_table <- DT::renderDataTable({
    like_table_time <- 
      microbenchmark({
        if(length(input$liked_artists) > 0){
          
          message('Liked Artists:\t\t\t\t', paste(input$liked_artists, collapse = ', '))
          
          update_profile()
          update_genres_nr()
          
          return_table <- 
            DT::datatable(
              head(get_similarity_vector(current_tfidf, profile_of_interest),20),
              options = list('dom' = 't')
            )
        }
      },
      times = 1L)
    message('Full Like Table Calculation Time:\t', round(like_table_time$time / 1e9, 2), ' s')
    message('----------------')
    
    if(length(input$liked_artists) > 0) return(return_table)
  })

}

shinyApp(ui, server)

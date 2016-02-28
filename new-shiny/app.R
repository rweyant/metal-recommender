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

update_artist_lists <- function(){
  unvoted <<- test_function %>% dplyr::filter(voted==0) %>% select(artist) %>% extract2(1) 
  liked <<- test_function %>% dplyr::filter(weight>0) %>% select(artist) %>% extract2(1) 
  disliked <<- test_function %>% dplyr::filter(weight<0) %>% select(artist) %>% extract2(1) 
  vote_skipped <<- test_function %>% dplyr::filter(weight==0,voted==1) %>% select(artist) %>% extract2(1) 
}

update_similarity <- function(){
  similarity <<- 
    get_similarity_vector(
      current_tfidf, 
      profile_of_interest
    ) 
}

# trim_metal_artists <-
#   metal_artists %>%
#   select(-artist) %>% 
#   select(which(colSums(., na.rm = TRUE) > 0 ))
# trim_metal_artists$artist <- metal_artists$artist

zero <- function(x) 0
profile_of_interest <- trim_metal_artists[1,] %>% mutate_each(funs(zero))

layered_tfidf_weights <- test_function
layered_tfidf_weights$weight <- 0
layered_tfidf_weights$voted <- 0

vote_weights <- data_frame(like=1,dislike=-1,na=0)
genre_map <- list(
  'Thrash/Speed Metal' = c('genre_thrash_metal'),
  'Death Metal' = c('genre_death_metal'),
  'Black Metal' = c('genre_black_metal'),
  'Doom/Sludge/Stoner Metal' = c('genre_doom_metal', 'genre_sludge_metal', 'genre_stoner_metal'),
  'Power Metal' = c('genre_power_metal'), 
  'Post-metal' = c('genre_post_metal', 'genre_postmetal'))

ui <- dashboardPage(
  dashboardHeader(
    title = 'Heavy Metal Recommender'
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(6,
             fluidRow(
               box(
                 selectInput(inputId = 'in6', 
                             label = 'Artists',
                             choices = metal_artists$artist,
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
                                    choices = c('USA', 
                                                'UK',
                                                'Sweden',
                                                'Finland',
                                                'Norway',
                                                'Germany', 
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
        # tableOutput("like_table")
      ),
      box(
        tableOutput('update_genres')
        # tableOutput("like_table")
      )
    )
  )
)


server <- function(input, output) { 
  
  output$out6 <- renderPrint(input$in6)
  
  like_table <- eventReactive(input$in6,{
    message(cat(input$in6))
    
    layered_tfidf_weights <<- layered_tfidf_weights %>% mutate(weight = ifelse( artist %in% input$in6, 1, 0 ))
    update_similarity()
    similarity %>% filter(! ( artist %in% input$in6) ) %>% head(15)
  })
  
  output$like_table <- DT::renderDataTable({
    DT::datatable(like_table(), options = list('dom' = 't'))
  })
  
  update_genres <- eventReactive(input$genres,{
    print(genre_map[input$genres])
    
    is_artist_relevant <- 
      trim_metal_artists %>%
      select(which(colnames(trim_metal_artists) %in% genre_map[input$genres])) %>% 
      mutate(relevant = rowSums(.))
    current_artists <<- trim_metal_artists %>% filter(is_artist_relevant$relevant > 0)
    message(length(input$genres))
    
    # Force profile of interest to have 1 for those columns
    dim(current_artists)
    # profile_of_interest[,colSums(current_artists %>% select(-artist)) > 0 ]
    
    tmp_df <- profile_of_interest
    tmp_df[,as.character(unlist(genre_map[(names(genre_map) %in% input$genres)]))] <- 1
    tmp_df[,as.character(unlist(genre_map[!(names(genre_map) %in% input$genres)]))] <- 0
    profile_of_interest <<- tmp_df
    
    current_tfidf <<- calc_tfidf(df = current_artists, columns = 'z')
    
    message(dim(current_artists)[1])
    head(get_similarity_vector(current_tfidf, profile_of_interest),8)
  })
  
  output$update_genres <- renderTable({
    message('renderTable')
    update_genres()
  })
  
  
  
  
  
}

shinyApp(ui, server)
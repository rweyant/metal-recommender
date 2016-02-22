library(dplyr)
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
library(shiny)

update_artist_lists <- function(){
  unvoted <<- test_function %>% dplyr::filter(voted==0) %>% select(artist) %>% extract2(1) 
  liked <<- test_function %>% dplyr::filter(weight>0) %>% select(artist) %>% extract2(1) 
  disliked <<- test_function %>% dplyr::filter(weight<0) %>% select(artist) %>% extract2(1) 
  vote_skipped <<- test_function %>% dplyr::filter(weight==0,voted==1) %>% select(artist) %>% extract2(1) 
}
update_similarity <- function(){
  chars <<- ((test_function %>% select(-weight,-artist,-voted)) * unlist(test_function$weight) ) %>% summarise_each(funs(mean)) %>% as.data.frame
  test_sims <<- 
    get_similarity_vector(test_function %>% select(-weight,-voted),chars) 
}

server <- function(input, output) {
  
  load('RData/saved_examples.RData')
  message('Data Loaded.')
  
  test_function$weight <- 0
  test_function$voted <- 0
  test_function %<>% mutate(weight=ifelse(artist %in% c('slayer'),1,weight)) 
  test_function %<>% mutate(voted=ifelse(artist %in% c('slayer'),1,voted)) 
  
  update_artist_lists()
  update_similarity()
  message('Liked:',liked)
  
  vote_weights <<- data.frame(like=1,dislike=-1,na=0)
  
  message('Ready to go.')
  
  testSubmit <- eventReactive(input$SubmitButton,{
    if(length(input$vote_artist1)>0){
      curArtist <<- test_sims %>% filter(row_number() == 1) %>% select(artist) %>% extract2(1) %>% as.character
      test_function <<- test_function %>% mutate(voted=ifelse(artist == curArtist,1,voted))
      test_function <<- test_function %>% mutate(weight=ifelse(artist == curArtist,vote_weights[input$vote_artist1],weight))
    }
#     unvoted <<- test_function %>% dplyr::filter(voted==0) %>% select(artist) %>% extract2(1) 
#     chars <- ((test_function %>% select(-weight,-artist)) * unlist(test_function$weight) ) %>% summarise_each(funs(mean)) %>% as.data.frame
#     test_sims <<- 
#       get_similarity_vector(test_function %>% select(-weight),chars) %>% 
#       dplyr::filter(artist %in% unvoted) 
#     
#     chars <- ((test_function %>% select(-weight,-artist,-voted)) * unlist(test_function$weight) ) %>% summarise_each(funs(mean)) %>% as.data.frame
#     get_similarity_vector(test_function %>% filter(voted == 0) %>% select(-weight,-voted),chars) %>% head(15)
    update_artist_lists()
    update_similarity()
    test_sims %>% filter(artist %in% unvoted) %>% head(10)
  })
  
  like_table <- eventReactive(input$SubmitButton,{
    message('like')
    message(paste(ls(.GlobalEnv),collapse='\n'))
    
    update_artist_lists()
    update_similarity()    
    test_sims %>% filter(artist %in% liked)
  })
    
  dislike_table <- eventReactive(input$SubmitButton,{
    update_artist_lists()
    update_similarity()
    test_sims %>% filter(artist %in% disliked)
  })
  
  na_table <- eventReactive(input$SubmitButton,{
    update_artist_lists()
    update_similarity()
    test_sims %>% filter(artist %in% vote_skipped)
  })
  
  top_unrated_band <- eventReactive(input$SubmitButton,{
    radioButtons('vote_artist1',label = test_sims[1,]$artist ,choices = c('like','dislike','skip'),inline = TRUE,selected = character(0))
  })
  
  
  output$table1 <- renderTable({
    testSubmit()
  })
  output$like_table <- renderTable({
    like_table()
  })
#   output$dislike_table <- renderTable({
#     dislike_table()
#   })
#   output$na_table <- renderTable({
#     na_table()
#   })
  
  output$bandVoting <- renderUI({
    top_unrated_band()
  })
  output$frame <- renderUI({
    tags$iframe(src="https://embed.spotify.com/?uri=spotify:track:4th1RQAelzqgY7wL53UGQt",width=300,height=250,frameborder=0,allowtransparency='true')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput('bandVoting'),
      actionButton('SubmitButton','Submit')
    ),
    mainPanel(
      h2('recommended artists'),
      tableOutput("table1"),
      h3('liked artists'),
      tableOutput("like_table"),
#       h3('disliked artists'),
#       tableOutput("dislike_table"),
#       h3('non-rated'),
#       tableOutput("na_table")
    )
  )
)

shinyApp(ui = ui, server = server)

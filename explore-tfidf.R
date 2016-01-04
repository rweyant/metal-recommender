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

source('helpers.R')
# source('comparison_functions.R')
data(world.cities)

load('RData/test_data.RData')
dim(full_df)

data_cols <- 7:dim(full_df)[2]
full_df[,data_cols] %>% names


###
### Create grouping variables
###

songs_cols <- get_columns(colnames(full_df))
indie_cols0 <- c(column_match(full_df,'indie',type='name'),column_match(full_df,'alt',type='name'))
indie_cols <- indie_cols0[str_detect(indie_cols0,'genre_') & !str_detect(indie_cols0,'folk')]
full_df$any_state <- apply(full_df[,songs_cols$origin_states_cols],1,any)
full_df$origin_missing <- !apply(full_df[,songs_cols$origin_cols],1,any)
full_df$overall_metal <- apply(full_df[,songs_cols$genre_metal_cols],1,any)
full_df$overall_punk <- apply(full_df[,songs_cols$genre_punk_cols],1,any)
full_df$overall_indie <- apply(full_df[,indie_cols],1,any)
full_df$overall_rock <- apply(full_df[,songs_cols$genre_rock_cols],1,any)


###
### Summarize Artists
### 
artist_summary <- 
  full_df %>% 
  group_by(artist) %>% 
  select(-title,-queried_artist,-album,-queried_album,-album_year) %>% 
  mutate(num_songs=n()) %>% 
  summarise_each(funs(mean)) %>% 
  mutate(rock=round(overall_rock),
         metal=round(overall_metal),
         punk=round(overall_punk),
         indie_alt=round(overall_indie)) 
artist_cols <- get_columns(colnames(artist_summary))

cosine_similarity <- function(X,y) {
  X %<>% as.matrix
  y %<>% as.matrix
  X %*% t(y) / (apply(X,1,function(x) norm(x,'2')) * norm(y,'2'))
}

get_similarity <- function(df,band){
  tmpcs <- 
    cosine_similarity(
      df %>% select(-artist) %>% as.data.frame,
      df %>% filter(artist==band) %>% select(-artist) %>% as.data.frame
    )
  cbind.data.frame(artist=df$artist,similarity=tmpcs) %>%  arrange(desc(similarity)) %>%  as.tbl
}

get_similarity_vector <- function(df,characteristics){
  tmpcs <- 
    cosine_similarity(
      df %>% select(-artist) %>% as.data.frame,
      characteristics
    )
  cbind.data.frame(artist=df$artist,similarity=tmpcs) %>%  arrange(desc(similarity)) %>%  as.tbl
}

get_tags <- function(df,band,threshold=0.1) df %>% filter(artist==band) %>% select(which(. > threshold)) %>% select(-artist) %>% data.frame %>% t

calc_tfidf <- function(df,columns){
  nrows <- nrow(df)
  tfidf <- 
    df %>% 
    select(artist,columns) %>% 
    select(-artist) %>%
    select(which(colSums(.)>0)) %>%
    mutate_each(funs(. * nrows/sum(.))) 
  tfidf$artist <- df$artist
  tfidf
}


get_layered_similarity_artist <- function(df,artist,first,second,percent=0.9){
  first_tfidf <- calc_tfidf(df,first)
  first_related_similarity <- get_similarity(first_tfidf,artist) 
  top_artists <- first_related_similarity %>% mutate(pct_rank=percent_rank(similarity)) %>% filter(pct_rank > percent | pct_rank == max(pct_rank)) %>% select(artist)
  first_related_df <- df %>% filter(artist %in% top_artists$artist)
  second_tfidf <- calc_tfidf(first_related_df,second)
  second_tfidf
}

metal_artists <- artist_summary %>% filter(metal == 1)
test_function <- get_layered_similarity_artist(metal_artists,'slayer',artist_cols$genre_cols,c(artist_cols$mood_cols,artist_cols$tempo_cols,artist_cols$era_cols))
get_similarity(test_function,'slayer')


test_function$weight <- 0
test_function %<>% mutate(weight=ifelse(artist %in% c('slayer'),1,weight)) 
test_function %<>% mutate(weight=ifelse(artist %in% c('slayer','testament','anacrusis','sadus','death angel','kreator','destruction','metallica'),1,weight)) 
test_function %<>% mutate(weight=ifelse(artist %in% c('strapping young lad','probot','nasty savage','meshuggah','municipal waste','overkill','gama bomb'),-1,weight)) 

chars <- ((test_function %>% select(-weight,-artist)) * test_function$weight ) %>% summarise_each(funs(mean)) %>% as.data.frame
get_similarity_vector(test_function %>% select(-weight),chars) %>% head(15)

get_tags(artist_summary,'unearthly trance')


genre_tfidf <- calc_tfidf(metal_artists,artist_cols$genre_cols)
slayer_related <- get_similarity(genre_tfidf,'slayer') 
top10 <- slayer_related %>% mutate(pct_rank=percent_rank(similarity)) %>% filter(pct_rank > 0.9 | pct_rank == max(pct_rank)) %>% select(artist)

get_tags(artist_summary,'slayer')
get_tags(artist_summary,'fight')
get_tags(artist_summary,'angel corpse')
get_tags(artist_summary,'angelcorpse')

genre_related <- artist_summary %>% filter(artist %in% top10$artist)
mood_tfidf <- calc_tfidf(genre_related,c(artist_cols$mood_cols,artist_cols$tempo_general_cols,artist_cols$era_cols))
get_similarity(mood_tfidf,'slayer')


system.time(
tfidf <- 
  cur_artist_set %>% 
  select(artist,artist_cols$genre_metal_cols) %>% 
  select(-artist) %>%
  select(which(colSums(.)>0)) %>%
  mutate_each(funs(. * nrows/sum(.))) 
)
# %>% mutate(metal=metal_wgt * metal)
tfidf$artist <- cur_artist_set$artist


get_tags(artist_summary,'megadeth')
get_tags(artist_summary,'skeletonwitch')

tfidf %>% filter(artist=='slayer') %>% select(metal)

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

source('helpers.R')
source('tfidf-helpers.R')
source('last-fm-helpers.R')

load('RData/metal-example.RData')

test_artist <- 'baroness'
test_function <- get_layered_similarity_artist(metal_artists,test_artist,artist_cols$genre_cols,c(artist_cols$mood_cols,artist_cols$tempo_cols,artist_cols$era_cols, artist_cols$lfm_cols))
dim(test_function)

get_similarity(test_function,test_artist)
get_similarity(test_function,'om')
get_similarity(test_function_lfm,test_artist)


test_function$weight <- 0
# test_function %<>% mutate(weight=ifelse(artist %in% c('slayer'),1,weight)) 
# test_function %<>% mutate(weight=ifelse(artist %in% c('slayer','testament','anacrusis','sadus','death angel','kreator','destruction','metallica'),1,weight)) 
test_function %<>% mutate(weight=ifelse(artist %in% c('earth','om','baroness'),1,weight)) 

chars <- ((test_function %>% select(-weight,-artist)) * test_function$weight ) %>% summarise_each(funs(mean)) %>% as.data.frame
get_similarity_vector(test_function %>% select(-weight),chars) %>% arrange(desc(similarity)) %>% head(15)

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

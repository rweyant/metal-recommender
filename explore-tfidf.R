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

songs.cols <- get_columns(colnames(full_df))
indie_cols0 <- c(column_match(full_df,'indie',type='name'),column_match(full_df,'alt',type='name'))
indie_cols <- indie_cols0[str_detect(indie_cols0,'genre_') & !str_detect(indie_cols0,'folk')]
full_df$any_state <- apply(full_df[,songs.cols$origin_states_cols],1,any)
full_df$origin_missing <- !apply(full_df[,songs.cols$origin_cols],1,any)
full_df$overall_metal <- apply(full_df[,songs.cols$genre_metal_cols],1,any)
full_df$overall_punk <- apply(full_df[,songs.cols$genre_punk_cols],1,any)
full_df$overall_indie <- apply(full_df[,indie_cols],1,any)
full_df$overall_rock <- apply(full_df[,songs.cols$genre_rock_cols],1,any)


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

n <- nrow(artist_summary)
tfidf <- 
  artist_summary %>% 
  select(-artist) %>%
  select(which(colSums(.)>0)) %>%
  mutate_each(funs(. * n/sum(.)))
tfidf$artist <- artist_summary$artist

slayer <- tfidf %>% filter(artist=='slayer') %>% select(-artist) %>% as.matrix
metallica <-  tfidf %>% filter(artist=='metallica') %>%  select(-artist) %>% as.matrix
slomatics <-  tfidf %>% filter(artist=='slomatics') %>%  select(-artist) %>% as.matrix

slayer %*% t(metallica)
slayer %*% t(slomatics)
metallica %*% t(slomatics)
metallica %*% t(slayer)

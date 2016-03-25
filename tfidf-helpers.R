
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
  
  common_columns <- names(characteristics)[names(characteristics) %in% names(characteristics) & names(characteristics) %in% names(df)]
  
  tmpcs <- 
    cosine_similarity(
      df %>% select(which(names(df) %in% common_columns)) %>% as.data.frame ,
      characteristics %>% select(which(names(characteristics) %in% common_columns)) %>% as.data.frame 
    )
  cbind.data.frame(artist=df$artist,similarity=tmpcs) %>%  arrange(desc(similarity)) %>%  as.tbl
}

get_tags <- function(df,band,threshold=0.1) df %>% filter(artist==band) %>% select(which(. > threshold)) %>% select(-artist) %>% data.frame %>% t

# Calculate TF-IDF based on a specific set of columns
calc_tfidf <- function(df){
  nrows <- nrow(df)
#   tfidf <- 
#     df %>% 
#     select(-artist) %>%
#     select(which(colSums(., na.rm = TRUE)>0)) %>%
#     mutate_each(funs(. * nrows/sum(.))) 
#   
  dt_current_artists <- as.data.table(df)
  nrows <- nrow(dt_current_artists)
  in_cols <- colnames(dt_current_artists)[colnames(dt_current_artists) != 'artist']
  dt_tfidf <- as.tbl(as.data.frame(dt_current_artists[,lapply(.SD, function(x) x * nrows / sum(x) ), .SDcols = in_cols])) %>% select(which(colSums(., na.rm = TRUE)>0))
  # message(dim(dt_tfidf)[1], ' ', dim(dt_tfidf)[2])
  
  dt_tfidf$artist <- df$artist
  dt_tfidf
}

#####

#####

get_layered_similarity_artist <- function(df,artist,first,second,percent=0.9){
  first_tfidf <- calc_tfidf(df,first)
  first_related_similarity <- get_similarity(first_tfidf,artist) 
  top_artists <- first_related_similarity %>% mutate(pct_rank=percent_rank(similarity)) %>% filter(pct_rank > percent | pct_rank == max(pct_rank)) %>% select(artist)
  first_related_df <- df %>% filter(artist %in% top_artists$artist)
  second_tfidf <- calc_tfidf(first_related_df,second)
  second_tfidf
}

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
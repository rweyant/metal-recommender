
create_artist_query_url_lfm <- function(artist_name){
  prefix <- "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist="
  postfix <- "&api_key=c2e57923a25c03f3d8b317b3c8622b43&format=json"
  encoded_artist <- URLencode(artist_name)
  return(paste0(prefix, encoded_artist, postfix))
}


get_tag_frame_lfm <- function(an_artist){
  message(paste0("Attempting to fetch: ", an_artist))
  artist_url <- create_artist_query_url_lfm(an_artist)
  json <- jsonlite::fromJSON(artist_url)
  if('message' %in% names(json)) {
    message(paste0("Could not find: ", an_artist))
    return(NULL)
  }
  return(json$toptags$tag[,c("name",'count')])
}

mem_get_tag_frame_lfm <- memoise(get_tag_frame_lfm)

get_last_fm_tags <- function(artist){
  try(tags0 <- mem_get_tag_frame_lfm(artist))
  cat(exists('tags0'))
  if( exists('tags0') && !is.null(tags0) ){
    tags1 <- 
      tags0 %>% 
      mutate(name = tolower(str_replace_all(str_replace_all(str_replace_all(str_trim(name,'both'),'[[:punct:]]',''),' ','_'),'__','_')),
             weight = count / max(count),
             name = tolower(name)) %>%
      filter(name != '' ) %>% 
      group_by(name) %>% 
      summarize(weight = sum(weight)) %>% 
      arrange(weight) 
    tags2 <- tags1 %>% spread(key = name, value = weight)
    colnames(tags2) <- paste0('lfm_',colnames(tags2))
    tags3 <- tags2 %>% mutate(artist = artist)
    tags3
  } else{
    message('Skipping')
    NULL
  }
}


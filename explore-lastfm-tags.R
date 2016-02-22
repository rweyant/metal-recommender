
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


artist_lfm_tags <- 
  lapply(head(artist_summary$artist,500),
       function(x){
         try(tags0 <- mem_get_tag_frame_lfm(x))
         cat(exists('tags0'))
         if(!is.null(tags0) && exists('tags0') ){
           tags1 <- tags0 %>% mutate(weight = count / max(count), name = tolower(name)) %>% select(-count)
           tags2 <- tags1 %>% spread(key = name, value = weight)
           colnames(tags2) <- paste0('lfm_',tolower(str_replace_all(str_replace_all(str_replace_all(str_trim(colnames(tags2),'both'),'[[:punct:]]',''),' ','_'),'__','_')))
           tags3 <- tags2 %>% mutate(artist = x)
           tags2
         } else{
           message('Skipping')
           NULL
         }
       })


last_fm_tags <- bind_rows(artist_lfm_tags)
# colnames(last_fm_tags) <- tolower(str_replace_all(str_replace_all(str_replace_all(str_trim(colnames(last_fm_tags),'both'),'[[:punct:]]',''),' ','_'),'__','_'))
names(last_fm_tags) %>% tail
last_fm_tags[1:10,]


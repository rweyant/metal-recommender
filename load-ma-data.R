

test <- RJSONIO::fromJSON(paste(readLines('ma-json/ma.json'),collapse=''))
devtools::load_all("/home/roberto/Documents/code/R/spotifyr/")


test[[1]] %>% names
test[[1]]$name
test[[4]]$genre
which(sapply(ms,function(x) x$name == 'The Lonely Hearts'))
ms <- test[[which(sapply(test,function(x) x$lyrical_themes == "Anti-paedophilia, Anti-rape"))]]
ms <- test[which(sapply(test,function(x) {
  !str_detect(x$current_label, 'Unsigned')
  }))]  

ms2 <- ms[which(sapply(ms,function(x) {
  
  exact_matches <- check_spotify$artists$items[which(sapply(check_spotify$artists$items,function(x) tolower(x$name)) == tolower(x$name) && )]
  check_spotify$artists$total > 0 &&
    (sapply(check_spotify$artists$items,function(x) x$genres) %>% unlist %>% str_detect('metal') %>% sum)>0
}))]  

sapply(test,function(x) x$current_label)
sapply(ms2,function(x) x$name)


check_spotify <- spotifyr::search('witch',type='artist',limit=50,offset=0)
check_spotify <- spotifyr::search('genre:nwobhm',type='artist',limit=50)

check_spotify$artists$total
sapply(check_spotify$artists$items,function(x) x$name)
sapply(check_spotify$artists$items,function(x) x$genres)


ma_results <-
  mclapply(ms, function(band){
    
    message(band$name)
    spotify_data <- spotifyr::search(tolower(band$name),type = 'artist',limit=1)

    if(band$current_label != 'Unsigned/independent' && spotify_data$artists$total > 0){
      spotify_band <- spotify_data$artists$items[[1]]
      
      
      lyrical_themes <- str_split(band$lyrical_themes,',') %>% unlist %>% str_trim
      ma_genres <- str_replace_all(band$genre,pattern = '\\(([^\\)]+)\\)','') %>% str_split(',') %>% unlist %>% str_trim
      spotify_genres <- spotify_band$genres %>% unlist
      spotify_band$followers$total
      spotify_band$popularity
      band$formation_year
      band$current_label
      band$country
      band$lineup$current_lineup %>% unlist %>% as.data.frame %>% rownames
      # band$years_active
      
      
      track_mood_tempo <- Reduce(function(...) merge(...,all=TRUE),track_info)
      full_info <-
        cbind.data.frame(artist=tolower(iconv(asjson$album_artist_name,from='latin1')),
                         queried_artist=curArtist,
                         album=tolower(iconv(asjson$album_title,from='latin1')),
                         queried_album=curAlbum,
                         album_year=asjson$album_year,
                         extract_info(asjson,'artist_type','artist_type'),
                         extract_info(asjson,'genre','genre'),
                         extract_info(asjson,'artist_era','era'),
                         extract_info(asjson,'artist_origin','origin'),
                         track_mood_tempo,stringsAsFactors=FALSE)
      full_info <- full_info[c('title',colnames(full_info)[colnames(full_info)!='title'])]
      full_info$title <- tolower(iconv(full_info$title,from='latin1'))
      full_info
    } else {
      warning('JSON is null')
      NULL
    }
  } 
  ,mc.cores=8 
  )


names(ms)
ms$location
ms$country
ms$genre
ms$current_label
ms$name
ms$lyrical_themes
ms$releases
ms$genre
ms$description
ms$lineup
ms$similar_artists
length(test)

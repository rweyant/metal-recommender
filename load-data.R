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


dir <- 'json/'
files <- list.files(dir)

small.n <- 626

full_results <-
  mclapply(files[str_detect(files,'json')], function(file.name){
    
    message(file.name)
    file_root <- file_path_sans_ext(file.name)
    
    curArtist <- str_split_fixed(file_root,'-',n=2)[1]
    curAlbum <- str_split_fixed(file_root,'-',n=2)[2]
    
    asjson <-
      tryCatch(
        expr=RJSONIO::fromJSON(paste(readLines(paste(dir,'/',file.name,sep=''),warn = FALSE),collapse='')),
        error=function(e) {}
      )
    
    if(!is.null(asjson)){
      track_info <-
        lapply(asjson$tracks,function(x){
          mood <- extract_info(x,'mood','mood')
          tempo <- extract_info(x,'tempo','tempo')
          # print(mood)
          title <- x$track_title
          cbind.data.frame(title,mood,tempo,stringsAsFactors=FALSE)
        })
      
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

### Combine Everything
comp_full_df <- Reduce(function(...) merge(...,all=TRUE), full_results[1:100])
test_mc <- mclapply(
  full_results[1:10],
  function(x) Reduce(function(...) merge(...,all=TRUE),x),
  mc.cores=8
  )


### Check inputs == outputs
sapply(full_results,function(x) x$artist[1]) %>% unlist %>% unique %>% length
comp_full_df$artist %>% unique %>% length

sapply(full_results,function(x) x$artist) %>% unlist %>% length
comp_full_df$artist %>% length


# Get rid of duplicates
full_df <- comp_full_df %>% group_by(title) %>% filter(1:n()==1) %>% ungroup
full_df[is.na(full_df)] <- FALSE


save(full_df,file = 'RData/test_data.RData')
save.image(file = 'RData/workspace_image.RData')

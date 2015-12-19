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

data(world.cities)

dir <- '/media/roberto/Main Storage/Documents/R/metal-recommender/json/'
files <- list.files(dir)

small.n <- 626

full_results <-
  mclapply(files[str_detect(files,'json')][1:small.n], function(file.name){
  
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
comp_full_df <- Reduce(function(...) merge(...,all=TRUE), full_results)

### Check inputs == outputs
sapply(full_results,function(x) x$artist[1]) %>% unlist %>% unique %>% length
comp_full_df$artist %>% unique %>% length

sapply(full_results,function(x) x$artist) %>% unlist %>% length
comp_full_df$artist %>% length


# Get rid of duplicates
full_df <- comp_full_df %>% group_by(title) %>% filter(1:n()==1) %>% ungroup
full_df[is.na(full_df)] <- FALSE
dim(full_df)



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
head(artist_summary)
artist.cols <- get_columns(colnames(artist_summary))

names(artist_summary)
head(artist_summary)
sapply(artist_summary,class)
artist_summary %>% dim
artist_summary %>% filter(genre_doom_metal > 0) %>% dim

get_similar_artist <- function(data,artist,n=20){
  data$curArtist <- data[,artist]
  data %>% select(artist,curArtist) %>% arrange(curArtist) %>% head(n)
}
get_dissimilar_artist <- function(data,artist,n=20){
  data$curArtist <- data[,artist]
  data %>% select(artist,curArtist) %>% arrange(curArtist) %>% tail(n)
}

weight <- function(x,w=2) x*w

weight_mood <- .3
weight_tempo <- .5
weight_genre <- .2
sim <-
  dist(
    artist_summary %>% 
      # Restrict to metal bands
      filter(overall_metal == 1) %>% 
      # Only based on mood
      mutate_each_(funs(weight(.,weight_mood)),column_match(.,pattern=c('mood_'),'name')) %>% 
      # mutate_each_(funs(weight(.,weight_tempo)),column_match(.,pattern=c('tempo_'),'name')) %>%
      # mutate_each_(funs(weight(.,weight_genre)),column_match(.,pattern=c('genre_'),'name')) %>% 
      select(column_match(., 'mood'
                          # pattern=c('mood','tempo','genre'))
             ))
    ) %>%
  as.matrix %>% 
  as.data.frame 
  
colnames(sim) <- artist_summary %>% filter(overall_metal==1) %>% select(artist) %>% .$artist
sim$artist <- artist_summary %>% filter(overall_metal==1) %>% .$artist
dim(sim)

get_similar_artist(sim,'slayer',10)

artist_summary %>% filter(artist=='sleep') %>% select(column_match(.,'tempo')) %>% as.data.frame
artist_summary %>% filter(artist=='slayer') %>% select(column_match(.,'tempo')) %>% as.data.frame

# get_dissimilar_artist(sim,'dark castle')








##
## Artist Tempo (port)
##
artist_tempo_summarized <- 
  artist_summary %>% 
  filter(genre_doom_metal>0) %>% 
  select(artist,rock,punk,metal,artist.cols$tempo_specific_cols) %>% 
  melt(measure.vars = c('punk','metal'),id.vars=c('artist',artist.cols$tempo_specific_names),variable.name = 'genre') %>% 
  filter(value==1) %>% 
  select(-value) 

colnames(artist_tempo_summarized) %<>% 
  str_replace('tempo_','') %>% 
  str_replace('s','') %>% 
  str_replace('\\+','') %>% 
  str_replace('artit','artist')

tempo_cols <- which(!str_detect(colnames(artist_tempo_summarized),'[A-z]'))
artist_tempo_summarized %>% 
  select(tempo_cols,genre) %>%
  group_by(genre) %>% 
  summarise_each(funs(mean)) %>% melt(id.vars = 'genre',value.name = 'count',variable.name = 'tempo') %>% 
  ggplot(.,aes(x=as.numeric(as.character(tempo)),y=count,group=genre,color=genre))+
  geom_line(size=3)+theme_bw()



##
## Artist Mood (port)
##
artist_mood_summarized <- 
  artist_summary %>%
  select(artist,rock,punk,metal,indie_alt,column_match(.,'mood_')) %>% 
  melt(measure.vars = c('punk','metal'),id.vars=c('artist',column_match(.,'mood_',type='name')),variable.name = 'genre') %>% 
  filter(value==1) %>% 
  select(-value) 

colnames(artist_mood_summarized) %<>% 
  str_replace('mood_','') 

artist_mood_summarized %>% 
  select(-artist) %>%
  group_by(genre) %>% 
  summarise_each(funs(mean)) %>%
  melt(id.vars = 'genre',value.name = 'count',variable.name = 'mood') %>% 
  filter(percent_rank(count) > 0.95) %>% 
  ggplot(.,aes(x=mood,y=count,fill=genre))+
  geom_bar(stat='identity',position='dodge')+theme_bw()


##
## Artist Genre (port)
##
artist_genre_summarized <- 
  artist_summary %>%
  select(artist,column_match(.,'genre_'),punk,metal,indie_alt) %>% 
  melt(id.vars=c('artist'),variable.name = 'genre') %>% 
  filter(value==1) %>% 
  select(-value) 

# Broad Genres
artist_genre_summarized %>% 
  select(-artist) %>%
  filter(genre %in% c('punk','metal')) %>% 
  group_by(genre) %>% 
  summarise(count=n()) %>%
  ggplot(.,aes(x=genre,y=count,fill=genre))+
  geom_bar(stat='identity',position='dodge')+theme_bw()

# Sub Genres
artist_genre_summarized %>% 
  select(-artist) %>%
  group_by(genre) %>% 
  summarise(count=n()) %>%
  mutate(genre=str_replace(genre,'genre_',''),genre=str_replace(genre,'general_',''),genre=str_replace(genre,'mainstream_','')) %>% 
  filter(!(genre %in% c('metal','rock','punk','indie_alt'))) %>% 
  filter(percent_rank(count) > 0.9) %>% 
  ggplot(.,aes(x=genre,y=count,fill=genre))+
  geom_bar(stat='identity',position='dodge')+theme_bw()












summarize_by_group(songs.cols$origin_states_cols,'origin',order_by='name')
summarize_by_group(songs.cols$tempo_specific_cols,'tempo',order_by='name')
summarize_by_group(songs.cols$tempo_general_cols,'tempo',order_by='count')
summarize_by_group(songs.cols$genre_metal_cols,'genre',order_by='count')
summarize_by_group(songs.cols$genre_punk_cols,'genre',order_by='count')
summarize_by_group(songs.cols$origin_country_cols,'origin',order_by='count')

full_df %$% table(any_state,origin_united_states)
# full_df %>% filter(!any_state & origin_united_states ) %>% select(songs.cols$origin_cols) %>% as.data.frame %>% head


##
## Artist Tempo
##
artist_tempo <- artist_summary %>% select(artist,overall_rock,overall_punk,overall_metal,artist.cols$tempo_specific_cols)
colnames(artist_tempo) %<>% str_replace('tempo_','') %>% str_replace('s','') %>% str_replace('artit','artist')
rock <- artist_tempo$overall_rock * artist_tempo[,str_detect(colnames(artist_tempo),'[0-9]')] 
metal <- artist_tempo$overall_metal * artist_tempo[,str_detect(colnames(artist_tempo),'[0-9]')] 
punk <- artist_tempo$overall_punk * artist_tempo[,str_detect(colnames(artist_tempo),'[0-9]')] 
artist_tempo_long <- 
  rbind.data.frame(
    data.frame(genre='rock',artist=artist_tempo$artist,rock,stringsAsFactors=FALSE),
    data.frame(genre='metal',artist=artist_tempo$artist,metal,stringsAsFactors=FALSE),
    data.frame(genre='punk',artist=artist_tempo$artist,punk,stringsAsFactors=FALSE)
  )
colnames(artist_tempo_long) %<>% str_replace('X','')
artist_tempo_long$`90`
artist_tempo_summary <- 
  artist_tempo_long %>%
  select(-artist) %>% 
  group_by(genre) %>% 
  summarise_each(funs(sum)) 
rownames(artist_tempo_summary) <- artist_tempo_summary$genre
artist_tempo_long <- artist_tempo_summary %>% select(-genre) %>% t %>% as.data.frame 
artist_tempo_long$tempo <- as.numeric(rownames(artist_tempo_long))


artist_tempo_long %>%
  melt(id.vars = 'tempo',variable.name = 'genre') %>% 
  group_by(genre) %>% 
  mutate(proportion=value/sum(value)) %>% ungroup %>% 
  ggplot(aes(x=tempo,y=proportion,group=genre,color=genre))+geom_line(size=3)+theme_bw()



colnames(artist_era)[str_detect(colnames(artist_era),'early')] %<>% paste(.,'a',sep='') %>% str_replace('early_','')
colnames(artist_era)[str_detect(colnames(artist_era),'late')] %>% paste(.,'c',sep='') %>% str_replace('late_','')

artist_summary %>% filter(artist == '88 fingers louie') %>% select(num_songs,artist.cols$origin_country_cols) %>% as.data.frame
artist_summary %>% select(artist.cols$era_cols) %>% summarize_each(funs(sum))

artist.cols$genre_metal_cols

### Summarize by Genres
full_df %$% table(overall_metal,overall_punk)
full_df %$% table(overall_metal,overall_rock)
full_df %$% table(overall_punk,overall_rock)



granular_genre_count <- full_df %>% select(genre_cols) %>% summarize_each(funs(sum)) %>% t %>% as.data.frame


dim(full_df)


full_df %>% filter(origin_wisconsin) %>% dim




full_df %>% select(tempo_cols) %>% summarize_each(funs(sum)) %>% as.data.frame
full_df %>% select(genre_cols) %>% summarize_each(funs(sum)) %>% as.data.frame

full_df %>% select(genre_cols) %$% table(genre_rock,genre_heavy_metal)

dim(full_df)
full_df %>% head





colnames(sim) <- full_df$title
sim$title <- full_df$title
sim$artist <- full_df$artist
sim$album <- full_df$album
dim(sim)

get_similar_songs <- function(sim,song_title,n=20){
  sim$curSong <- sim[,song_title]
  sim %>% select(title,artist,curSong) %>% arrange(curSong) %>% head(n)
}

get_similar_songs(sim,'16',30)



####






full_df %>% filter(title=='Walk')
song.title <- 'Beatbox'
sim %>% filter(title=='Master Of Puppets') %>% select(-title,-album)

sim[order(sim[,which(sim$title=='Walk')]),c('title','album','artist',which(sim$title=='Walk'))] %>% head(20)
sim[,c('title','album','artist',which(sim$title=='Walk'))] %>% head


colnames(full_df)[(full_df %>% filter(title=='Necrophiliac'))==TRUE]
colnames(full_df)[(full_df %>% filter(title=='Master Of Puppets'))==TRUE]

extract_tags('Necrophiliac',full_df)[extract_tags('Necrophiliac',full_df) %in% extract_tags('Master Of Puppets',full_df)]
1-mean(extract_tags('Necrophiliac',full_df) %in% extract_tags('Master Of Puppets',full_df))


head(sim)



rtl %>% names
track <- rtl$tracks[[1]]
names(track)




rtl$album_year)
names(rtl)

%>% t %>% as.data.frame

names(rtl)
rtl$tracks$tempo
rtl$tracks$mood

x <- data.frame(id=1,a=2,b=3)
y <- data.frame(id=2,u=5,v=2)
merge(x,y,by='id',all=TRUE)


library(dplyr)
library(cluster)
library(magrittr)
library(stringr)
library(spotifyr)

library(jsonlite)
library(RJSONIO)

set_credentials(
  client_id='ea7dc6e1d340430bb88b693f75f24ca8',
  client_secret='5344bc88cd3043dc9f503b6e61208371')
auth_test()


relateds <- get_artist_relatedartists('1IQ2e1buppatiN1bxUVkrk')
initial <- simplify_result(relateds,type='artists')

augmented <- initial
# i <- 1
for(i in 1:length(initial$id)){
  augmented <- unique(rbind.data.frame(augmented,simplify_result(get_artist_relatedartists(initial$id[i]))))
}

deduped <- augmented %>% group_by(name) %>% filter((1:n())==1) %>% ungroup


categorized <-
  deduped %>%
  mutate(cleaned_genre=str_replace_all(genre,' ','' )) %>%
  select(name,popularity,followers.total,cleaned_genre) %>%
  mutate(scaled_popularity=scale(as.numeric(popularity)),
         scaled_followers=scale(as.numeric(followers.total)))
genres <- str_split(categorized$cleaned_genre,'\\|') %>% unlist %>% unique
genres <- genres[sapply(genres,function(x) str_length(x) > 1)]

for(i in 1:length(genres)){
  categorized[,genres[i]] = str_detect(categorized$cleaned_genre,genres[i])
}

sim <- daisy(categorized %>% select(-name,-cleaned_genre,-popularity,-followers.total,-scaled_followers,-scaled_popularity),metric='gower') %>% as.matrix %>% as.data.frame
# colnames(sim) <- categorized$name
colnames(sim) <- categorized$name
sim$name <- categorized$name

band <- 'Metallica'
var <- match(band,colnames(sim))
sim %>% select(name,var) %>% arrange_(band) %>% head

head(sim)

categorized$name[sim[,categorized$name == 'Rob Zombie'] < .3]
hist(sim[,categorized$name == 'Rob Zombie'])
augmented %>% filter(name == 'American Head Charge' | name == 'Rob Zombie')
categorized %>% dplyr::filter(industrialmetal==TRUE)
sim
head(sim)


#####


# extract from JSONs
extract_info <- function(x,item='tempo',prefix=''){
  df <- sapply(x[[item]], function(y) y['TEXT']) %>% t

  if(is.null(nrow(df==df)) || nrow(df==df) == 0)
    return(FALSE)

  if(str_length(prefix)>1)
    colnames(df) <- paste(prefix,'_',
                          tolower(str_replace_all(str_replace_all(str_replace_all(str_trim(df[1,],'both'),'[[:punct:]]',''),' ','_'),'__','_')),sep='')
  else colnames(df) <- tolower(str_replace_all(str_replace_all(str_replace_all(str_trim(df[1,],'both'),'[[:punct:]]',''),' ','_'),'__','_'))


  as.data.frame(df == df)
}

### Extract
### by reference?
extract_tags <- function(song_title,df){
  colnames(df)[(df %>% filter(title==song_title))==TRUE]
}


dir <- '/media/roberto/Main Storage/Documents/R/metal-recommender/json/'
files <- list.files(dir)

# for(file.name in files[str_detect(files,'json')]){
full_results <-
  lapply(files[str_detect(files,'json')], function(file.name){
    message(file.name)
    asjson <- RJSONIO::fromJSON(paste(readLines(paste(dir,'/',file.name,sep=''),warn = FALSE),collapse=''))

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
      cbind.data.frame(artist=asjson$album_artist_name,
                       album=asjson$album_title,
                       album_year=asjson$album_year,
                       extract_info(asjson,'artist_type','artist_type'),
                       extract_info(asjson,'genre','genre'),
                       extract_info(asjson,'artist_era','era'),
                       extract_info(asjson,'artist_origin','origin'),
                       # extract_info(asjson,'tempo'),
                       track_mood_tempo,stringsAsFactors=FALSE)
    full_info <- full_info[c('title',colnames(full_info)[colnames(full_info)!='title'])]
    full_info
  })

comp_full_df <- Reduce(function(...) merge(...,all=TRUE), full_results)
full_df <- comp_full_df %>% group_by(title) %>% filter(1:n()==1) %>% ungroup
full_df[is.na(full_df)] <- FALSE
dim(full_df)

tempo_names <- names(full_df)[str_detect(names(full_df),'tempo_')]
tempo_cols <- which(str_detect(names(full_df),'tempo_'))

genre_names <- names(full_df)[str_detect(names(full_df),'genre_')]
genre_cols <- which(str_detect(names(full_df),'genre_'))

full_df %>% select(tempo_cols) %>% summarize_each(funs(sum)) %>% as.data.frame
full_df %>% select(genre_cols) %>% summarize_each(funs(sum)) %>% as.data.frame

full_df %>% filter(genre_nashville_sound)


names(full_df)
# head(full_df)
# full_df %>% select(-title,-artist,-album) %>% head
# full_df %>% filter(very_slow) %>% select(artist,title)

sim <- daisy(full_df %>% select(-title,-artist,-album,-album_year),metric='gower') %>% as.matrix %>% as.data.frame
test_sim <- sim
# head(sim)
# dim(sim)

colnames(sim) <- full_df$title
sim$title <- full_df$title
sim$artist <- full_df$artist
sim$album <- full_df$album

full_df %>% filter(title=='Walk')
sim %>% filter(title=='Master Of Puppets') %>% select(-title,-album)

sim[order(sim[,which(sim$title=='Walk')]),c('title','album','artist',which(sim$title=='Walk'))] %>% head(20)
sim[,c('title','album','artist',which(sim$title=='Walk'))] %>% head

song_title <- 'Master Of Puppets'
sim$curSong <- sim[,song_title]
sim %>% select(title,artist,curSong) %>% arrange(curSong) %>% head(20)
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

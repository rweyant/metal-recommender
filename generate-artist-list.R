# library(spotifyr)

library(dplyr)
# devtools::load_all("/media/roberto/Main Storage/Documents/R/packages/spotifyr/")
devtools::load_all("~/Documents/code/R/spotifyr/")

set_credentials(
  client_id='ea7dc6e1d340430bb88b693f75f24ca8',
  client_secret='5344bc88cd3043dc9f503b6e61208371')
auth_test()



### generate list for python script
my_artists <- get_user_followed_artists(limit=50)
artist_df <- simplify_result(my_artists$artists,type = 'artists')  
full_list <- lapply(1:ceiling(my_artists$artist$total/49),function(x) {
  cur_artists <- get_user_followed_artists(limit=50,after=49*x)
  simplify_result(cur_artists$artists,type = 'artists') 
})

all_followed <- Reduce(function(...) rbind(...),full_list) %>% rbind(.,artist_df) %>% unique 
rownames(all_followed) <- 1:nrow(all_followed)
all_followed %>% dim

get_full_related_artist_list <- function(artist.df){
  
  related_list <- sapply(artist.df$id,function(x) {
    Sys.sleep(.01)
    get_artist_relatedartists(x,country='us')
    })
  
  simplified_related <- sapply(related_list,function(x) simplify_result(x,type='artists'))
  reduced_related <- Reduce(function(...) rbind(...),simplified_related) %>% unique %>% na.omit
  combined <- bind_rows(artist.df,reduced_related)
  
  combined$name <- tolower(combined$name)
  combined <- unique(combined)
  rownames(combined) <- 1:nrow(combined)
  combined
}

augmented_once <- get_full_related_artist_list(all_followed)
augmented_twice <- get_full_related_artist_list(augmented_once)
augmented_thrice1 <- get_full_related_artist_list(augmented_twice[1:1000,])
augmented_thrice2 <- get_full_related_artist_list(augmented_twice[1001:2000,])
augmented_thrice3 <- get_full_related_artist_list(augmented_twice[2001:3000,])
augmented_thrice4 <- get_full_related_artist_list(augmented_twice[3001:4000,])

augmented_twice %>% dim
augmented_twice %>% tail


# full_artist_list <- all_followed
full_artist_list <- unique(bind_rows(all_followed, augmented_twice, augmented_thrice1))

full_artist_album_list <-
  lapply(1:dim(full_artist_list)[1], function(i){
    message(i,' ',full_artist_list$name[i],' ',full_artist_list$id[i])
    Sys.sleep(.05)
    albums_list <- get_artist_albums(id = full_artist_list$id[i],album_type='album')
    
    if(length(albums_list$items) > 0 ){
      simplified_list <- 
        cbind.data.frame(artist = full_artist_list$name[i],
                         simplify_result(albums_list,type='albums') %>% 
                           dplyr::rename(album_artist = artist),
                         stringsAsFactors=FALSE)
      simplified_list$artist <- tolower(simplified_list$artist)
      simplified_list$name <- tolower(simplified_list$name)
    } else {
      simplified_list <- data.frame()
    }
    simplified_list
  })

full_artist_album_df <- Reduce(rbind.data.frame,full_artist_album_list)
full_artist_album_df$artist <- str_replace_all(full_artist_album_df$artist , '[[:punct:]]','')
full_artist_album_df$name <- gsub('\\s+',' ',str_replace_all(full_artist_album_df$name , '[[:punct:]]',''))
# full_artist_album_df %>% filter(str_detect(name,"\\blive\\b")) %>% select(artist,name)
# full_artist_album_df %>% select(name) %>% unique
 
uniq_artist_album <- 
  full_artist_album_df %>% 
  select(artist,name) %>% 
  filter(!str_detect(name,'commentary'),
         !str_detect(name,'remastered'),
         !str_detect(name,'reissue'),
         !str_detect(name,'version'),
         !str_detect(name,'special edition'),
         !str_detect(name,"\\blive\\b")) %>% 
  unique
uniq_artist_album %>% dim
# write.csv(uniq_artist_album,file = '/media/roberto/Main Storage/Documents/R/metal-recommender/album_list/album_list.csv', quote = FALSE,row.names = FALSE)
write.csv(uniq_artist_album,file = '~/Documents/code/R/metal-recommender/album_list/album_list2.csv', quote = FALSE,row.names = FALSE)


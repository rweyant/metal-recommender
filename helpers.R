
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


## This might suck
get_columns <- function(column.names){
  
  is_tempo <- str_detect(column.names,'tempo_')
  is_general_tempo <- str_detect(column.names,'tempo_[a-z]')
  is_specific_tempo <- str_detect(column.names,'tempo_[1-9]')
  is_era <- str_detect(column.names,'era_')
  is_mood <- str_detect(column.names,'mood_')
  
  is_origin <- str_detect(column.names,'origin_')
  is_state_name <- 
    is_origin & str_detect(column.names,paste(str_replace(tolower(state.name),' ','_'),collapse='|')) & !str_detect(column.names,'city')
  
  is_country_name <- 
    (is_origin 
     & str_detect(column.names,paste(str_replace(tolower(world.cities$country.etc),' ','_'),collapse='|')) 
     & !str_detect(column.names,'city')
     & !str_detect(column.names,'milwaukee') 
     & !str_detect(column.names,'new_jersey')  ) | 
    str_detect(column.names,'united_states')  | 
    str_detect(column.names,'united_kingdom')
  
  ### Genre
  is_genre <- str_detect(column.names,'genre_')
  is_metal <- is_genre & (str_detect(column.names,'metal') | str_detect(column.names,'industrial'))
  is_punk <- is_genre & (str_detect(column.names,'punk') | str_detect(column.names,'hardcore'))
  is_rock <- is_genre & (str_detect(column.names,'rock') | str_detect(column.names,'alternative'))
  
  is_lfm <- str_detect(column.names,'lfm_')
  
  list(
    tempo_cols=which(is_tempo),
    tempo_general_cols=which(is_general_tempo),
    tempo_specific_cols=which(is_specific_tempo),
    tempo_specific_names=column.names[which(is_specific_tempo)],
    era_cols=which(is_era),
    era_cols_names=column.names[which(is_era)],
    origin_cols=which(is_origin),
    origin_states_cols=which(is_state_name),
    origin_country_cols=which(is_country_name),
    origin_country_names=column.names[which(is_country_name)],
    mood_cols=which(is_mood),
    genre_cols=which(is_genre),
    genre_metal_cols= which(is_metal),
    genre_punk_cols=which(is_punk),
    genre_rock_cols=which(is_rock),
    lfm_cols=which(is_lfm)
  )
}

### Summarize by variables
summarize_by_group <- function(cols,prefix,order_by=c('name','count')){
  counts <- full_df %>% select(cols) %>% summarize_each(funs(sum)) %>% as.data.frame
  names(counts) <- str_replace(names(counts),paste(prefix,'_',sep=''),'')
  count_df <- data.frame(var=names(counts),count=as.numeric(counts[1,]))
  if(order_by == 'name' ) count_df %<>% arrange(var)
  if(order_by == 'count' ) count_df %<>% arrange(count)
  names(count_df)[1] <- prefix
  count_df
}


column_match <- function(.data,pattern,type='n'){
  if(length(pattern) > 1) pattern <- paste(pattern,collapse='|')
  if(type=='n') return(which(str_detect(colnames(.data),pattern)) )
  if(type=='name') return(colnames(.data)[str_detect(colnames(.data),pattern)])
}

coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}

camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
categories <- c('genre_death_metal','genre_doom_metal','genre_thrash_metal')

compare_numeric_columns <- function(data,categories,columns,prefix='genre',x.axis.label=NULL){
  expr=paste(paste(categories,collapse=' > 0 | '),'> 0 ')
  summarized <- 
    data %>%
    # Filter based on whether bands are in specified categories
    filter_(interp(expr)) %>%
    select(artist,column_match(data,categories),columns) %>% 
    melt(measure.vars = categories,id.vars=c('artist',colnames(data)[columns]),variable.name = 'category') %>% 
    filter(value>0) %>% 
    select(-value) 

  # Delete non-numeric character in columns of interest
  colnames(summarized)[colnames(summarized) %in% colnames(data)[columns]] %<>% 
    str_replace_all('[A-z]','')

  cols <- column_match(summarized,'[^A-z]')
    
  summarized %>% 
    select(cols,category) %>%
    mutate(category= str_replace(category,prefix,'')) %>% 
    mutate(category=str_replace_all(category,'_',' ')) %>% 
    group_by(category) %>% 
    summarise_each(funs(mean)) %>% melt(id.vars = 'category',value.name = 'count',variable.name = 'var') %>% 
    ggplot(.,aes(x=as.numeric(as.character(var)),y=count,group=category,color=category))+
      geom_line(size=3)+
      scale_x_continuous(ifelse(is.null(x.axis.label),'VARIABLE',x.axis.label))+
      theme_bw()
}

##
## Artist Era
##
era_barchart <- function(data){
  summarized <- 
    data %>% 
    select(artist,rock,punk,metal,artist.cols$era_cols) %>% 
    melt(measure.vars = c('rock','punk','metal'),id.vars=c('artist',artist.cols$era_cols_names),variable.name = 'genre') %>% 
    filter(value==1) %>% 
    select(-value) 
  
  colnames(summarized) %<>%
    str_replace('_30','_1920') %>% 
    str_replace('_40','_1940') %>% 
    str_replace('_50','_1950') %>% 
    str_replace('_60','_1960') %>%
    str_replace('_70','_1970') %>%
    str_replace('_80','_1980') %>%
    str_replace('_90','_1990')
  
  for( i in as.character((5:9)*10)){
    if(sum(str_detect(colnames(summarized),i)) > 0)
      summarized[,paste('19',i,sep='')] <- apply(as.matrix(summarized[,str_detect(colnames(summarized),i)]),1,max)
  }
  
  summarized$`2000` <- apply(summarized[,str_detect(colnames(summarized),'2000')],1,max)
  summarized$`2010` <- apply(summarized[,str_detect(colnames(summarized),'2010')],1,max)
  
  decade_cols <- which(!str_detect(colnames(summarized),'[A-z]'))
  summarized %>% 
    select(decade_cols,genre) %>%
    group_by(genre) %>% 
    summarise_each(funs(sum)) %>% melt(id.vars = 'genre',value.name = 'count',variable.name = 'year') %>% 
    filter(count>0) %>% 
    ggplot(.,aes(x=year,y=count,group=genre,fill=genre))+
    geom_bar(stat='identity',position='dodge')+theme_bw()
}

# colnames(artist_summary)
# column_match(artist_summary,'origin','name')
# compare_numeric_columns(data=artist_summary,categories=c('genre_death_metal','genre_doom_metal','genre_neopsychedelic'),columns=artist.cols$tempo_specific_cols,x.axis.label='Tempo')
# era_barchart(artist_summary)


# compare_tempos(categories=c('genre_death_metal','genre_doom_metal'),columns=artist.cols$tempo_specific_cols,prefix='abc')
# compare_tempos(categories=c('origin_pittsburgh','origin_japan'),prefix='origin_')
# artist_summary %>% filter(origin_pittsburgh > 0)





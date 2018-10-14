
setwd("D:\\Work\\Coursera\\Final\\final\\en_US");
library("e1071")
library("caret")
library( "quanteda") 
library("irlba")
library("dplyr")
library(tidyr)

blogs <- readLines("en_us.blogs.txt", skipNul = TRUE , n=100  )
news <- readLines("en_US.news.txt" , skipNul = TRUE ,n=100 )
twitter <- readLines("en_US.twitter.txt" , skipNul = TRUE ,n=100  )
DataRaw = c(blogs, news, twitter)

DataRaw<-gsub("([^A-Za-z0-9 ])+", "", x = DataRaw)

DataRaw.tokens<- tokens(DataRaw, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE ,remove_url = TRUE,remove_twitter=TRUE,
                       ngrams = 1)
DataRaw.tokens <- tokens_tolower(DataRaw.tokens)
#DataRaw.tokens.dfm <- dfm(DataRaw.tokens, tolower = FALSE)
#topfeatures(blogs.tokens.dfm, 20)
#textplot_wordcloud(blogs.tokens.dfm, min_count = 50, random_order = FALSE,rotation = .25, color = RColorBrewer::brewer.pal(8,"Dark2"))
DataRaw.tokens = readRDS("DataRaw.tokens");


DataRaw.tokens.bigram = tokens_ngrams(DataRaw.tokens, n =2)
DataRaw.tokens.trigram = tokens_ngrams(DataRaw.tokens, n =3)

next1 = data_frame(Next = DataRaw.tokens)%>%separate(Next, c('Next'), '_')
next2 = data_frame(Next = DataRaw.tokens.bigram) %>%separate(Next, c('w1', 'Next'), '_')
next3 = data_frame(Next = DataRaw.tokens.trigram) %>%separate(Next, c('w1','w2', 'Next'), '_')


#blogs.tokens.bigram$count <-rep(1,length(blogs.tokens.bigram))
x1 = next1 %>%group_by(Next)  %>%summarize(count = n()) %>%filter(count >= 1)
x1 = x1  %>%arrange(desc(count) ,Next)%>% select(Next , count)


x2 = next2 %>%group_by(Next,w1)  %>%summarize(count = n()) %>%filter(count >= 1)
x2 = x2  %>% arrange(desc(count) ,w1)%>% select(w1,Next , count)

x3 = next3 %>%group_by(Next,w1,w2)  %>%summarize(count = n()) %>%filter(count >= 1)
x3 = x3  %>% arrange(desc(count) ,w1,w2)%>% select(w1,w2,Next , count)
saveRDS(x1,'x1')
saveRDS(x2,'x2')
saveRDS(x2,'x2')



predictnext<-function (str)
{
  
  a=strsplit(  tolower(trimws(str)), ' ');
  l = length(a[[1]])
  ind1 = 1
  ind2 = 2
  if(l>2)
  {
    ind2 = l;
    ind1 = l-1;
    
  }
  inputw1 = a[[1]][ind1]
  inputw2 = a[[1]][ind2]
  i=1;
  output <- rep(NA,6)
  if(!is.na(inputw2))
  {
    
    
    
    
    o2= x2[x2$w1==inputw2   , "Next"][1:3,1]
    
    o3 = x3[x3$w1==inputw1 && x3$w2==inputw2  ,"Next"][1:3,1]
    for (v in o3[[1]]) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
      
    }
    
    for (v in o2[[1]]) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
    
    
    
  }
  
  if(is.na(inputw2) && !is.na(inputw1))
  {
    o1 = ( x2[x2$w1==inputw1   ,"Next" ])[1:3,1]
    
    for (v in o1[[1]]) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
    
    
  }
  
  if(length(which(is.na(output)))==6)
  {
    o0 =  x1[1:3   ,"Next" ][1:3,1];
    for (v in o0[[1]]) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
  }
  
  return(output);
  
}

predictnext('my' )
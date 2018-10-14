x1<-readRDS('PredictText/data/x1')
x2<-readRDS('PredictText/data/x2')
x3<-readRDS('PredictText/data/x3')


predictnext<-function (str)
{
  
  a=strsplit( tolower( trimws(str)), ' ');
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
    
    
    
    
    o2= x2[x2$w1==inputw2   , "Next"][1:3]
    
    o3 = x3[x3$w1==inputw1 && x3$w2==inputw2  ,"Next"][1:3]
    for (v in o3) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
      
    }
    
    for (v in o2) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
    
    
    
  }
  
  if(is.na(inputw2) && !is.na(inputw1))
  {
    o1 =  x2[x2$w1==inputw1   ,"Next" ][1:3]
    
    for (v in o1) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
    
    
  }
  
  if(length(which(is.na(output)))==6)
  {
    o0 =  x1[1:3   ,"Next" ][1:3];
    for (v in o0) {
      if(!is.na(v))
      {output[i] = v
      i = i+1}
    }
  }
  
  return(output);
  
}
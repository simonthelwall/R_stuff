NHSvalidation2 <- function(NHSnumber){
  NHSnumber <- as.character(NHSnumber)
  RV <<- NA
  ifelse(
    nchar(NHSnumber) != 10, RV <- 0, uniformN(NHSnumber)
                 )
  return(RV)
}

splitNHS <- function(NHSnumber){
  a<-as.numeric(substr(NHSnumber,1,1))
  b<-as.numeric(substr(NHSnumber,2,2))
  c<-as.numeric(substr(NHSnumber,3,3))
  d<-as.numeric(substr(NHSnumber,4,4))
  e<-as.numeric(substr(NHSnumber,5,5))
  f<-as.numeric(substr(NHSnumber,6,6))
  g<-as.numeric(substr(NHSnumber,7,7))
  h<-as.numeric(substr(NHSnumber,8,8))
  i<-as.numeric(substr(NHSnumber,9,9))
  j<-as.numeric(substr(NHSnumber,10,10))
  return(list(a, b, c, d, e, f, g, h, i, j))
}

uniformN <- function(NHSnumber){
#  print("uniformN called")
  z <- splitNHS(NHSnumber)
#  RV <- NA
  ifelse( 
    ( (z[[1]]==z[[2]]) & (z[[2]]==z[[3]]) & (z[[3]]==z[[4]]) & (z[[4]]==z[[5]]) & (z[[5]]==z[[6]]) & 
        (z[[6]]==z[[7]]) & (z[[7]]==z[[8]]) & (z[[8]]==z[[9]]) & (z[[9]]==z[[10]]) ) , RV <<- 0, mo(NHSnumber)
  )
  return(RV)  
}

mo <- function(NHSnumber){
#  print("mo called")
  z <- splitNHS(NHSnumber)
#  RV <- NA
  Modulus<-((z[[1]]*10)+(z[[2]]*9)+(z[[3]]*8)+(z[[4]]*7)+(z[[5]]*6)+(z[[6]]*5)+(z[[7]]*4)+(z[[8]]*3)+(z[[9]]*2))
  Modulus<-(11-(Modulus%%11))
  RV <- "post modulus NA"
  ifelse(
    (Modulus==z[[10]]) | ((Modulus==11) & (z[[10]]==0)), RV <<- 1, RV <<- 0
  )
  return(RV)
}

NHSvalidation <- function(NHSnumber){
  
  NHSlength<-nchar(NHSnumber)
  
  a<-as.numeric(substr(NHSnumber,1,1))
  b<-as.numeric(substr(NHSnumber,2,2))
  c<-as.numeric(substr(NHSnumber,3,3))
  d<-as.numeric(substr(NHSnumber,4,4))
  e<-as.numeric(substr(NHSnumber,5,5))
  f<-as.numeric(substr(NHSnumber,6,6))
  g<-as.numeric(substr(NHSnumber,7,7))
  h<-as.numeric(substr(NHSnumber,8,8))
  i<-as.numeric(substr(NHSnumber,9,9))
  j<-as.numeric(substr(NHSnumber,10,10))
  
  if ((a==b)&(b==c)&(c==d)&(d==e)&(e==f)&(f==g)&(g==h)&(h==i)&(i==j))
    {UniformNumberCheck<-1}
  else
  
  {UniformNumberCheck<-0}
  
  Modulus<-((a*10)+(b*9)+(c*8)+(d*7)+(e*6)+(f*5)+(g*4)+(h*3)+(i*2))
  Modulus<-(11-(Modulus%%11))
  
  if (
    
    ((Modulus==j) & (UniformNumberCheck!=1) & (NHSlength==10))|((Modulus==11) & (j==0) & (UniformNumberCheck!=1) & (NHSlength==10)))
  {ReturnValue<-1}
  else
  {ReturnValue<-0}
  
  return(ReturnValue)
}

# Use: numbers$valid<-sapply(numbers$NHSn,NHSvalidation)
# Slightly adapted from: http://healthanalyst.wordpress.com/2011/08/21/nhs-number-validation/

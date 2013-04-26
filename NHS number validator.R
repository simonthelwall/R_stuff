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

niceNames <- function(x){
  z <- tolower(names(x))
  z <- str_replace_all(z, "\\_|\\s", "\\.")
  return(z)
}

simpleCap<-function(x){
  s<-as.character(x)
  s<-paste(toupper(substring(s,1,1)),tolower(substring(s,2)),sep="")
}

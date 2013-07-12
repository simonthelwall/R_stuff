# Test of two proportions. Supply two numerators and denominators.
testTwoProps<-function(n1,d1,n2,d2){
  p1<-n1/d1
  p2<-n2/d2
  pbar<-p1-p2
  se<-sqrt(pbar*(1-pbar)*((1/d1)+(1/d2)))
  cise<-sqrt(((p1*(1-p1))/d1)+(p2*(1-p2))/d2)
  lci<-pbar-1.96*cise
  uci<-pbar+1.96*cise
  z<-pbar/se
  p<-2*pnorm(-abs(z))
  cat("Proportion 1:",round(p1,4),"\t Proportion 2:",round(p2,4),"\n",
      "Difference of proportions:",round(pbar,4),"\n Std err of difference:",round(se,4),
      "\n LCI of diff:",round(lci,4),"\t UCI of diff",round(uci,4),
      "\n z-value:",round(z,4),"p-value",p,"\n\n")
  }
#testTwoProps(13,19,6,23)
#should give 0.68, 0.26 and p~0.0057

##Std error of the mean:
sem<-function(x){
  sd(x,na.rm=TRUE)/sqrt(length(x,na.omit=TRUE))
}

# Risk ratio
riskRatio<-function(n1,d1,n2,d2){
 r1<-n1/d1
 r2<-n2/d2
 rr<-r1/r2
 return(rr)
}
#riskRatio(410,560,425,577)

# Confidence interval for a risk ratio
cirr<-function(n1,d1,n2,d2){
  r1<-n1/d1
  r2<-n2/d2
  rr<-r1/r2
  logrr<-log(rr)
  se<-sqrt((1/n2)-(1/d2)+(1/n1)-(1/d1))
  lci<-exp(logrr-(1.96*se))
  uci<-exp(logrr+(1.96*se))
  return(paste(round(lci,2),"-",round(uci,2),sep=""))
}
#serr(410,560,425,577)

# function for std error of a proportion or percentage
seP <-function(x,n,z){
  # x is proportion, n is number of observations, z should be vector of length 1 to establish whether prop or perc
  if(tolower(z) == "percent"){
    se <- sqrt((x*(100-x))/n)  
  }
  else if(tolower(z) == "proportion"){
    se <- sqrt((x*(1-x))/n)  
  }
  else print("Please indicate either percent or proportion")
  return(se)
}

# For use with regression models. Extract p values and odds ratios.
pWrapper<-function(x){
  round(with(x,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail=FALSE)),3)
}
orWrapper<-function(x){
  cbind(OR=exp(coef(x)),exp(confint(x)),p_val=round(summary(x)$coefficients[,4],6))
}

# Convert text to sentence case
simpleCap<-function(x){
  s<-as.character(x)
  s<-paste(toupper(substring(s,1,1)),tolower(substring(s,2)),sep="")
  #  s<-as.factor(s)
}

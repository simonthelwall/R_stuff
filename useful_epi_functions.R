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

# Confidence interval for a proportion or percent. Requires seP above.
ciP <- function(x, n, p = "percent", bound = "lower"){
  if(tolower(bound) == "lower"){
    se <- seP(x,n,p)
    lci <- x - (1.96*se)
    return(lci)
  } else if(tolower(bound) == "upper"){
    se <- seP(x,n,p)
    uci <- x + (1.96*se)
    return(uci)
  }
    else print("Please choose upper or lower bounds for confidence interval.")
}

# Confidence interval for a mean. Vectorised, intended for use with ddply. 
ciMean <- function(x, bound="lower", na.rm = TRUE){
  if(tolower(bound) == "lower"){
    lci <- mean(x, na.rm = na.rm) - (1.96 * sd(x, na.rm = na.rm))
    return(lci)
  } else if(tolower(bound) == "upper"){
    uci <- mean(x, na.rm = na.rm) + (1.96 * sd(x, na.rm = na.rm))
    return(uci)
  }
  else print("Please choose upper or lower bounds for confidence interval.")
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

# Simple odds ratios, confidence intervals and p-values. 
# requires as.numeric() for OR calculation as large datasets can cause integer overflow. 
orCalc <- function(outcome, riskf){
  # x should be outcome, y stratifying variable. 
  # cribbed from mhodds in epicalc package
  # with reference to p157 + p164 of Kirkewood and Sterne, Essential Medical Statistics. 2nd Ed
  tab <- table(riskf, outcome)
  print(tab)
  cat("\n")
  or <- c(1:dim(tab)[1]) # create vector of same length as table rows. 
  se.log.or <- c(1:dim(tab)[1])
  lci <- c(1:dim(tab)[1])
  uci <- c(1:dim(tab)[1])
  z <- c(1:dim(tab)[1])
  p <- c(1:dim(tab)[1])
  for (i in 1:dim(tab)[1]){
    or[i] <- (as.numeric(tab[1,1])*as.numeric(tab[i,2]))/(as.numeric(tab[1,2])*as.numeric(tab[i,1]))
    se.log.or[i] <-sqrt(1/tab[1,1] + 1/tab[1,2] + 1/tab[i,1] + 1/tab[i,2])
    lci[i] <- or[i]/exp(1.96 * se.log.or[i])
    uci[i] <- or[i]*exp(1.96 * se.log.or[i])
    z[i] <- log(or[i])/se.log.or[i]
    p[i] <- 2*(1-pnorm(abs(z[i]))) #?
  }
  m <- as.matrix(cbind(or, se.log.or, lci, uci, z, p))
  return(m)
}

# Wrapper for calculating exponentiated forms of linear combinations of interaction terms from regression models
# including confidence intervals. Returns list in order to facilitate integration into a dataframe. 
lincom <- function(svycontrast_object){
  require(survey)
  if (class(svycontrast_object)=="svystat"){
    or <- exp(svycontrast_object[1])
    lci <- or / exp(1.96*sqrt(attributes(svycontrast_object)$var))
    uci <- or * exp(1.96*sqrt(attributes(svycontrast_object)$var))
    return(as.list(c(or, lci, uci)))
  } else {
    print("Requires object of class svystat")
  }
}

# useful wrapper to extract p value from linear regression object. 
# http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- unname(pf(f[1],f[2],f[3],lower.tail=F))
  attributes(p) <- NULL
  return(p)
}

pValFun <- function(x){
  if (x < 0.001) print("p < 0.001")
  else if (x < 0.01) print("p < 0.01")
  else if (x < 0.05) print("p < 0.05")
  else if (x < 0.1) print("p < 0.1")
  else print(paste("p = ", round(x, 2), sep = ""))
}

# It's worth knowing that pValFunTEx can be used on a column of a dataframe using lapply(df$var, pValFunTex)
pValFunTex <- function(x){
  if (x < 0.001) pval <- "p \\textless 0.001"
  else if (x < 0.01) pval <- "p \\textless 0.01"
  else if (x < 0.05) pval <- "p \\textless 0.05"
  else if (x < 0.1) pval <- "p \\textless 0.1"
  else pval <- paste("p = ", round(x, 2), sep = "")
  return(pval)
}

# Calculating change in mean square error for assessing multicollinearity
# as per Neil Pearce.
rRMSE <- function(exposure, reduced.model, full.model){
  beta.r <- summary(reduced.model)$coefficients[exposure,1]
  beta.full <- summary(full.model)$coefficients[exposure,1]
  se.r <- summary(reduced.model)$coefficients[exposure,2]
  se.full <- summary(full.model)$coefficients[exposure,2]
  ceChange <- beta.r-beta.full
  rMSE.full <- sqrt((se.full - se.full)^2 + se.full^2)
  rMSE.r <- sqrt(((beta.r - beta.full)^2)+(se.r^2))
  dMSE <- (beta.r-beta.full)^2-((se.full^2)-(se.r^2))
  rel.rMSE <- rMSE.r/rMSE.full
  cat("beta reduced:", beta.r, "\n", sep = " ")
  cat("beta full:", beta.full, "\n", sep = " ")
  cat("se reduced:", se.r, "\n", sep = " ")
  cat("se full:", se.full, "\n", sep = " ")
  cat("Root MSE reduced:", rMSE.r, "\n", sep = " ")
  cat("Root MSE full:", rMSE.full, "\n", sep = " ")
  cat("Change in estimate:", ceChange, "\t(", round((ceChange/beta.full)*100,2), "%)", "\n", sep = " ")
  cat("Relative RMSE:", rel.rMSE, "\n", sep = " ")
  cat("\tA negative change in MSE indicates collinearity. ")
}

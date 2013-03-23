library(survival)
library(ggplot2)
data(aml)
head(aml) # looks like status 0 is alive and 1 is died. Data is right-censored. 

length(aml$time)

Surv(aml$time, aml$status)

# kaplan-meier ####
fit1 <- survfit(Surv(aml$time[1:11], aml$status[1:11])~1)
summary(fit1)
str(summary(fit1))
# Extract data into dataframe for plotting. 
summfit1 <- as.data.frame(cbind(summary(fit1)$time, summary(fit1)$surv, 
                          summary(fit1)$upper,summary(fit1)$lower))
names(summfit1) <- c("time","surv","upper","lower")
head(summfit1)
# Make attractive plot
p <- ggplot(summfit1,aes(x=time, y=surv)) + geom_line() 
p <- p + geom_line(aes(y=lower), linetype=2) + geom_line(aes(y=upper), linetype=2)
p
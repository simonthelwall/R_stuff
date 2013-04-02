library(RCurl)

mylink<-getURL("https://docs.google.com/spreadsheet/pub?key=0Ak-3NcJ0Vkl4dE9SUktOZEpxLWZHWngxWUFSTWJBVlE&single=true&gid=0&output=csv")
df<-read.csv(textConnection(mylink), header = TRUE, sep = ",", stringsAsFactors = FALSE)
df$Started <- as.Date(df$Started, format="%d/%m/%Y")
df$Ended <- as.Date(df$Ended, format="%d/%m/%Y")
df$diff <- as.numeric(df$Ended-df$Started, units = "days")
head(df)
(median(df$diff, na.rm = TRUE)/365.25) # Median survival for discontinued google services, years.
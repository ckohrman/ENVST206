datT <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/2338035.csv")
#install lubridate
install.packages("lubridate")
#make date factor data
datT$DATE <- as.factor(datT$DATE)

#plot min and max temp against  date
plot(datT$DATE, datT$TMIN,
     type = "b",
     pch = 19,
     ylab = "minimum temperature (C)",
     xlab = "date")
plot(datT$DATE, datT$TMAX,
     type = "b",
     pch = 19,
     ylab = "maximum temperature (C)",
     xlab = "date")
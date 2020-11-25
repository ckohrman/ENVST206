datT <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/2338035.csv")
#install lubridate
install.packages("lubridate")
library(lubridate)
#make date factor data
datT$DATE <- as.Date(datT$DATE, "%Y-%m-%d" )

datT$year <- year(datT$DATE)

datT$month <- month(datT$DATE)

summer <- datT[datT$month >= 6 & datT$month < 9,]

winter <- datT[datT$month >= 1 & datT$month < 3,]


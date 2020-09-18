datW<- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/a02/noaa2011124.csv")
#tree height vector in meters
heights <- c(20,24,32,40)
#convert to cm
heights_cm <-heights*100
heights_cm
#see first tree height
heights[1]
#see second and third tree heights
heights[2:3]
help(matrix)
#set up 2 column matrix
mat1 <- matrix(c(1,2,3,4,5,6,7,8), ncol=2, byrow=TRUE)
mat1
#practice matrix 2
mat2<-matrix(c(1,2,3,4,5,6,7,8), ncol=2, byrow=FALSE)
mat2
mat2[1,2]
mat2[1,]
mat2[,2]
#dataframe info
str(datW)
#create factor data for names row
datW$NAME<- as.factor(datW$NAME)

#QUESTION 2
#example numeric data
num_ex <- c(1.1,1.2,2.1,2.2,3)
#example integer data
int_ex <- c(1,1,2,2,3)
#example character data
chr_ex <- c("1a","1b","2a","2b",3)
#example factor data
fac_ex <- as.factor(chr_ex)
fac_ex

#establish site names
levels(datW$NAME)
#check mean max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
#check mean max temp for Aberdeen
#with na.rm argument set to true to ingore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#check standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#calculate average daily temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get mean for all sites
#by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#comma after FUN applies arguments  
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
#rename columns
#MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp
#convert site names to numbers for factor data type
#must reference the level output or look at data row to see the name
datW$siteN <- as.numeric(datW$NAME)

#make histogram for the first site in levels
#main= is the title name argument
#use actual site name
hist(datW$TAVE[datW$siteN == 1], 
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "average daily temp (degrees C)",
     ylab = "relative frequency",
     col = "grey75",
     border = "white")
help(hist)

#QUESTION $
#histogram for site 4
hist(datW$TAVE[datW$siteN == 4], 
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "average daily temp (degrees C)",
     ylab = "relative frequency",
     col = "grey75",
     border = "white")

help(dnorm)
#pnorm(value to evaluate at, mean, standard deviation)
#will evaluate for all values below
pnorm(0,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#pnorm 5 and below
pnorm(5,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#pnorm 5-0
pnorm(5,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#pnorm 20 and below
pnorm(20,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#pnorm above 20
1 - pnorm(20,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#qnorm = value at which all values + below = probability in my argument
#qnorm 95th quantile
qnorm(.95,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#QUESTION 5
#qnorm to establish high temp threshold 
qnorm(.95,
      mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))
#threshold is 18.51026 degC 
#pnorm for hight temp threshold and above w/ adjusted mean
1 - pnorm(18.51026,
          mean(datW$TAVE [datW$siteN ==1], na.rm=TRUE) + 4,
          sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#QUESTION 6
#mean prcp Aberdeen
mean(datW$PRCP[datW$siteN ==1], na.rm=TRUE)
#prcp histogram
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily precipitation (cm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#QUESTION 7
#use aggregate and sum to find annual prcp
annualPrcp <- aggregate(datW$PRCP,
              by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
#rename columns
colnames(annualPrcp) <- c("NAME","Year", "Prcp_mm")

#QUESTION 8
#annual prcp histogram for Aberdeen
hist(annualPrcp$Prcp_mm[annualPrcp$NAME == "ABERDEEN, WA US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual precipitation total (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
#annual prcp histogram for Mandan Station
hist(annualPrcp$Prcp_mm[annualPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual precipitation total (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#QUESTION 9
#likelihood of <700mm prcp at Mandan
#pnom 700 and below
pnorm(700,
      mean(annualPrcp$Prcp_mm[annualPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE),
      sd(annualPrcp$Prcp_mm[annualPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE))
#likelihood of <700mm prcp in Aberdeen
#pnom 700 and below
pnorm(700,
      mean(annualPrcp$Prcp_mm[annualPrcp$NAME == "ABERDEEN, WA US"],na.rm=TRUE),
      sd(annualPrcp$Prcp_mm[annualPrcp$NAME == "ABERDEEN, WA US"],na.rm=TRUE))

#read in data
datT <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/2338035.csv")
#install lubridate
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)

#make date factor data
datT$DATE <- as.Date(datT$DATE, "%Y-%m-%d" )
#create year column
datT$year <- year(datT$DATE)
#create month column
datT$month <- month(datT$DATE)


#create summer datatframe
summer <- datT[datT$month >= 7 & datT$month < 9,]
summer <- na.omit(summer)
summerAve <- aggregate(summer$TMAX, 
                       by=list(summer$NAME, summer$year), 
                       FUN="mean", na.rm=TRUE)
#rename columns
names(summerAve)[names(summerAve) == "Name"] <- "Station"
names(summerAve)[names(summerAve) == "Group.2"] <- "year"
names(summerAve)[names(summerAve) == "x"] <- "Tmax"

#create winter dataframe
winter <- datT[datT$month >= 1 & datT$month < 3,]
winter<- na.omit(winter)
winterAve <- aggregate(winter$TMIN, 
                       by=list(winter$NAME, winter$year), 
                       FUN="mean", na.rm=TRUE)
#rename columns
colnames(winterAve) 
names(winterAve)[names(winterAve) == "Name"] <- "Station"
names(winterAve)[names(winterAve) == "Group.2"] <- "year"
names(winterAve)[names(winterAve) == "x"] <- "Tmin"

#combine summer and winter data
temperature <- data.frame(year = c(summerAve$year, winterAve$year), 
                          temp = c(summerAve$Tmax, winterAve$Tmin),
                          season = c(rep("summer", nrow(summerAve)), 
                                     rep("winter", nrow(winterAve))))

#plot summer and winter
ggplot(data = temperature, aes(x=year, y= temp, color=season))+
         geom_point()+ #make points at data point
         geom_path()+ #use lines to connect data points
         theme_classic()+
         scale_color_manual(values = c("goldenrod2", "slateblue2"))+
         labs(x="Year", y="Average Max Summer and Min Winter Temperatures (°F)") #make axis labels

#create new temperature dataframe for regression
temperature2 <- full_join(winterAve, summerAve, by="year", na.rm=TRUE)
temperature3 <- na.omit(temperature2)

#plot data
ggplot(data = temperature3, aes(x = Tmax, y=Tmin, color=Station.x) )+ #data for plot
  geom_point()+ #make points at data point
  theme_classic()+
  scale_color_manual(values = c("slateblue2"))+
  labs(x="Tmax", y="Tmin") #make axis labels

#get residuals
temp.mod <- lm(temperature3$Tmax ~ temperature3$Tmin)
temp.res <- rstandard(temp.mod)

#assess normality
qqnorm(temp.res)
qqline(temp.res)
shapiro.test(temp.res)

#check for trends with residuals plot
plot(temperature3$Tmin, temp.res,
     xlab="minimum temperature (°F)",
     ylab="standardized residual")
abline(h=0)

#get regression results
summary(temp.mod)

datW <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/a02/noaa2011124.csv")
datW$NAME <- as.factor(datW$NAME)
nameS <- levels(datW$NAME)
nameS

#dataframe with precp year and name
#remove na using na.omit
datP <- na.omit(data.frame(NAME = datW$NAME,
                           year = datW$year,
                           PRCP = datW$PRCP))
#total annual precip (mm)
precip <- aggregate(datW$PRCP, 
                    by = list(datW$NAME, datW$year),
                    FUN = "sum",
                    na.rm = TRUE)
#use aggregate to get total annual precip
precip <- aggregate(datP$PRCP, 
                     by = list(datP$NAME, datP$year),
                     FUN = "sum",
                     na.rm = TRUE)
#rename columns
colnames(precip) <- c("name", "year", "totalP" )
#add x column from agg for length of obsevations per year
precip$ncount <- aggregate(datP$PRCP, 
                           by = list(datP$NAME, datP$year),
                           FUN = "length")$x
#make a new dataframe
pr <- precip[precip$ncount >=364, ]

#cali and ny precip data
ca <- pr[pr$name == nameS[2],]
ny <- pr[pr$name == nameS[5],]

#cali precip plot
plot(ca$year, ca$totalP)

#cali precip plot take 2
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year",
     yaxt = "n",
     ylim =c(0, 1600))


#add y axis
axis(2, seq(200,1600, by=400), las=2 )

#add ny data
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1,
       bty="n")

#QUESTION 2
#create maxtemp dataframe
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
tmax <- aggregate(datT$TMAX, 
                    by=list(datT$NAME,datT$year), 
                    FUN="mean", 
                    na.rm=TRUE)
colnames(tmax) <- c("NAME","year","maxT")
#add x column
tmax$ncount <- aggregate(datT$TMAX, 
                           by=list(datT$NAME,datT$year), 
                           FUN="length")$x
#new dataframe
tm  <- tmax[tmax$ncount >= 364,]
#ny and nd
nd <- tm[tm$NAME == nameS[3], ]
ny <- tm[tm$NAME == nameS[5], ]

#plot
plot(nd$year, nd$maxT,
     type = "b",
     pch = 19,
     ylab = "Average Maximum Temperature (C)",
     xlab = "Year",
     ylim =c(8.5, 14.5),
     yaxt = "n")
#add ny
points(ny$year, ny$maxT,
       type = "b",
       pch = 19,
       col="tomato3")
#add y axis
axis(2, seq(8.5,14.5, by=.5), las=2 )
#add legend
legend("bottomright", #position
       c("North Dakota", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, 
       lwd=1,
       bty="n") 

#ggplot
install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr, 
       aes(x = year, y=totalP, color = name ) )+ #data
  geom_point()+ #make points
  geom_path()+ #use lines
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic()+ #change plot theme
  scale_color_manual(values = c("indianred3","slategray3", 
                                "aquamarine3", "goldenrod3",
                                "deepskyblue3"))

#violin plots
ggplot(data = datW, aes(x=NAME, y=TMIN))+ #daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #blue violin plot 
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ 
  theme_classic() 

#az data
sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

#specify date format
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

#ggplot 2
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

#gg bar graph
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#QUESTION 8
#1974, Wa
sub2 <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
sub2$DATE <- as.Date(sub2$DATE,"%Y-%m-%d")
#temp plot
ggplot(data=sub2, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")
#precip plot
ggplot(data=sub2, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#QUESTION 9
datTmin <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMIN=datW$TMIN))
tmin <- aggregate(datTmin$TMIN, 
                  by=list(datTmin$NAME,datTmin$year), 
                  FUN="mean", 
                  na.rm=TRUE)
colnames(tmin) <- c("NAME","year","minT")
tmin$ncount <- aggregate(datTmin$TMIN, 
                         by=list(datTmin$NAME,datTmin$year), 
                         FUN="length")$x

minsub <- datTmin[datTmin$NAME == nameS[1] & datTmin$year >= 2000,]

ggplot(data = minsub, aes(group=year, x=year, y=TMIN))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic()+
  labs(x="year", y="minimum temperature (C)")+
  ggtitle("Minimum Temperatures in Aberdeen, WA")



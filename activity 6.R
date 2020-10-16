install.packages("rgdal")
install.packages("dplyr")
install.packages("sp")

library(dplyr)

#shapefiles
#glaciers in 1966
g1966 <- readOGR("/Users/clarakohrman/Documents/ENVST206 Data/a06/GNPglaciers/GNPglaciers_1966.shp")
#glaciers in 2015
g2015 <- readOGR("/Users/clarakohrman/Documents/ENVST206 Data/a06/GNPglaciers/GNPglaciers_2015.shp")
str(g2015)

#glacier map
plot(g1966, col = "lightblue2", border = "grey50")

#see data
head(g2015@data)
g1966@proj4string

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier names
g2015@data$GLACNAME <- 
  ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
  ifelse(g2015@data$GLACNAME ==  "Miche Wabun", 
                              "Miche Wabun Glacier",
   as.character(g2015@data$GLACNAME)))

#combine data
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate change in area 1966-2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

#scatterplot
plot(gAll$area66, gAll$gdiff, 
    pch=19, xlab="glacier area 1966 (m^2)", 
    ylab="% change in area (m^2)")

#join data with spatial data table
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
#shade polygons, add title and border color
spplot(g1966, "gdiff", main="% change in area", col="transparent")

#QUESTION 8
#mean of %loss
mean(gAll$gdiff)
sd(gAll$gdiff)

#QUESTION 9
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Boulder Glacier Shrinkage", col="slategray")
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder15, col = "black", add=TRUE)
legend("bottomleft", 
  c("area in 1966", "area in 2015"),
  fill= c("slategray", "black"),
  bty="n")

pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly66, main = "Pumpelly Glacier Shrinkage", col="slategray")
pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly15, col = "black", add=TRUE)
legend("bottomright", 
       c("area in 1966", "area in 2015"),
       fill= c("slategray", "black"),
       bty="n")



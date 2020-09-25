datB <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/beaver_dam.csv")

#make scatterplot
plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     ylab = "surface water area (ha)",
     xlab = "number of beaaver dams")

#regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
dam.res <- rstandard(dam.mod)

#qq plot to assess normality
qqnorm(dam.res)
#qq line
qqline(dam.res)

#residual plot
plot(datB$dams.n, dam.res,
     xlab = "beaver dams",
     ylab = "standardized residual")
#add line at zero
abline(h=0)

#analyze results
summary(dam.mod)
#make plot
plot(datB$dams.n, datB$area.ha,
    pch = 19,
    col = "royalblue4",
    ylab = "surface water area (ha)",
    xlab = "number of beaaver dams")
#add thick line
abline(dam.mod, lwd=2)

#maple data
pheno <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/red_maple_pheno.csv")

#panel of plots
par(mfrow=c(1,2))
plot(pheno$Tmax, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "day of leaf out",
     xlab = "max temperature (C)")
plot(pheno$Prcp, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "day of leaf out",
     xlab = "precipitation")
     
#make more plots
#latitude
plot(pheno$Lat, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "day of leaf out",
     xlab = "latitude")
#mintemp
plot(pheno$Tmin, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "day of leaf out",
     xlab = "min temperature")
#elevation
plot(pheno$elev, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "day of leaf out",
     xlab = "elevation")
#rural/urban
pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$siteDesc, pheno$doy,
     ylab = "day of leaf out",
     xlab = "site type")

#colinearity check
plot( ~  pheno$Lat + pheno$Tmax 
      + pheno$Tmin +pheno$Prcp + 
        pheno$elev + pheno$siteDesc)

#regression time!
#quantify site types
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
#Run regression
mlr <- lm(pheno$doy ~  pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)
#check data
#residuals
ml.res <- rstandard(mlr)
#qqplot
qqnorm(ml.res)
qqline(ml.res)
#residual plot
mlFitted <- fitted(mlr)
plot(mlFitted, ml.res,
     xlab = "fitted values",
     ylab = "standardized residual")
abline(h=0) 

#interpret regression
summary(mlr)

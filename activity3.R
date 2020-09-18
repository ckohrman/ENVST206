ch4 <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)
#boxplot
plot(ch4$CH4_Flux ~ ch4$herbivory)
plot(ch4$CH4_Flux ~ ch4$herbivory, 
     xlab = "Treatment", 
     ylab = "CH4 fluxes (mgC m -2 day-1)")

#shapiro wilkes test - normality check
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

#bartlett test - variance check
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#t test
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#insect data
datI <- read.csv("/Users/clarakohrman/Documents/ENVST206 Data/insect_richness.csv")
#convert to factors
datI$urbanName <- as.factor(datI$urbanName)

#QUESTION 4 test assumptions
#shapiro test
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
shapiro.test(datI$Richness[datI$urbanName == "Natural"])
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
#bartlett test
bartlett.test(datI$Richness ~ datI$urbanName)

#ANOVA test
#specify linear model
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run ANOVA
in.aov <- aov(in.mod)
#print
summary(in.aov)

#tukey test
#run tukey hsd
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT
#make plot
#resize axis using cex.axis 
plot(tukeyT, cex.axis=0.75)

#means
tapply(datI$Richness, datI$urbanName, "mean")

#chi-squared data
#contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("not protected", "protected")
rownames(species) <- c("declining", "stable/increasing")
#mosaic plot with labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")
#chi-squared test
chisq.test(species)

ch4 <- read.csv("/Users/margaretmanning/Desktop/GitHub/Activity3/lemming_herbivory.csv")

#make data factor data so can work with it
ch4$herbivory <- as.factor(ch4$herbivory)

#create a plot to see the range
plot(ch4$CH4_Flux ~ ch4$herbivory)

#shapiro checks if data are normally dist (pvalue<0.05)
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#barlett checks if the variance of one grp is the same as variance of other (requires data are normally dist)
#dependent variable ~ independent variable 
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#we've confirmed we can use t-test for data
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#one sample t-test 
help("t.test")

#read in insect data
datI <- read.csv("/Users/margaretmanning/Desktop/Data Science/a03/insect_richness.csv")

datI$urbanName <- as.factor(datI$urbanName)

#running an ANOVA test using lm and aov
#specifiy model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)

#run Tukey HSD (compares differences btw grps)
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

#make a plot to visualize Tukey
#make axes labels smaller than usual to fit on plot using cex.axis
plot(tukeyT, cex.axis=0.75)

#calculate means across factor data 
tapply(datI$Richness, datI$urbanName, "mean")

#set up contingency table
species <- matrix(c(18,8,15,32), ncol = 2, byrow = TRUE)
colnames(species) <- c("Not Protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot with an informative title and axes labels 
mosaicplot(species, xlab="Population Status", ylab="Legal Protection",
           main="Legal Protection Impacts on Populations")

#Conduct a chi-squared test 
chisq.test(species)
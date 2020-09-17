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
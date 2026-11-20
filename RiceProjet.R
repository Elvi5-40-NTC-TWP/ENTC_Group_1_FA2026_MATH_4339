library(readr)
rice_data <- read_csv("rice_data.csv")

plot(rice_data)
dim(rice_data)

library(corrplot)
corrplot(cor(rice_data[,-8]),method="number",type="upper")

# Assumption
library(MVN)
library(biotools)
rice.mvn = mvn(rice_data,subset = "Class")
rice.mvn$multivariate_normality # Not Normal
boxM(rice_data[,1:7],rice_data$Class) # No Equal Variance

var = cbind(rice_data$Area,
            rice_data$Perimeter,
            rice_data$Major_Axis_Length,
            rice_data$Minor_Axis_Length,
            rice_data$Eccentricity,
            rice_data$Convex_Area,
            rice_data$Extent)
rice.ma = manova(var~Class,data=rice_data)
summary(rice.ma,test="Wilks")

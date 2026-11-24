library(readr)
rice_data <- read_csv("RStudio/Multivariate Statistics - RStudio/Multivariate Project/rice_data.csv")
#-------------------------------------------------------------------------------
plot(rice_data)
dim(rice_data)
#-------------------------------------------------------------------------------
library(corrplot)
corrplot(cor(rice_data[,-8]),method="number",type="upper")
#-------------------------------------------------------------------------------
# Assumption for MANOVA
library(MVN)
library(biotools)
rice.mvn = mvn(rice_data,subset = "Class")
rice.mvn$multivariate_normality # Not Normal
boxM(rice_data[,1:7],rice_data$Class) # No Equal Variance
#-------------------------------------------------------------------------------
# MANOVA Test
var = cbind(rice_data$Area,
            rice_data$Perimeter,
            rice_data$Major_Axis_Length,
            rice_data$Minor_Axis_Length,
            rice_data$Eccentricity,
            rice_data$Convex_Area,
            rice_data$Extent)
rice.ma = manova(var~Class,data=rice_data)
summary(rice.ma,test="Wilks")
# Reject the Null Hypothesis, there is evidence that suggest a difference in the means vectors
#-------------------------------------------------------------------------------
# Logistic Regression
rice.glm = glm(as.factor(Class)~.,family="binomial",data=rice_data)
summary(rice.glm)

par(mfrow = c(2,2))
plot(rice.glm)

step(rice.glm,direction="both")# Finds the best combination
rice.glmBest = glm(as.factor(Class)~Area+
                                    Perimeter+
                                    Major_Axis_Length+
                                    Minor_Axis_Length+
                                    Convex_Area,
                                    family="binomial",data=rice_data)
summary(rice.glmBest)
# Area and Convex_Area have the most significance at *** ( 0 < x < 0.001 )
# Perimeter has the second most significance at ** ( 0.001 < x < 0.01 )
# Minor_Axis_Length has the least significance at * ( 0.01 < x < 0.05 )
# Major_Axis_Length has no significance, but p-val is pretty low  at . ( 0.05 < 0.1 )
# AIC is 1368.7

rice.glmBestSub = glm(as.factor(Class)~Area+
                                       Perimeter+
                                       Minor_Axis_Length+
                                       Convex_Area,
                                       family="binomial",data=rice_data)
summary(rice.glmBestSub)
# AIC is 1369.9

# Meaning the best variable for deciding if a rice species is Cammeo or Osmancik
#-------------------------------------------------------------------------------

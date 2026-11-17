#nolint start
#       Basic PCA with prcomp and visualation with ggplot -- from a very quick glance 
# -- The first component is sufficent to explain 65% of the variance in the plot, the
# second component can explain 21%, and the third component pratically explains 13$; 
# no need for the other components. The Cam rice from the looks of it tend to be 
# smaller in all aspects as explained by the PCA; whilst the other is the complete 
# opposite pratical wise.

#       Oh also, Mohammed, you can tweak and play around this code or add to
# it; the code compile very quick on my end -- about a 1-2 seconds. Additionally,
# I would prefer you to comment prior to commetn to keep me updated, and as I stated
# to the team, to create your own branch and maintain said branch -- keep up-to-date
# with the changes in the main branch -- and only merge your branch into the main one
# after a session of coding. For posterity's sake -- Halogram from Laterns 2026.

# ----- Elvis, 2026/11/7

library(ggplot2)
setwd(this.path::here())

Rice_Dataset <- read.csv("rice_data.csv")
Rice_Dataset$Class <- as.factor(Rice_Dataset$Class)

Rice_Dataset.PC <- prcomp(Rice_Dataset[, -8], scale. = TRUE)
Rice_Dataset.PC.Data <- data.frame(Rice_Dataset.PC$x, Class = Rice_Dataset$Class)

Rice_Dataset.PC.Plot <- ggplot(Rice_Dataset.PC.Data, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

print(Rice_Dataset.PC)
print(summary(Rice_Dataset.PC)) # Does print really need to be there? 11/15/26 Mohammed
print(Rice_Dataset.PC.Plot)


# Hey Elvis, I made a PCA graph of variables to contribute to our model. 
# I thought your PCA plot looked great in that it classifies the 2 categories
# All the remains is just specifying our axes and thought this could be stated in the paper.
# It seems to me both categories show PC1 corresponds to Area, Convex_Area, Perimeter, Major_Axis_Length, and Eccentricity whereas PC2 corresponds to Extent and Minor_Axis_Length.
# Not sure if this means anything and am having difficulty naming PC1 and PC2; IIRC Dr. Poliak mentioned naming was according to our discretion if you remember the Baseball example.

# ----- Mohammed, 2026/11/15

# Quick edit here, I just added the factorextra library into this little section as the code as it's didn't
# worked properly without it. Also, I implemented the print funciton onto the plot funcitons as my setup
# doesn't work like Rstudio with its run button. Also, I saw the graphs you mentioned; up to PC2 should be
# fine stopping as its eigenvalue is above one -- the kaiser criterion -- and it explained 8684% of the 
# dataset. Also, we can have just a single #nolint zone in this file anyways -- save on typing the zone.
# Also I added the PCLogR onto it aswell.

# ----- Elvis, 2026/11/17

library(FactoMineR)
library(factoextra)

Rice.PCA = PCA(Rice_Dataset[,-8], graph = FALSE)
print(plot(Rice.PCA, choix = "var"))
print(screeplot(Rice_Dataset.PC, type = "lines"))  # Help me find/determine the elbow! 11/15/26 Mohammed

Rice.PCA.Scores <- as.data.frame(Rice.PCA$ind$coord)
Number_Comps <- 2
Rice.PCA.Data <- data.frame(Class = Rice_Dataset$Class, Rice.PCA.Scores[, 1:Number_Comps, drop = FALSE])
Rice.PCA.glm <- glm(Class ~ ., data = Rice.PCA.Data, family = binomial)
print(summary(Rice.PCA.glm))

#nolint end
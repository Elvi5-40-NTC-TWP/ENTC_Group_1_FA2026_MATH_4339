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
print(summary(Rice_Dataset.PC))
print(Rice_Dataset.PC.Plot)
#nolint end
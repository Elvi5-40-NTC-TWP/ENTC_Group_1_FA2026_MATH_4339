library(ggplot2)
setwd(this.path::here())
rice_data<-read.csv("rice_data.csv")
GGally::ggpairs(rice_data[,1:7])

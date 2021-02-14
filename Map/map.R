# source data & libraries ----

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)
library(readxl)
library(tibble) #to add columns
library(googleVis)

# Read Files----
sales_data <- data.frame(read.csv("E:/Programming/RStudio/Map/gdp.csv", header=T))

G2 <- gvisGeoChart(sales_data, locationvar='Country.Name',
                   colorvar='FY2019',options=list(width=1200, height=800,region="world"))

plot(G2)



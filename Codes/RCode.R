library(tidyverse)    
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

#MAC AW
setwd("/Users/andywilson1/Documents/GitHub/Olivia-DIA")

#get the list of states from us_state_polygons.json
states <- geojsonio::geojson_read("us_state_polygons.json", what = "sp")
counties<- geojsonio::geojson_read("us_county_polygons.json", what = "sp")



# Maps of the US (State level)

# Maps of Washington state (County level)

Washington <- subset(counties, STATE == "53")

## HPV vaccination coverage in 2018 
WV2018<- read.csv(file = "HPV Data/HPV vaccine/Washington_2018_analysis.csv", na.strings = c("", "NA"))

WV2018 <- WV2018 %>%
  rename(NAME = Geography)

mydf<- sp::merge(Washington, WV2018 , by="NAME", all=T)
plot(mydf, col="lightblue", lwd=2, main = "Washington State (US)
     Counties")

## Income data 2017
library(readxl)


# Clustering and polygons and interactivity 

# Hypothesize benefits of Kriging and photo-negative of vaccine and HPV status

  ## Ordinary Kriging 
 ### Concept and method 

library(gstat)
library(sp) 
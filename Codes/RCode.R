library(tidyverse)    
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(gstat)
library(sp) 
#library(maptools)

#MAC AW
#setwd("/Users/andywilson1/Documents/GitHub/Olivia-DIA")
#PC AW
setwd("C:/Users/wilso/Documents/GitHub/Olivia-DIA")

## Centralizing in Washington state (square boundaries) 
## Note: it is possible to create grids with irregular boundaries, but this is close enough for Wash. state

lat_min = 45.5
lat_max = 49
lon_max = -116.85 
lon_min = -124.75

## Import prevalence data 


prev <- read.csv(file = "External data/ZIP3_XY_HPV_SCREEN_2019_SLIM.csv", na.strings = c("", "NA"))
Wash_prev <- prev %>%
  filter(Longitude_avg >=  -124.46) %>%
  filter(Longitude_avg <=  -116.55) %>%
  filter(Latitude_avg  >=  45.33) %>%
  filter(Latitude_avg  <=  49)

#get the list of states from us_state_polygons.json
states <- geojsonio::geojson_read("us_state_polygons.json", what = "sp")
counties<- geojsonio::geojson_read("us_county_polygons.json", what = "sp")



# Maps of the US (State level)

# Maps of Washington state (County level)

Washington <- subset(counties, STATE == "53")



## HPV vaccination coverage in 2018 
WV2018<- read.csv(file = "HPV Data/HPV vaccine/Washington_2018_analysis.csv", na.strings = c("", "NA"), fileEncoding="UTF-8-BOM")

WV2018 <- WV2018 %>%
  rename(NAME = Geography)


mydf<- sp::merge(Washington, WV2018 , by="NAME", all=T)


plot(mydf, col="lightblue", lwd=2, main = "Washington State (US)
     grid for Kriging")
abline(h=c(lat_max, lat_min), lty=2, lwd=3, col="blue")
abline(v=c(lon_max, lon_min), lty=2, lwd=3, col="blue")

Percent_uninsured_from_census<- read.csv(file = "Percent_uninsured_from_census.csv", na.strings = c("", "NA"))
head(Percent_uninsured_from_census)
Percent_uninsured <- Percent_uninsured_from_census %>%
  filter(state=="53") 

Percent_uninsured$NAME <- gsub(" County, WA" , "", Percent_uninsured$NAME)
Percent_uninsured$PCTUI_PTnum <- as.numeric(as.character(Percent_uninsured$PCTUI_PT))
mydf2 <- sp::merge(Washington, Percent_uninsured , by ="NAME",  all=T)

## Income data 2017
library(readxl)

ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
                    "AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
head(ZipCodes)
ZipCodes <- ZipCodes %>%
  filter(AdminName1=="Washington")

WashIRS <- read_excel("IRS Income Data/DIA Submission Income Data 2017.xlsx")
WashIRS <- WashIRS %>% 
  filter(STATE=="WA") %>% 
  rename(zip = 'ZIP CODE')

table(WashIRS$STATE)

head(ZipCodes)
head(WashIRS)

myWashIRS <- merge(ZipCodes, WashIRS, by="zip", all=T)
  #Rename Adjusted income:
myWashIRS$AdjInc <- myWashIRS$`AVG ADJUSTED GROSS INCOME (IN THOUSANDS OF DOLLARS)`
summary(myWashIRS$AdjInc)

# Clustering and polygons and interactivity 

# Hypothesize benefits of Kriging and photo-negative of vaccine and HPV status

  ## Ordinary Kriging 
 ### Concept and method 








################
### Overlay maps
################

#Universal Washington base map:
m <- leaflet(mydf, options = leafletOptions(dragging=TRUE, 
                                            minZoom=6, 
                                            maxZoom=11))%>%
  setView(-120.74, 47.75, 7) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


#Palettes:
pal  <- colorNumeric(palette = "RdYlBu", domain =c(0:45))
pal2 <- colorNumeric(palette = "RdYlBu", domain =c(0:140), reverse = TRUE)
pal3 <- colorNumeric(palette = "RdYlBu", domain =c(0:15), reverse = TRUE)



MM <- m %>% addPolygons(data = mydf, weight=1, fillOpacity = 0.75,
                        color = pal(mydf$All),
                        label = mydf$NAME,
                        popup = paste0("Percent change: ", 
                                       mydf$All, "%"),
                        labelOptions = labelOptions(textOnly = FALSE),
                        group = "Factor") %>%
  addCircleMarkers(lng = myWashIRS$longitude,
                   lat = myWashIRS$latitude,
                   label = myWashIRS$PlaceName,
                   popup = paste("<b>", myWashIRS$PlaceName,"</b>",
                                 "<br/>",
                                 " Address = ", myWashIRS$Address,
                                 "<br/>",
                                 "ID = ", myWashIRS$PlaceName),
                   radius = myWashIRS$AdjInc,
                   color = pal2(myWashIRS$AdjInc), 
                   group = "Sites",
                   clusterOptions = markerClusterOptions())%>% 
  addLayersControl(overlayGroups = c("Factor", "Sites"))

MMM <- MM %>% addResetMapButton() %>%
  addLegend(title = "Percent case change (1 day)", 
            position = "bottomleft",
            pal=pal,
            values = c(0:45))
MMM
saveWidget(MMM, file = "C:\\Users\\wilso\\Documents\\GitHub\\COVID-19-strategic-feasibility-support\\Outputs\\New York example.html")

pal3 <- colorNumeric(palette = "RdYlBu", domain =c(0:15), reverse = TRUE)


MM <- m %>% addPolygons(data = mydf, weight=1, fillOpacity = 0.45,
                        color = pal(mydf$All),
                        label = mydf$NAME,
                        popup = paste0("Percent HPV vaccine UtD: ", 
                                       mydf$All, "%"),
                        labelOptions = labelOptions(textOnly = FALSE),
                        group = "County HPV vaccination") %>%
  addPolygons(data = mydf2, weight=1, fillOpacity = 0.45,
              color = pal3(Percent_uninsured$PCTUI_PTnum),
              label = mydf2$NAME,
              popup = paste0("Percent Uninsured: ", 
                             Percent_uninsured$PCTUI_PTnum, "%"),
              labelOptions = labelOptions(textOnly = FALSE),
              group = "County Uninsured") %>%
  addCircleMarkers(lng = myWashIRS$longitude,
                   lat = myWashIRS$latitude,
                   label = myWashIRS$PlaceName,
                   popup = paste("<b>", myWashIRS$PlaceName,"</b>",
                                 "<br/>",
                                 " AVG ADJUSTED GROSS INCOME (IN THOUSANDS OF DOLLARS)     = $", round(myWashIRS$AdjInc,2),
                                 "<br/>",
                                 "zip = ", myWashIRS$zip),
                   radius = myWashIRS$AdjInc,
                   stroke = FALSE, fillOpacity = 0.5,
                   color = pal2(myWashIRS$AdjInc), 
                   group = "IRS tax data by zip",
                   clusterOptions = markerClusterOptions())%>% 
  
  addLayersControl(overlayGroups = c("County HPV vaccination", "IRS tax data by zip", "County Uninsured"))

MMM <- MM %>% addResetMapButton() %>%
  addLegend(title = "Vaccination rates", 
            position = "bottomleft",
            pal=pal,
            values = c(0:45)) %>%
  addLegend(title = "Avg annual income", 
            position = "bottomright",
            pal=pal2,
            values = c(0:150))%>%
  addLegend(title = "Percent uninsured", 
            position = "topleft",
            pal=pal3,
            values = c(0:15))
MMM

saveWidget(MMM, file = "/Users/andywilson1/Documents/GitHub/Olivia-DIA/Output/Prototype Washington map v0.1.html")



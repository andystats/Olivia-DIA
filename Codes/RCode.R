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

Percent_uninsured_from_census<- read.csv(file = "Percent_uninsured_from_census.csv", na.strings = c("", "NA"))
head(Percent_uninsured_from_census)
Percent_uninsured <- Percent_uninsured_from_census %>%
  filter(state=="53")

mydf2 <- merge(x = Washington, y = Percent_uninsured , by ="GEOID_ID",  all=T)
class(Washington$GEO_ID)
class(Percent_uninsured$GEOID_ID)
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
head(myWashIRS)
# Clustering and polygons and interactivity 

# Hypothesize benefits of Kriging and photo-negative of vaccine and HPV status

  ## Ordinary Kriging 
 ### Concept and method 

library(gstat)
library(sp) 





### Overlay maps

m <- leaflet(mydf, options = leafletOptions(dragging=TRUE, 
                                            minZoom=6, 
                                            maxZoom=11))%>%
  setView(-120.74, 47.75, 7) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


pal  <- colorNumeric(palette = "RdYlBu", domain =c(0:45))
pal2 <- colorNumeric(palette = "RdYlBu", domain =c(0:140), reverse = TRUE)

myWashIRS$AdjInc <- myWashIRS$`AVG ADJUSTED GROSS INCOME (IN THOUSANDS OF DOLLARS)`
summary(myWashIRS$AdjInc)
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



MM <- m %>% addPolygons(data = mydf, weight=1, fillOpacity = 0.75,
                        color = pal(mydf$All),
                        label = mydf$NAME,
                        popup = paste0("Percent HPV vaccine UtD: ", 
                                       mydf$All, "%"),
                        labelOptions = labelOptions(textOnly = FALSE),
                        group = "County HPV vaccination") %>%
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
  
  addLayersControl(overlayGroups = c("County HPV vaccination", "IRS tax data by zip"))

MMM <- MM %>% addResetMapButton() %>%
  addLegend(title = "Vaccination rates", 
            position = "bottomleft",
            pal=pal,
            values = c(0:45)) %>%
  addLegend(title = "Avg annual income", 
            position = "bottomright",
            pal=pal2,
            values = c(0:150))
MMM

saveWidget(MMM, file = "/Users/andywilson1/Documents/GitHub/Olivia-DIA/Output/Prototype Washington map v0.1.html")



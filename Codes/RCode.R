library(tidyverse)    
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(gstat)
library(sp) 
#library(maptools)

#MAC AW
setwd("/Users/andywilson1/Documents/GitHub/Olivia-DIA")
#PC AW
#setwd("C:/Users/wilso/Documents/GitHub/Olivia-DIA")

## Centralizing in Washington state (square boundaries) 
## Note: it is possible to create grids with irregular boundaries, but this is close enough for Wash. state


lat_min = 45.54354
lat_max = 49.00249
lon_max = -116.91599 
lon_min = -124.73317

## Import positivity data 

prev <- read.csv(file = "External data/ZIP3_XY_HPV_SCREEN_2019_SLIM.csv", na.strings = c("", "NA"))

## Restrict to Washington state
Wash_prev <- prev %>%
  filter(Longitude_avg >=  lon_min) %>%
  filter(Longitude_avg <=  lon_max) %>%
  filter(Latitude_avg  >=  lat_min) %>%
  filter(Latitude_avg  <=  lat_max) %>%
  filter(zip3 != "971")

summary(Wash_prev)

#get the list of states from us_state_polygons.json
states <- geojsonio::geojson_read("us_state_polygons.json", what = "sp")
counties<- geojsonio::geojson_read("us_county_polygons.json", what = "sp")



# Maps of the US (State level)
US2017<- read.csv(file = "HPV Data/HPV vaccine/HPV_vax_by_state.csv", na.strings = c("", "NA"), fileEncoding="UTF-8-BOM") %>%
  drop_na(HPV)

mydfz<- sp::merge(states, US2017 , by="NAME", all=T)




# Maps of Washington state (County level)

Washington <- subset(counties, STATE == "53")



## HPV vaccination coverage in 2018 
WV2018<- read.csv(file = "HPV Data/HPV vaccine/Washington_2018_analysis.csv", na.strings = c("", "NA"), fileEncoding="UTF-8-BOM")

WV2018 <- WV2018 %>%
  rename(NAME = Geography)


mydf<- sp::merge(Washington, WV2018 , by="NAME", all=T)

pdf(file = "Output/Washington bounding box for Kriging.pdf")
plot(mydf, col="lightblue", lwd=2, main = "Washington State (US)
     grid for Kriging")
abline(h=c(lat_max, lat_min), lty=2, lwd=3, col="blue")
abline(v=c(lon_max, lon_min), lty=2, lwd=3, col="blue")
dev.off()

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
myWashIRS <- myWashIRS %>%
  drop_na(AdjInc)
summary(myWashIRS$AdjInc)

# Clustering and polygons and interactivity 

# Hypothesize benefits of Kriging and photo-negative of vaccine and HPV status

  ## Ordinary Kriging 
 ### Concept and method 


#Kriging for vaccination rates

#fitting variogram 

pct.vgm <- variogram(PCTUI_PTnum ~1, mydf2)
pct.fit <- fit.variogram(pct.vgm, model=vgm(1, "Sph"))

pdf(file = "Output/Variogram vax rates.pdf", width=10)
plot(pct.vgm, pct.fit)
dev.off()

## Generate grid to perform generative Kriging 

grd <- makegrid(mydf2, n=100000) 
#colnames(grd) <- c('x', 'y')

grd_pts <- SpatialPoints(coords = grd, 
                         proj4string = CRS(proj4string(mydf2)))

grd_pts_in <- grd_pts[mydf2, ]

gdf <- as.data.frame(grd_pts_in)
head(gdf)
library(ggthemes)

pdf(file = "Output/Washington grid for Kriging2.pdf", width=10)
ggplot(gdf) +
  geom_point(aes(x=x1, y=x2))+
  theme_few()
dev.off()


#Computation

pct.kriged <-krige(PCTUI_PTnum ~1, mydf2, grd_pts_in, pct.fit )

pct.kriged.data <- as.data.frame(pct.kriged) 
head(pct.kriged.data)

pdf(file = "Output/Predicted vaccination rate.pdf", width=10)
pct.kriged.data %>%
ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient("Predicted", low="red", high ="blue") +
  theme_void()
dev.off()  

pdf(file = "Output/Variance vaccination rate.pdf", width=10)
pct.kriged.data %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill=var1.var)) + coord_equal() +
  scale_fill_gradient("Variance", low="blue", high ="red") +
  theme_void()
dev.off()  

write.csv(pct.kriged.data, "Kriged data.csv")

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
pal  <- colorNumeric(palette = "Reds", domain =c(0:45))
pal2 <- colorNumeric(palette = "Blues", domain =c(0:140), reverse = TRUE)
pal3 <- colorNumeric(palette = "Blues", domain =c(0:15), reverse = TRUE)
pal4 <- colorNumeric(palette = "Blues", domain =c(1:50))


MM <- m %>% addPolygons(data = mydf, weight=1, fillOpacity = 0.25,
                        color = pal(mydf$All),
                        label = mydf$NAME,
                        popup = paste0("Percent HPV vaccine UtD: ", 
                                       mydf$All, "%"),
                        labelOptions = labelOptions(textOnly = FALSE),
                        group = "County HPV vaccination") %>%
  addPolygons(data = mydf2, weight=1, fillOpacity = 0.25,
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
                   radius = myWashIRS$AdjInc/10,
                   stroke = TRUE, fillOpacity = 0.8,
                   color = pal2(myWashIRS$AdjInc), 
                   group = "IRS tax data by zip",
                   clusterOptions = markerClusterOptions()) %>% 
  addCircleMarkers(lng = Wash_prev$Longitude_avg,
                   lat = Wash_prev$Latitude_avg,
                   label = Wash_prev$zip3,
                   popup = paste("<b>", Wash_prev$zip3,"</b>",
                                 "<br/>",
                                 " Positivity rate: ", round(Wash_prev$positivity,2), "%", 
                                 "<br/>",
                                 "n screened = ", Wash_prev$n_screened,
                                 "<br/>",
                                 "n positive = ", Wash_prev$n_positive
                                 ),
                   radius = log(Wash_prev$n_positive),
                   stroke = TRUE, fillOpacity = 0.8,
                   color = pal4(Wash_prev$positivity), 
                   group = "Positivity by 3-digit zip") %>%
  addLayersControl(overlayGroups = c("County HPV vaccination", "IRS tax data by zip", "County Uninsured", "Positivity by 3-digit zip"))

MMM <- MM %>% addResetMapButton() %>%
  addLegend(title = "Vaccination rates", 
            position = "bottomleft",
            pal=pal,
            values = c(0:45)) %>%
  addLegend(title = "Avg annual income (in $1,000s)", 
            position = "bottomright",
            pal=pal2,
            values = c(0:150))%>%
  addLegend(title = "Percent uninsured", 
            position = "topleft",
            pal=pal3,
            values = c(0:15)) %>%
  addLegend(title = "Positivity rate", 
            position = "topright",
            pal=pal4,
            values = c(0:50))
MMM

#MAC AW
#saveWidget(MMM, file = "/Users/andywilson1/Documents/GitHub/Olivia-DIA/Output/Prototype Washington map v0.1.html")

#PC AW
saveWidget(MMM, file = "C:/Users/wilso/Documents/GitHub/Olivia-DIA/Output/Prototype Washington map v1.1.html")

# MM <- m %>% addPolygons(data = mydf, weight=1, fillOpacity = 0.75,
#                         color = pal(mydf$All),
#                         label = mydf$NAME,
#                         popup = paste0("Percent change: ", 
#                                        mydf$All, "%"),
#                         labelOptions = labelOptions(textOnly = FALSE),
#                         group = "Factor") %>%
#   addCircleMarkers(lng = myWashIRS$longitude,
#                    lat = myWashIRS$latitude,
#                    label = myWashIRS$PlaceName,
#                    popup = paste("<b>", myWashIRS$PlaceName,"</b>",
#                                  "<br/>",
#                                  " Address = ", myWashIRS$Address,
#                                  "<br/>",
#                                  "ID = ", myWashIRS$PlaceName),
#                    radius = myWashIRS$AdjInc,
#                    color = pal2(myWashIRS$AdjInc), 
#                    group = "Sites",
#                    clusterOptions = markerClusterOptions())%>% 
#   addLayersControl(overlayGroups = c("Factor", "Sites"))
# 
# MMM <- MM %>% addResetMapButton() %>%
#   addLegend(title = "Percent case change (1 day)", 
#             position = "bottomleft",
#             pal=pal,
#             values = c(0:45))
# MMM
# saveWidget(MMM, file = "C:\\Users\\wilso\\Documents\\GitHub\\COVID-19-strategic-feasibility-support\\Outputs\\New York example.html")


## Bonus: State-level maps 
head(prev)
qpal <- colorQuantile("Reds", prev$positivity, n = 5)
S <- leaflet(states, options = leafletOptions(dragging=TRUE, 
                                            minZoom=4, 
                                            maxZoom=11))%>%
  setView(-98.35, 39.5, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

SS <- S %>% addPolygons(data = states, weight=.5, label = states$NAME) %>%
  addCircleMarkers(lng = prev$Longitude_avg,
                   lat = prev$Latitude_avg,
                   label = prev$positivity,
                   popup = paste("<b>","Positivity =", prev$positivity,"</b>"),
                   radius = log(prev$n_screened),
                   stroke = TRUE, fillOpacity = 0.8,
                   color = qpal(prev$positivity)
                  ) 
SSS <- SS %>% addResetMapButton() %>%
  addLegend(title = "Positivity quantile (relative standing)", 
              position = "bottomleft",
              pal=qpal,
             values = prev$positivity)
SSS
saveWidget(SSS, file = "C:/Users/wilso/Documents/GitHub/Olivia-DIA/Output/Prototype US positivity quantiles map v1.1.html")

## States
z <- leaflet(mydfz, options = leafletOptions(dragging=TRUE, 
                                              minZoom=5, 
                                              maxZoom=11))%>%
  setView(-98.35, 39.5, 5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


#Palettes:
palz1  <- colorNumeric(palette = "Blues", domain =c(45:95))
palz2  <- colorNumeric(palette = "Reds", domain =c(25:80), reverse = TRUE)
summary(mydfz@data)

ZZ <- z %>% addPolygons(data = mydfz, weight=1, fillOpacity = 0.25,
                        color = palz1(mydfz$HPV),
                        label = mydfz$NAME,
                        popup = paste0("Percent HPV vaccine Initiated: ", 
                                       mydfz$HPV2, "%"),
                        labelOptions = labelOptions(textOnly = FALSE),
                        group = "HPV 1+") %>%
  addPolygons(data = mydfz, weight=1, fillOpacity = 0.25,
              color = palz2(mydfz$HPV.UTD),
              label = mydfz$NAME,
              popup = paste0("Percent HPV vaccine UtD ", 
                             mydfz$HPV.UTD2 , "%"),
              labelOptions = labelOptions(textOnly = FALSE),
              group = "HPV Vaccine UtD") %>%
  addLayersControl(overlayGroups = c("HPV 1+", "HPV Vaccine UtD"))

ZZZ <- ZZ %>% addResetMapButton() %>%
  addLegend(title = "Vaccination Initiated", 
            position = "bottomleft",
            pal=palz1,
            values = c(45:95)) %>%
  addLegend(title = "Vaccination UtD", 
            position = "bottomright",
            pal=palz2,
            values = c(25:80)) 
ZZZ

saveWidget(ZZZ, file = "/Users/andywilson1/Documents/GitHub/Olivia-DIA/Output/NIS Teen Vax map.html")


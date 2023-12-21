library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
require(dplyr)
require(countrycode)

worldplot <- function(simdata, div = 1, ColName, CountryName, CountryNameType, rangeVal,
                      longitude = c(-180, 180) ,latitude = c(-90, 90),
                      title = "", legendTitle = as.character(ColName)) {
  
  world <- ne_countries(scale = "medium", continent = NULL, returnclass = "sf")
  
  map_df0<- world %>%
    select(name, iso_a2, iso_a3, geometry)
  
  simdata$MapFiller <- simdata[, which(colnames(simdata) == ColName)]

  if (CountryNameType == "isoa2") {
    simdata$iso_a2 <- simdata[, which(colnames(simdata) == CountryName)]
  } else if (CountryNameType == "name") {
    simdata$iso_a2 <- countrycode(sourcevar = simdata[, which(colnames(simdata) == CountryName)],
                                  origin = "country.name", destination = "iso2c")
  } else if (CountryNameType == "isoa3") {
    simdata$iso_a2 <- countrycode(sourcevar = simdata[, which(colnames(simdata) == CountryName)],
                                  origin = "iso3c", destination = "iso2c")
  } else {
    simdata$iso_a2 <- NULL
  }

  
  map_df <- left_join(map_df0, simdata, by = "iso_a2")
  
  wplot <- ggplot(data= map_df) +
    geom_sf(color= 'black', aes(fill= MapFiller)) +
    theme(legend.key.size = unit(1, 'lines'),
          legend.text = element_text(size= 8),
          legend.title = element_text(size= 8),
          plot.title = element_text(size=8),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = 'grey95'))+
    labs(fill= legendTitle)+
    scale_fill_viridis_c(option='viridis', na.value = 'grey80',direction=1,begin=0.3, limits= rangeVal)+
    coord_sf(xlim= longitude, ylim= latitude, expand= FALSE, label_axes = 'SW') +
    xlab('') + ylab('')+
    ggtitle(title) 
  
  wplot
}


#### Test 3

set.seed(4879)

data4 <- as.data.frame(world) %>% 
  select(name, iso_a2) %>%
  mutate(dummykeep = rbinom(nrow(world),1,0.4),
         narandom = rbinom(nrow(world),1, 0.1), 
         value0 = round(runif(nrow(world), min = 0, max = 100),2),
         IntVal = ifelse(narandom == 1, NA, value0)
  )  %>%
  filter(dummykeep == 1) %>%              
  filter(!is.na(iso_a2)) %>%     
  select(!c(dummykeep, narandom, value0)) %>%
  rename(countrycode = iso_a2)

worldplot(simdata = data4, 
          ColName = "IntVal",
          CountryName = "countrycode",
          CountryNameType = "isoa2",
          rangeVal = c(0,100), 
          latitude = c(0,45), longitude = c(-10,50))


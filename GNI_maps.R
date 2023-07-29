# preparazione dataset ####

library(readxl)
df<- as.data.frame(read_excel('C:\\files\\HDI maps\\GNI_in_2014_and_2019_LA.xlsx',
                              col_names= FALSE, range = 'A4:E27'))


dimnames(df)[[2]]<- c( "Country",  "code", "income_2014",
                        "income_2019", "category")

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

europe<- ne_countries(scale= 'medium',continent = 'Europe' ,returnclass = 'sf')
asia<- ne_countries(scale='medium', continent='Asia', returnclass = 'sf')
africa<- ne_countries(scale= 'medium', continent= 'Africa', returnclass = 'sf')
eurasiafrica<- rbind(europe, asia, africa)


map_europe_comp<- as.data.frame(cbind(eurasiafrica$name,eurasiafrica$iso_a2))
dimnames(map_europe_comp)[[2]]<- c('name', 'iso_a2')

df$control<- rep(0,dim(df)[1])

map_europe_comp$income_2019<- rep(NA,dim(eurasiafrica)[1])
map_europe_comp$category_income<- rep(NA,dim(eurasiafrica)[1])


for (z in c(1:dim(eurasiafrica)[1]))
{
  for (k in c(1: dim(df)[1]))
  {
    if (!is.na(map_europe_comp$iso_a2[z]) & df$code[k]== map_europe_comp$iso_a2[z])
    {
      map_europe_comp$income_2019[z]<- df$income_2019[k]
      df$control[k]<- 1
    }}}


map_europe_comp$category_income[map_europe_comp$income_2019<34000]<- '1_Low'
map_europe_comp$category_income[map_europe_comp$income_2019>34000 &
                               map_europe_comp$income_2019<51000]<- '2_Medium'
map_europe_comp$category_income[map_europe_comp$income_2019>51000]<- '3_High'


HDI_nations<- subset(eurasiafrica, 
                     (!is.na(map_europe_comp$income_2019) & eurasiafrica$sov_a3!= 'NOR'
                      & eurasiafrica$iso_a2!= 'GB'
                      & eurasiafrica$iso_a2!= 'DK'
                      & eurasiafrica$iso_a2!= 'GR'
                      & eurasiafrica$iso_a2!= 'FR'
                      & eurasiafrica$iso_a2!= 'RU'
                      & eurasiafrica$iso_a2!= 'IL'))

eurasiafrica_points<- st_centroid(HDI_nations)
eurasiafrica_points<- cbind(HDI_nations, st_coordinates(st_centroid(HDI_nations$geometry)))


### maps blue, yellow: income ####

setwd('C:\\files\\HDI maps\\maps_income_GNI')

tiff(filename='map_cont_2023.jpg', width = 1500, height=1620)

ggplot(data= eurasiafrica) +
  geom_sf(color= 'black', aes(fill=map_europe_comp$income_2019)) +
  theme(legend.key.size = unit(3.5, 'lines'),
        legend.text = element_text(size= 20),
        legend.title = element_text(size= 20),
        plot.title = element_text(size=30))+
  labs(fill='Income')+
  scale_fill_viridis_c(option='viridis', na.value = 'grey80',direction=1,begin=0.3, limits= c(10000,71000))+
  coord_sf(xlim= c(-25, 60), ylim= c(25, 70), expand= FALSE, label_axes = 'SW')+
  xlab('') + ylab('')+
  ggtitle('Income, continuous scale')+
  geom_text(data= eurasiafrica_points, aes(x=X, y=Y,label= iso_a2),size=5, color= 'black', fontface= 'bold')+
  annotate(geom= 'text',x=40, y=55, label= 'RU', color= 'black', size=5, fontface= 'bold')+
  annotate(geom='text', x=-1, y=52.5, label= 'GB', color= 'black', size=5,fontface= 'bold')+
  annotate(geom= 'text', x= 9, y= 56, label= 'DK', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 21.5, y= 39.5, label= 'GR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 3.5, y= 47, label= 'FR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 35, y= 31, label= 'IL', color= 'black', size= 5, fontface= 'bold')

dev.off()

tiff(filename='map_cat2023.jpg', width = 1500, height=1620)

ggplot(data= eurasiafrica) +
  geom_sf(color= 'black', aes(fill= map_europe_comp$category_income)) +
  theme(legend.key.size = unit(3.5, 'lines'),
        legend.text = element_text(size= 20),
        legend.title = element_text(size= 20),
        plot.title = element_text(size=30))+
  labs(fill= 'Income category')+
  scale_fill_viridis_d(option = 'viridis',begin= 0.3, na.value = 'grey80', direction= 1,
                       labels= c('Low', 'Medium', 'High', 'NA'), na.translate=F)+
  coord_sf(xlim= c(-25, 60), ylim= c(25, 70), expand= FALSE,label_axes = 'SW')+
  xlab('')+ ylab('')+
  ggtitle('Income categories')+
  geom_text(data= eurasiafrica_points, aes(x=X, y=Y,label= iso_a2),size=5, color= 'black', fontface= 'bold')+
  annotate(geom= 'text',x=40, y=55, label= 'RU', color= 'black', size=5, fontface= 'bold')+
  annotate(geom='text', x=-1, y=52.5, label= 'GB', color= 'black', size=5,fontface= 'bold')+
  annotate(geom= 'text', x= 9, y= 56, label= 'DK', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 21.5, y= 39.5, label= 'GR', color= 'black', size=5, fontface= 'bold')+
  annotate(geom= 'text',x= 3.5, y= 47, label= 'FR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 35, y= 31, label= 'IL', color= 'black', size= 5, fontface= 'bold')

dev.off()



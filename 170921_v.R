library(ggplot2)

dane <- data.frame(mylevels=c(1,2,5,9), myvalues=c(2, 5, 3, 4))
head(dane)

ggplot(dane, aes(x=factor(mylevels), y=myvalues)) + geom_line(group = 1) + geom_point(size=2)

data(economics)
data(presidential)

ggplot(economics, aes(date, unemploy)) + geom_line()

presidential = subset(presidential, start > economics$date[1])


str(presidential)

ggplot(economics) + geom_rect(aes(xmin = start,xmax = end, fill = party), ymin = -Inf, ymax = Inf, data = presidential) + geom_line(aes(date, unemploy), data = economics)


#############

library(datasets)
data("airquality")
plot(airquality$Ozone, type = 'b')
head(airquality)

aq_trim <- airquality[which(airquality$Month == 7 |
                              airquality$Month == 8 |
                              airquality$Month == 9), ]

aq_trim$Month <- factor(aq_trim$Month,labels = c("July", "August", "September"))
geom_line
ggplot(aq_trim, aes(x = Day, y = Ozone, size = Wind, fill = Temp)) +
  geom_point(shape = 21) +   ggtitle("Air Quality in New York by Day") +
  labs(x = "Day of the month", y = "Ozone (ppb)") +
  scale_x_continuous(breaks = seq(1, 31, 5))

############################

festival.data <- read.table(file = 'DownloadFestival.dat', sep = '\t', header = T)
head(festival.data)

Day1Histogram <- ggplot(data = festival.data, aes( x= day1))
Day1Histogram + geom_histogram()

Day1Histogram + geom_histogram(color = 'royalblue1', fill = 'royalblue2')

Day1Histogram + geom_histogram(color = 'royalblue1', fill = 'royalblue2', 
                               binwidth  = 0.1)

Day1Histogram + geom_histogram(binwidth = 0.2, aes( y = ..density..), 
                               color= 'royalblue3', fill = 'yellow', bins = 35)


Day1Histogram +geom_histogram(binwidth = 0.1, aes(y=..density..), 
                              color="black", 
                              fill="lightblue") + geom_density(alpha=.2, fill="#FF6666")


######################
library(reshape2)

festival.data.stack <- melt(festival.data, id = c('ticknumb', 'gender'))

colnames(festival.data.stack)[3:4] <- c('day', 'score')

head(festival.data.stack)

Histogram.3day2 <- ggplot( data = festival.data.stack, aes(x = score, y = ..density..)) + 
  geom_histogram(binwidth = 0.4, color= 'black', fill = 'yellow') + 
  labs( x = 'Score', y = 'Density')

Histogram.3day2

Histogram.3day2 + facet_grid(~gender)

Histogram.3day2 + facet_grid(gender~day)



###################
Scatterplot <- ggplot(data = festival.data.stack, aes(x = gender, y = score, color = gender)) + 
  geom_point(position = 'jitter') + facet_grid(~day)

Scatterplot

Scatterplot + geom_boxplot(alpha = 0.5, color= 'black', fill = 'orange')
###################
library(maps)
load(file="storms.RData")
wm = map_data("world")

str(wm)

wm[1:100,]

str(substorms)


substorms = storms %>% filter(Season %in% 1999:2010) %>%
  filter(!is.na(Season)) %>%
  filter(Name!="NOT NAMED")
substorms$ID = as.factor(paste(substorms$Name, 
                               substorms$Season, sep = "."))
substorms$Name = as.factor(substorms$Name)

map1 = ggplot(substorms, 
              aes(x = Longitude, y = Latitude, group = ID)) + 
  geom_polygon(data = wm, 
               aes(x = long, y = lat, group = group), 
               fill = "gray25", colour = "gray10", size = 0.2) + 
  geom_path(data = substorms, 
            aes(group = ID, colour = Wind.WMO.),
            alpha = 0.5, size = 0.8) +
  xlim(-138, -20) + ylim(3, 55) + 
  labs(x = "", y = "", colour = "Wind \n(knots)")


map2 = ggplot(substorms, 
              aes(x = Longitude, y = Latitude, group = ID)) + 
  geom_polygon(data = wm, 
               aes(x = long, y = lat, group = group), 
               fill = "gray25", colour = "gray10", size = 0.2) + 
  geom_path(data = substorms, 
            aes(group = ID, colour = Wind.WMO.), size = 0.5) + 
  xlim(-138, -20) + ylim(3, 55) + 
  labs(x = "", y = "", colour = "Wind \n(knots)") + 
  facet_wrap(~Month)
map1 + facet_wrap(~Month)


#####################

par(bg="white")
set.seed(1)
a=seq(1:100) + 0.1*seq(1:100)*sample(c(1:10) , 100 , replace=T)
b=seq(1:100) + 0.2*seq(1:100)*sample(c(1:10) , 100 , replace=T)
size = 3 +(a/30) + rnorm(length(a))
d = (b/300) + rnorm(length(a),0, 0.1)
d[d<0] = 0
rdata<- data.frame(x = a, y = b, size = size, temp = d)
myplot <- ggplot(data = rdata, aes ( x = x, y = y)) + 
  geom_point(aes(x,y, colour = temp), size = size) + 
  scale_color_gradient2(midpoint = 0.5, low="#EF5500", 
                        mid="#FFFF77", high="blue")

###################3
library(maps)
library(mapdata)

par(mfrow = c(1,2))
map(database = "usa")
map(database = "county")

par(mfrow = c(1,1))
map(database = 'world', region = 'South Korea')
map('world2Hires', 'South Korea')

wm2 = ggplot2::map_data('world2Hires')
str(wm2)

krmap = wm2 %>% filter(region == 'South Korea')

head(krmap)

unique(krmap$subregion)
######################

data("us.cities")
head(us.cities)

map("state", "GEORGIA")
map.cities(us.cities, country = 'GA')

map('world', fill = TRUE, col = rainbow(30))

#####################################
data(unemp) # unemployed rate data
data(county.fips) # county fips data
head(unemp,3)
head(county.fips, 3)

unemp$colorBuckets <- as.numeric(cut(unemp$unemp, 
                                     c(0, 2, 4, 6, 8, 10, 100)))

colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]
colors = c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0","#DD1C77","#980043")
library(mapproj)
map("county", col = colors[colorsmatched], fill = TRUE,
    resolution = 0, lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1,
    lwd = 0.2,projection = "polyconic")
title("unemployment by county, 2009")


############################
library(dplyr)

ur <- wm %>% dplyr::select(region)%>%unique()
grep( "Korea", ur$region )


map("world", ur$region[c(125,185)], fill = F,
    col = c("blue"))

abline(h = 38)

#install.packages('ggmap') #위/경도 좌표 받기
#install.packages('mapplots') #add.pie 용
library(mapplots)
library(ggmap)

map('worldHires', 'South Korea')
seoul_loc = geocode('seoul')
busan_loc = geocode('Busan')
add.pie(z=1:2, labels = c('a','b'), x = seoul_loc$lon, y = seoul_loc$lat, radius = 0.5)
add.pie(z=4:3, labels = c('a','b'), x = busan_loc$lon, y = busan_loc$lat, radius = 0.5)

#install.packages('OpenStreetMap')
library(OpenStreetMap)
library(ggplot2)
map = OpenStreetMap::openmap(upperLeft = c(43, 119), lowerRight = c(33, 134),
                             type = 'bing')


nm = c("osm", "maptoolkit-topo", "stamen-toner", 
       "stamen-watercolor", "esri", "esri-topo", 
       "nps", "apple-iphoto", "skobbler")
par(mfrow=c(3,3))
for(i in 1:length(nm))
  {
  map <- OpenStreetMap::openmap(c(43.46, 119.94), c(33.22, 133.98), minNumTiles=3, type=nm[i])
  plot(map, xlab = paste(nm[i]))
  } ##error


par(mfrow=c(1,1))
map1 <- openmap(c(43.46,119.94),
                c(33.22,133.98))
plot(map1)
abline(h = 38, col = 'red') # 안댐
str(map1)
map1$tiles[[1]]$projection

library(sp)
map_p <- openproj(map1, projection = CRS("+proj=longlat"))
str(map_p)
plot(map_p)
abline(h = 38, col = 'blue')


map_p <- openproj(map1, projection = 
                    CRS("+proj=utm +zone=52N + datum=WGS84"))
plot(map_p)
abline(h = 38, col = 'blue') # 안댐

a  <-data.frame(lon =  seq(100,140,by = 0.1),
                lat =  38)
sp::coordinates(a) = ~ lon + lat
str(a)

sp::proj4string(a) = "+proj=longlat" #a를 projection longlat에 할당해준다 
#a@proj4string  = CRS("+proj=longlat")
str(a)

a_tf = spTransform(a,  CRS("+proj=utm +zone=52N + datum=WGS84")) # 좌표계를 가진 객체로 변환

str(a_tf) #coords에 좌표가 있다

plot(map_p)
points(a_tf@coords[,1], a_tf@coords[,2], type = 'l', col = 'blue') #동경, 북위 좌표 순?

#############pie chart on map
library(sp)
map = openmap(upperLeft = c(43, 119),lowerRight = c(33, 134))

seoul_loc = geocode('Seoul')
coordinates(seoul_loc) = ~lon + lat
proj4string(seoul_loc) = "+proj=longlat +datum=WGS84" 
seoul_loc_Tf = spTransform(seoul_loc,
                           CRS(as.character(map$tiles[[1]]$projection)))
plot(map)
add.pie(z=1:2,labels = c('a','b'),
        x = seoul_loc_Tf@coords[1],
        y = seoul_loc_Tf@coords[2], radius = 100000)
########
########ggmap####
library(ggmap)
data(crime)

violent_crimes = subset(crime,
                        offense != "auto theft" & 
                          offense != "theft" & 
                          offense != "burglary")
violent_crimes$offense <- factor(violent_crimes$offense,
                                 levels = c("robbery", "aggravated assault", "rape", "murder"))
violent_crimes = subset(violent_crimes,
                        -95.39681 <= lon & lon <= -95.34188 &
                          29.73631 <= lat & lat <=  29.78400)
HoustonMap = qmap("houston", zoom = 14,
                  color = "bw", legend = "topleft")
HoustonMap + geom_point(aes(x = lon, y = lat,
                            colour = offense, size = offense),
                        data = violent_crimes)


HoustonMap +
  geom_point(aes(x = lon, y = lat,
                 colour = offense, size = offense),
             data = violent_crimes) +
  geom_density2d(aes(x = lon, y = lat), size = 0.2 , bins = 4, 
                 data = violent_crimes) + stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 2, bins = 4, data = violent_crimes, geom = 'polygon')
 #1번은 지도 및 포인트 찍기, 2번은 등고선 긋기, 3번은 등고선 레벨 별 색칠


##########################practice#####Airport###################

load('r6/airport.Rdata')
head(airport_krjp)
head(link_krjp)


link_krjp[link_krjp$group == 2,]

map = ggmap(get_googlemap(center = c(lon=134, lat=36),
                          zoom = 5, maptype='roadmap', color='bw'))
map + geom_line(data=link_krjp,aes(x=lon,y=lat,group=group), 
                col='grey10',alpha=0.05) + 
  geom_point(data=airport_krjp[complete.cases(airport_krjp),],
             aes(x=lon,y=lat, size=Freq), colour='black',alpha=0.5) + 
  scale_size(range=c(0,15))

########데이터정제 및 시각화를 위한 library 호출#########
if (!require(sp)) install.packages('sp')
if (!require(gstat)) install.packages('gstat')
if (!require(automap)) install.packages('automap')
if (!require(rgdal)) install.packages('rgdal')
if (!require(e1071)) install.packages('e1071')
if (!require(dplyr)) install.packages('dplyr')
if (!require(lattice)) install.packages('lattice')
if (!require(viridis)) install.packages('viridis')
library(sp); library(gstat); library(automap);
library(rgdal); library(e1071); library(lattice);
library(ggplot2); library(raster);library(viridis)

seoul032823 <- read.csv ("r6/seoul032823.csv")
head(seoul032823)

skorea <- getData(name ="GADM", country= "KOR", level=2)
head(skorea,2)
str(skorea)
class(skorea)
head(skorea@polygons[[1]]@Polygons[[1]]@coords, 3)
skorea = fortify(skorea)
str(skorea)
class(skorea)
head(skorea,3)

ggplot() + geom_map(data= skorea, map= skorea,
                    aes(map_id=id,group=group),fill=NA, colour="black") +
  geom_point(data=seoul032823, aes(LON, LAT, col = PM10),alpha=0.7)
labs(title= "PM10 Concentration in Seoul Area at South Korea",
     x="Longitude", y= "Latitude", size="PM10(microgm/m3)")

coordinates(seoul032823) <- ~LON+LAT
class(seoul032823)
LON.range <- c(126.5, 127.5)
LAT.range <- c(37, 38)
seoul032823.grid <- expand.grid(
  LON = seq(from = LON.range[1], to = LON.range[2], by = 0.01),
  LAT = seq(from = LAT.range[1], to = LAT.range[2], by = 0.01))

plot(seoul032823.grid)
points(seoul032823, pch= 16,col="red")

coordinates(seoul032823.grid)<- ~LON+LAT ## sp class
gridded(seoul032823.grid)<- T
plot(seoul032823.grid)
points(seoul032823, pch= 16,col="red")

seoul032823_OK <- autoKrige(formula = PM10~1,
                            input_data = seoul032823,
                            new_data = seoul032823.grid )


myPoints <- data.frame(seoul032823) # 지점 데이터
myKorea <- data.frame(skorea) # 지도데이터
myKrige <- data.frame(seoul032823_OK$krige_output@coords, 
                      pred = seoul032823_OK$krige_output@data$var1.pred) # 공간상의 크리그 데이터

ggplot()+ theme_minimal() +
  geom_tile(data = myKrige, aes(x= LON, y= LAT, fill = pred)) +
  geom_map(data= myKorea, map= myKorea, aes(map_id=id,group=group),
           fill=NA, colour="black") +
  coord_cartesian(xlim= LON.range ,ylim= LAT.range) +
  labs(title= "PM10 Concentration in Seoul Area at South Korea",
       x="Longitude", y= "Latitude")+
  theme(title= element_text(hjust = 0.5,vjust = 1,face= c("bold")))+
  scale_fill_viridis(option="magma")

library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(spdep)

tokyo1 <- st_read("C:/Users/Desmpnd/Documents/japan/jpn/jpn_admbnda_adm1_2019.shp")

tokyomap1 <- tokyo1 %>%
  dplyr::filter(str_detect(ADM0_EN, "Japan"))%>%
  st_transform(., 27700)
qtm(tokyomap1)

library(tidyverse)

tokyo1980 <-  read_csv("C:/Users/Desmpnd/Documents/japan/emp15.csv")

tokyomap1985 <- tokyomap1%>%
  merge(.,
        tokyo1980,
        by.x="ADM1_PCODE", 
        by.y="Area Code")

coordsW1 <- tokyomap1985%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW1,axes=TRUE)

tokyonb1985 <- tokyomap1985 %>%
  poly2nb(., queen=T)

#Plot Queen neighbors
plot(tokyonb1985, st_geometry(coordsW1), col="red")

#Add map
plot(tokyomap1985$geometry, add=T)

tokyolist1985 <-  nb2listw(tokyonb1985, style='W')
print(tokyolist1985)

globalmoran1985 <- moran.test(tokyomap1985$`Employed`, tokyolist1985)
globalmoran1985
globalmoran1985[["p.value"]]

moranplot1985 <- moran.plot(tokyomap1985$`Employed`, listw = nb2listw(tokyonb1985, style='W'))

local1 <- localmoran(x = tokyomap1985$`Employed`, listw = nb2listw(tokyonb1985, style='W'))
summary(local1)

quadrant <- vector(mode="numeric",length=nrow(local1))

#Centering variables with mean
m.epp1 <- tokyomap1985$`Employed` - mean(tokyomap1985$`Employed`)
m.local1 <- local1[,1] - mean(local1[,1])

#Setting Significant level
signif <- 0.05

#Setting quadrants
quadrant[m.epp1 >0 & m.local1>0] <- 4  
quadrant[m.epp1 <0 & m.local1<0] <- 1      
quadrant[m.epp1 <0 & m.local1>0] <- 2
quadrant[m.epp1 >0 & m.local1<0] <- 3
quadrant[local1[,5]>signif] <- 0 

#Plotting
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(tokyomap1985, main="Local Moran's I in 1985", border="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)], max.plot = 1,)
legend("bottomleft", title="LISA Cluster", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")


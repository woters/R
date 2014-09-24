library(rgdal)
library(mapproj)
library(maptools)
library(gpclib)
library(rgeos)
library(ggplot2)

load("UKR_adm1.RData")

Ukr.adm1.spdf <- get("gadm")

Ukr.adm1.df <- fortify(Ukr.adm1.spdf, region = "NAME_1")

vrp.df <- data.frame(id= unique(Ukr.adm1.df[,'id']),
                              vrp = c(24558,12096,14529,22675,44650,38907,23379,29972,87910,19920,40483,97429,72082,25950,24387,24838,27070,38424,18860,25872,21722,96644,17088,20253,39249,30656,19551))
Ukr.adm1.df <- merge(Ukr.adm1.df, vrp.df, by.y = 'id', all.x = TRUE)
Ukr.adm1.centroids.df <- data.frame(long = coordinates(Ukr.adm1.spdf)[, 1], 
                                    lat = coordinates(Ukr.adm1.spdf)[, 2]) 
Ukr.adm1.centroids.df[, 'ID_1'] <- Ukr.adm1.spdf@data[,'ID_1']
Ukr.adm1.centroids.df[, 'NAME_1'] <- Ukr.adm1.spdf@data[,'NAME_1']
p <- ggplot(Ukr.adm1.df, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = cut(vrp,5))) +
  geom_text(data = Ukr.adm1.centroids.df, aes(label = NAME_1, x = long, y = lat, group = NAME_1), size = 3) + 
  labs(x=" ", y=" ") + 
  theme_bw() + scale_fill_brewer('GDP of Ukraine (2012)', palette  = 'BuPu') + 
  coord_map() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())
print(p)
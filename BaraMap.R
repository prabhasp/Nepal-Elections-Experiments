require(maptools); require(gpclib); require(stringr); require(plyr); require(ggplot2)


# setwd("~/Code/Nepal-Elections-Experiments/")

# For me, the NepalMaps repo (https://github.com/prabhasp/NepalMaps/) is one directory above
vdcshp <- readShapeSpatial("../NepalMaps/baselayers/NPL_adm/NPL_adm4.shp")
bara <- subset(vdcshp, NAME_3 == "Bara")

# We want to map all the vdcs in bara, including names, so we need the centroids
# for this, we need to calculate centroids, and then merge in the rest of the data
# about them to get the vdc names
require(rgeos)
baravdcs <- data.frame(gCentroid(bara, byid=TRUE, id=bara$ID_4))
baravdcs$ID_4 <- row.names(baravdcs)
baravdcs <- merge(baravdcs, bara@data, by="ID_4")

# Okay, we have the data now. Lets "fortify" bara so its easier to plot via ggplot,
# and make the plot!
bara.ff <- fortify(bara, region="NAME_4")
ggplot() + 
  geom_polygon(data=bara.ff, aes(x=long, y=lat, group=group, fill=id)) + # the polygons
  geom_text(data=baravdcs, aes(x=x, y=y, label=NAME_4)) # the vdc names

ggsave("BaraPlot.pdf", height=20, width=30, units="in")

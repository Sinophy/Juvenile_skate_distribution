
library (rgdal)
library(ggplot2)
Shapef <- readOGR(dsn = "data/shp/data_boundary.shp")
plot(Shapef)
shape_df <- fortify(Shapef)

head(shape_df)

LLU <- LongLatToUTM(x = shape_df$long, 
                   y = shape_df$lat, 
                   zone = 29,
                   Hemisphere = "north")
shape_df$xkm <- (LLU$X)/1000
shape_df$ykm <- (LLU$Y)/1000

head(shape_df)


coastline <- shape_df[,c("xkm", "ykm")]
N <- nrow(coastline)
Coast.rev <- coastline[N:1, c("xkm", "ykm")]


#_______________________________________________________________________________

# Its a little different for a mesh with an island...

Shapeland <- readOGR(dsn = "data/23/mesh_poly_land.shp")
plot(Shapeland)
Shapesea <- readOGR(dsn = "data/23/mesh_poly_sea.shp")
plot(Shapesea)

# Function to use one spdf as a hole for another spdf

library("sp")
AddHoleToPolygon <-function(poly,hole){
  # invert the coordinates for Polygons to flag it as a hole
  coordsHole <-  hole@polygons[[1]]@Polygons[[1]]@coords
  newHole <- Polygon(coordsHole,hole=TRUE)
  
  # punch the hole in the main poly
  listPol <- poly@polygons[[1]]@Polygons
  listPol[[length(listPol)+1]] <- newHole
  punch <- Polygons(listPol,poly@polygons[[1]]@ID)
  
  # make the polygon a SpatialPolygonsDataFrame as the entry
  new <- SpatialPolygons(list(punch),proj4string=poly@proj4string)
  new <- SpatialPolygonsDataFrame(new,data=as(poly,"data.frame"))
  
  return(new)
}
  
# Run the function
load(url("http://spatcontrol.net/CorentinMBarbu/misc/spdf.rda"))
punchedPoly <-AddHoleToPolygon(Shapesea,Shapeland)

# Convert to df
shape_df <- fortify(punchedPoly)

# Location stuff
LLU <- LongLatToUTM(x = shape_df$long, 
                    y = shape_df$lat, 
                    zone = 29,
                    Hemisphere = "north")
shape_df$xkm <- (LLU$X)/1000
shape_df$ykm <- (LLU$Y)/1000

head(shape_df)

coastline <- shape_df[,c("xkm", "ykm")]
N <- nrow(coastline)
Coast.rev <- coastline[N:1, c("xkm", "ykm")]



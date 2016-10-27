library(spdep)
library(geosphere)
library(threejs)
library(rworldmap)
library(leaflet)
library(rgeos)
library(raster)
library(geojsonio)
library(ggplot2)
library(DT)


gifts <- read.csv("../input/gifts.csv")

gifts_coord <- cbind(gifts$Longitude, gifts$Latitude)  # set spatial coordinates
gifts_sp <- SpatialPoints(gifts_coord, proj4string=CRS("+proj=longlat"))
gifts_spdf <- SpatialPointsDataFrame(gifts_sp, data = gifts)

sample_rows <- 1000

north_pole <- data.frame(Latitude=90,Longitude=0,Weight=0,r=1)
coordinates(north_pole) <- c("Longitude", "Latitude")  # set spatial coordinates
north_pole_sp <- SpatialPoints(north_pole,  proj4string=CRS("+proj=longlat"))

gifts_spdf$dist_north_pole <- distHaversine(gifts_sp,north_pole_sp)
gifts_spdf$bearing_north_pole <- bearing(north_pole_sp,gifts_sp)

datatable(gifts_spdf@data[1:sample_rows,], rownames = FALSE)


hist(gifts_spdf$bearing_north_pole, main = "Count of gifts by bearing from North Pole")

plot(gifts_sp[1:sample_rows,])

worldMap <- getMap()
world.points <- fortify(worldMap)


world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  geom_point(aes(x=gifts[1:sample_rows,3], y=gifts[1:sample_rows,2]),color="yellow", size=1) + 
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap

worldmap +coord_map("ortho", orientation=c(40, 0, 0))

worldmap <- ggplot() + 
  geom_polygon(data = world.df[world.df$region != 'Antarctica',], aes(x = long, y = lat, group = group)) +
  geom_point(aes(x=gifts[1:sample_rows,3], y=gifts[1:sample_rows,2]),color="yellow", size=1) + 
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("azequidist",orientation=c(90, 0, 0))
worldmap

earth <- system.file("images/world.jpg",  package="threejs")

test_flights <- data.frame(origin_lat = rep(90,sample_rows), origin_long = rep(0,sample_rows), dest_lat = gifts[1:sample_rows,'Latitude'], dest_long = gifts[1:sample_rows,'Longitude'])

globejs(img=earth, arcs=test_flights,
        arcsHeight=0.3, arcsLwd=2, arcsColor="#ffff00", arcsOpacity=0.15,
        atmosphere=TRUE)
		
		model <- kmeans(gifts[,2:3],5564,iter.max =50)

gifts_spdf <- gifts_spdf[order(gifts_spdf$GiftId),]
gifts_spdf$TripId <-  model$cluster
gifts_spdf <- gifts_spdf[order(gifts_spdf$TripId, -gifts_spdf$Latitude),]

clip.extent <- as(extent(-125, -65, 49,24), "SpatialPolygons")
proj4string(clip.extent) <- CRS(proj4string(gifts_spdf))
gifts_spdf_subset <- gifts_spdf[clip.extent,]

trip_split <- split(gifts_spdf_subset@data[,c(3,2)], f = gifts_spdf_subset$TripId, drop = TRUE)

L1 <- sapply(trip_split,Line)
Ls1 = Lines(L1, ID = "1")
SL1 = SpatialLines(list(Ls1))

SL1_geojson <- geojson_json(SL1) # uncomment to see lines if running locally


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  setView(-90, 40, 5) %>% # map location
  addCircles(color = "black", data= gifts_spdf[clip.extent,] ) %>%
  addGeoJSON(SL1_geojson, weight = 3, color = "red") # uncomment to see lines if running locally
m 
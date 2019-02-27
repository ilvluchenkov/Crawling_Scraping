library(sp)

coords = matrix(c(78.46801, 19.53407,
                  78.46801, 19.74557,
                  78.83157, 19.74557,
                  78.83157, 19.53407,
                  78.46801, 19.53407), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps1, axes = TRUE)

e <- as(raster::extent(78.46801, 78.83157, 19.53407, 19.74557), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e)

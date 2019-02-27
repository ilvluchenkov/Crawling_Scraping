---
title: "Web Mapping in R"
author: "Bhaskar Karambelkar"
date: "2017/01/13"
output:
  pdf_document: default
  html_document:
    df_print: paged
  xaringan::moon_reader:
    chakra: libs/remark-latest.min.js
    css:
    - default
    - style.css
    lib_dir: libs
    nature:
      countIncrementalSlides: no
      highlightStyle: github
      navigation:
        click: no
        scroll: no
      ratio: '16:9'
subtitle: using Leaflet
---

```{r setup, include=FALSE}
options(htmltools.dir.version = FALSE)
```

```{r libs, echo=FALSE, message=FALSE, warning=FALSE}
suppressPackageStartupMessages(library(widgetframe))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))
```


```{r eval=FALSE, tidy=FALSE}
devtools::install_github("rstudio/leaflet")
```





# Building a Map

```{r eg0, eval=FALSE}
leaflet(data) | leafletProxy() %>% 
  
  
  setView(lat, lon, zoom) # Initial View OR
  fitBounds(lat_se, lon_se, latnw, lon_nw) # Initial Bounds
  setMaxBounds(lat_se, lon_se, latnw, lon_nw) # Max Bounds
  
  addTiles() | addProviderTiles() | addWMSTiles() #Tiles
  
  addMarkers() | addCircleMarkers() |
    addAwesomeMarkers() | addLabelOnlyMarkers() # Markers
  
  addPolylines() | addCircles() |
  addRectangles() | addPolygons() # Shapes
  
  addRasterImage(image) # Raster Data
  
  addLegend() | addLayersControl() | addControl() # Controls
  
```
# Minimal Example
```{r eg1.print, eval=FALSE}
library(leaflet)
leaflet()
```

```{r eg1, echo=FALSE, message=FALSE, warning=FALSE}
leaflet() %>%
  frameWidget(height='275')
```
# Tiles
```{r eg2.print, eval=FALSE}
leaflet() %>%
  addTiles()
```

```{r eg2, echo=FALSE, message=FALSE, warning=FALSE}
leaflet() %>%
  addTiles() %>% setMapWidgetStyle() %>%
  frameWidget(height='275')
```

# Provider Tiles

```{r eg3.print, eval=FALSE}
leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
  addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
  addLayersControl(baseGroups=c('Dark','Light'))
```

```{r eg3, echo=FALSE, message=FALSE, warning=FALSE}
leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
  addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
  addLayersControl(baseGroups=c('Dark','Light')) %>%
  setMapWidgetStyle() %>%
  frameWidget(height='275')
```
# Adding Markers
```{r eg4, eval=FALSE}

leaflet(data) %>%
  
  addMarkers(
    lat = ~latitude, lon = ~longitude,
    
    options = markerOptions(),
  
    label=~label, labelOptions = labelOptions(),
    popup=~popup, popupOptions = popupOptions(),
    
    clusterOptions = clusterOptions(),
    
    group = 'Group-A')

  # Similarly 
  addCircleMarkers()  # Fixed scale Circles
  addAwesomeMarkers() # More choices for icons
  addLabelOnlyMarkers() # No icon
```
# Markers Example
```{r eg5.0, warning=FALSE}
quak <- head(result[c(13,14,8,7,9)], 500)
quak$rnk <-abs(log(quak$rnk))/max(abs(log(quak$rnk)))

quakes.df <- quak %>% dplyr::mutate(
    mag.level = cut(rnk,c(.1,.2,.5,1.0),
    labels = c('> .1 & <=.2', '>.2 & <=.3', '>.5 & <=1.0'))) %>%
  split(.$mag.level)

l <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap)

names(quakes.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=quakes.df[[df]], lng=~long, lat=~lat,
                 label=~as.character(rnk), popup=~as.character(rnk),
                 group = df,
                 clusterOptions = markerClusterOptions())
  })

l <- l %>%
  addLayersControl(
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = providers$Esri.OceanBasemap, width = 120, height=80)
```
# Markers Example
```{r eg5.1, echo=FALSE, message=FALSE, warning=FALSE}
l %>%
  frameWidget(height='940')
```
# Adding Shapes
```{r eg6, eval=FALSE}

leaflet(data) %>%
  addPolygons(
    label=~label, labelOptions = labelOptions(),
    popup=~popup, popupOptions = popupOptions(),
             
    # Shape Options
    options = pathOptions(),
    weight = 1, opacity=0.8, color = "#000000",
    fillColor="#ff0000", fillOpacity=0.7,
             
    # Highlighting on mouse-over
    highlightOptions = highlightOptions(
      color='#00ff00', weight = 2,
      opacity = 1, fillOpacity = 1,
      bringToFront = TRUE, sendToBack = TRUE),
             
    group = 'Group-A')

  #Similarly
  addCircles()
  addPolylines()
  addRectangles()
```
# Shapes Example
```{r eg 7.0, echo=FALSE, warning=FALSE}
library(magrittr)
fName <- 'world-population.geo.json'
spdf <- geojsonio::geojson_sp(rmapshaper::ms_simplify(readr::read_file(fName)))
spdf@data %<>% dplyr::mutate(
  AREA = as.numeric(as.character(AREA)),
  POP2005 = as.numeric(as.character(POP2005))
)

spdf <- subset(
  spdf,
  !(is.na(AREA) | AREA <1 | is.na(POP2005) | POP2005<1)
)

spdf@data %<>%
  dplyr::mutate(
  POPDENSITY = POP2005/AREA
)

spdf.world <- spdf
```

```{r eg7.1}
# spdf is a sp::SpatialPolygonsDataFrame
qpal <- colorQuantile(rev(viridis::viridis(5)),
                      spdf$POPDENSITY, n=5)

l <- leaflet(spdf, options =
               leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME, ' ',
      formatC(POPDENSITY, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#333333', opacity=1,
    fillColor = ~qpal(POPDENSITY), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)
    ) %>%
  addLegend(
    "topright", pal = qpal, values = ~POPDENSITY,
    title = htmltools::HTML("Population Density<br/>(2005)"),
    opacity = 1 )

```
# Shapes Example
```{r eg7.2, echo=FALSE, message=FALSE, warning=FALSE}
l %>% setMapWidgetStyle() %>%
  frameWidget(height='400', width='95%')
```
# Basic use 
```{r eg8, eval=FALSE}
leaflet(options = 
          leafletOptions(crs = leafletCRS()))
```


# Projections Example 1.

```{r eg9.0, echo=FALSE}
spdf <- spdf.world
```

```{r eg9.1}
crs.molvidde <- leafletCRS(
  crsClass="L.Proj.CRS", code='ESRI:53009',
  proj4def= '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
  resolutions = c(65536, 32768, 16384, 8192, 4096, 2048))

l <- leaflet(
  spdf,
  options = leafletOptions(
    maxZoom = 5, crs= crs.molvidde, attributionControl = FALSE)) %>%
  addGraticule(style= list(color= '#999', weight= 0.5, opacity= 1)) %>%
  addGraticule(sphere = TRUE,
               style= list(color= '#777', weight= 1, opacity= 0.25)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME, ' ', formatC(POPDENSITY, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#ffffff', opacity=1,
    fillColor = ~qpal(POPDENSITY), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)) 
```

---

# Projections Example 1.

```{r eg9.3, echo=FALSE, message=FALSE, warning=FALSE}
l %>%
  setView(10,0,0.5) %>% setMapWidgetStyle() %>%
  frameWidget(height='440')
```

---

# Projections Example 2.

```{r eg10.0}
spdf <- rmapshaper::ms_simplify(albersusa::usa_composite())
pal <- colorNumeric(palette = "Blues", domain = spdf@data$pop_2014)

crs.laea <- leafletCRS(
  crsClass="L.Proj.CRS", code='EPSG:2163',
  proj4def='+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
  resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128))

l <- leaflet(
  options= leafletOptions(
    worldCopyJump = FALSE, crs=crs.laea, attributionControl = FALSE)) %>%
  addPolygons(
    data=spdf, label=~stringr::str_c(
      name, ' ', formatC(pop_2014, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight = 1, color = "#000000",
    fillColor=~pal(pop_2014), fillOpacity=0.7,
    highlightOptions = highlightOptions(
      color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
      bringToFront = TRUE, sendToBack = TRUE))
```

---

# Projections Example 2.

```{r eg10.1, echo=FALSE, message=FALSE, warning=FALSE}
l %>%
  fitBounds(-125, 24 ,-75, 45) %>%
  setMaxBounds(-125, 24 ,-75, 45) %>%
  setMapWidgetStyle() %>% 
  frameWidget(height='400')
```

---

# Projections Example 3.


```{r eg11.0, echo=FALSE}
suppressPackageStartupMessages(library(tilegramsR))

states <- FiveThirtyEightElectoralCollege.states@data$state
factpal <- colorFactor(
  colormap::colormap(nshades = length(states), 
                     colormap = colormap::colormaps$jet ), 
  states)
```

```{r eg11.1}
l <- leaflet(
  options=leafletOptions(
    crs = leafletCRS("L.CRS.Simple"),
    minZoom = -2, maxZoom = -2,
    dragging = FALSE, zoomControl = FALSE, attributionControl = FALSE)) %>%
  addPolygons(
    data=FiveThirtyEightElectoralCollege,
    weight=1,color='#000000', fillOpacity = 0.5, opacity=0.2,
    fillColor= ~factpal(state)) %>%
  addPolygons(
    data=FiveThirtyEightElectoralCollege.states, group = 'states',
    weight=2,color='#000000',
    fill = T, opacity = 1, fillOpacity = 0,
    highlightOptions = highlightOptions(weight = 4)) %>%
  addLabelOnlyMarkers(
    data=FiveThirtyEightElectoralCollege.centers,
    label = ~as.character(state),
    labelOptions = labelOptions(
      noHide = 'T', textOnly = T,
      offset=c(-8,-20), textsize = '12px'))
```

---

# Projections Example 3

```{r eg11.2, echo=FALSE, message=FALSE, warning=FALSE}
l %>%
  leaflet.extras::setMapWidgetStyle() %>%
  setMapWidgetStyle() %>% 
  frameWidget(height='450')
```

# leaflet.extras

- Add/Modify/delete/style markers/shapes using [Leaflet.Draw](http://rpubs.com/bhaskarvk/leaflet-draw).

- Add [GeoJSON](http://rpubs.com/bhaskarvk/geojsonv2), [TopoJSON](http://rpubs.com/bhaskarvk/topojsonv2), [KML](http://rpubs.com/bhaskarvk/kml), [GPX](http://rpubs.com/bhaskarvk/gpx), [CSV](http://rpubs.com/bhaskarvk/csv) files directly.
  + Customizable Markers and Shapes
  + Choropleths from polygon data w/ auto legends and bi-directional highlighting
  + Popup showing properties in a tables

- Create [Heatmap](http://rpubs.com/bhaskarvk/leaflet-heatmap) from point data.

- [Search](http://rpubs.com/bhaskarvk/leaflet-search) Markers. Geo-locate using OSM Nominatum API.

- [Pulsating](http://rpubs.com/bhaskarvk/leaflet-pulseIcon) and [Weather](http://rpubs.com/bhaskarvk/leaflet-weather) icons for markers.

- [Tiles Caching](http://rpubs.com/bhaskarvk/TileLayer-Caching), [GPS](https://github.com/stefanocudini/leaflet-gps), and many more!

---

# TopoJSON Example

```{r eg12.0}
library(leaflet.extras)
fName <- 'crimes_by_district.topojson'

l <- leaflet() %>%
  addBootstrapDependency() %>%
  setView(-75.14, 40, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addGeoJSONChoropleth(
    readr::read_file(fName), valueProperty ='incidents',
    scale = 'OrRd', mode='q', steps = 5, padding = c(0.2,0),
    popupProperty = propstoHTMLTable(
      props = c('dist_numc', 'location', 'incidents', '_feature_id_string'),
      table.attrs = list(class='table table-striped table-bordered'),drop.na = T),
    labelProperty = JS('function(feature){return "WARD: " + feature.properties.dist_numc;}'),
    color='#ffffff', weight=1, fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      fillOpacity=1, weight=2, opacity=1, color='#000000',
      bringToFront=TRUE, sendToBack = TRUE),
    legendOptions = legendOptions(title='Crimes', position='topright'))
```

---

# TopoJSON Example

```{r eg12.1, echo=FALSE, message=FALSE, warning=FALSE}
l %>%
  frameWidget(height='450')
```

library(rgdal)
library(leaflet)
library(htmlwidgets) #saveWidget()

source('../../manipulate/gpx2leaflet.r')

setwd('data/201509_Italy/')

# Read in required GPX-data and photo metadata
# =============================================

# Map 1: Cinque Terre (two tracks)
# --------------------------------

# Cinque Terre H1: Moterosso-Corniglia (23/09/2015)
cinque.h1.gpx.fn <- 'monterosso_corniglia/20150923_Italy_Moterosso-Corniglia.gpx'
cinque.h1.track <- readOGR(cinque.h1.gpx.fn, layer = "tracks")
cinque.h1.wp <- readOGR(cinque.h1.gpx.fn, layer = "track_points")
stopifnot(all(dim(cinque.h1.track) == c(1,13)),
          all(dim(cinque.h1.wp) == c(2838,26)))

cinque.h1.photos <- generatePhotoMarkers(
  photo_dir = 'monterosso_corniglia/photos/',
  waypoints = cinque.h1.wp,
  base_url = 'monterosso_corniglia/photos/',
  time_offset = 5260)


# Cinque Terre H2: Corniglia-Vernazza (24/09/2015)
cinque.h2.gpx.fn <- 'corniglia_vernazza/20150924_Italy_Corniglia-Vernazza.gpx'
cinque.h2.track <- readOGR(cinque.h2.gpx.fn, layer = "tracks")
cinque.h2.wp <- readOGR(cinque.h2.gpx.fn, layer = "track_points")
stopifnot(all(dim(cinque.h2.track) == c(1,13)),
          all(dim(cinque.h2.wp) == c(2298,26)))

cinque.h2.photos <- generatePhotoMarkers(
  photo_dir = 'corniglia_vernazza/photos/',
  waypoints = cinque.h2.wp,
  base_url = 'corniglia_vernazza/photos/',
  time_offset = 7200)


# Map 2: Portofino (single track)
# -------------------------------

portofino.h1.gpx.fn <- 'portofinovetta_sanfrutuoso/20150926_Italy_PortofinoVetta-SanFrutuoso.gpx'
portofino.h1.track <- readOGR(portofino.h1.gpx.fn, layer = "tracks")
portofino.h1.wp <- readOGR(portofino.h1.gpx.fn, layer = "track_points")
stopifnot(all(dim(portofino.h1.track) == c(1,13)),
          all(dim(portofino.h1.wp) == c(1638,26)))

portofino.h1.photos <- generatePhotoMarkers(
  photo_dir = 'portofinovetta_sanfrutuoso/photos/',
  waypoints = portofino.h1.wp,
  base_url = 'portofinovetta_sanfrutuoso/photos/',
  time_offset = 7200)


# Map 3: Aosta (two tracks)
# -------------------------

# Aosta H1: Cogne-Lillaz
aosta.h1.gpx.fn <- 'cogne_lillaz/20150930_Italy_Cogne-Lillaz.gpx'
aosta.h1.track <- readOGR(aosta.h1.gpx.fn, layer = "tracks")
aosta.h1.wp <- readOGR(aosta.h1.gpx.fn, layer = "track_points")
aosta.h1.photos <- generatePhotoMarkers(
  photo_dir = 'cogne_lillaz/photos/',
  waypoints = aosta.h1.wp,
  base_url = 'cogne_lillaz/photos/',
  time_offset = 7200)

# Aosta H2: Gimillan-Grauson
aosta.h2.gpx.fn <- 'gimillan_grauson/20151001_Italy_Gimillan-Grauson.gpx'
aosta.h2.track <- readOGR(aosta.h2.gpx.fn, layer = "tracks")
aosta.h2.wp <- readOGR(aosta.h2.gpx.fn, layer = "track_points")
aosta.h2.photos <- generatePhotoMarkers(
  photo_dir = 'gimillan_grauson/photos/',
  waypoints = aosta.h2.wp,
  base_url = 'gimillan_grauson/photos/',
  time_offset = 7200)



# Construct 3 joint Leaflet maps
# ==============================

# Construct the base map used for all maps 
# ---------------------------------------

photoIcon <- makeIcon(
  iconAnchorX = 12, iconAnchorY = 12,
  iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
)

m.base <- leaflet() %>%
  
  # Add tiles
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Topographical", "Road map", "Satellite"),
    overlayGroups = c("Hiking routes", "Photo markers"),
    options = layersControlOptions(collapsed = FALSE))

m.base


# Map 1 for Cinque Terre
# --------------------

m.cinque <- m.base %>% 
  
  # Add legend
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('blue', 'red'), 
            labels = c('Monterosso-Corniglia (23/09)',
                       'Corniglia-Vernazza (24/09)'),
            title = 'Hikes Italy, Cinque Terre') %>%
  
  # Add tracks
  addPolylines(data=cinque.h1.track, 
               color='blue', group='Hiking routes') %>%
  addPolylines(data=cinque.h2.track, 
               color='red', group='Hiking routes') %>%
  
  # Add photo markers
  addMarkers(data=cinque.h1.photos, 
             popup=cinque.h1.photos$popup_html, 
             icon = photoIcon,
             group='Photo markers') %>%
  addMarkers(data=cinque.h2.photos, 
             popup=cinque.h2.photos$popup_html,
             icon = photoIcon,
             group='Photo markers')

m.cinque



# Map 2 for Portofinno
# --------------------

m.portofino <- m.base %>% 
  
  # Add legend
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('red'), 
            labels = c('Portofino-San Fruttuoso (26/09)'),
            title = 'Hikes Italy, region Portofino') %>%
  
  # Add tracks
  addPolylines(data=portofino.h1.track, 
               color='red', group='Hiking routes') %>%
  
  # Add photo markers
  addMarkers(data=portofino.h1.photos, 
             popup=portofino.h1.photos$popup_html, 
             icon = photoIcon,
             group='Photo markers')

m.portofino


# Map 3 for Aosta
# ---------------

m.aosta <- m.base %>% 
  
  # Add legend
  addLegend(position = 'topright',opacity = 0.4, 
            colors = c('blue', 'red'), 
            labels = c('Cogne-Lillaz (30/09)', 
                       'Gimillan-Grausson (01/10)'),
            title = 'Hikes Italy, region Aosta') %>%
  
  # Add tracks
  addPolylines(data=aosta.h1.track, 
               color='blue', group='Hiking routes') %>%
  addPolylines(data=aosta.h2.track, 
               color='red', group='Hiking routes') %>%
  
  # Add photo markers
  addMarkers(data=aosta.h1.photos, 
             popup=aosta.h1.photos$popup_html, 
             icon = photoIcon,
             group='Photo markers') %>%
  addMarkers(data=aosta.h2.photos, 
             popup=aosta.h2.photos$popup_html,
             icon = photoIcon,
             group='Photo markers')


saveWidget(widget = m.cinque, file="cinque.html", selfcontained = TRUE)
saveWidget(widget = m.portofino, file="portofino.html", selfcontained = TRUE)
saveWidget(widget = m.aosta, file="aosta.html", selfcontained = TRUE)


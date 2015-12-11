

gpx_file <- "data/MyTracks/gpx/Italy_Gilillan-Grausson-Dessud.gpx"
photo_dir <- 'data/MyTracks/pictures/MyTracks15/'
base_url <- 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks15/'

# read in GPX waypoints
wp <- readOGR(gpx_file, layer = "track_points")
wp$time <- ymd_hms(wp$time)


# read in GPX track
track <- readOGR(gpx_file, layer = "tracks")


# plot waypoints

# cut off waypoints

# cut off track?

# set full track name/label if needed (used in the pop-up box)
track$name
track$name <- 'Wandelroute Gimillan-Grauson (Noord-Italië)'

# add track legend label (shorter)
track$legend_lbl <- 'Gimillan-Grauson'
# track$color <- "blue"
# track$group <- "Wandelroutes"

# retrieve associated photo timestamps
d.photos <- photo_metadata(photo_dir, base_url)


# change/align timestamps if needed
d.photos$photo_timestamp <- d.photos$photo_timestamp - duration(2, 'hours')

# match timestamp with closest WP
d.photos <- cbind(d.photos, 
                  ldply(d.photos$photo_timestamp, .fun = nearest_waypoint, wp))
d.photos

wp <- merge(wp, d.photos, by='track_seg_point_id')

coordinates(wp[!is.na(wp$photo_timestamp),])

dim(wp)
# insert photo metadata into WP dataframe 
# <-> waarom wp, niet geplot => photo df meegeven


# generate brewer colors for each track


# construct map

addPhotoMarkers()



m <- m.base %>% addPolylines(data = track, 
                             color=track$color, 
                             weight = 4,
                             popup = track$name, 
                             group = track$group)

m   %>% addMarkers(
  d.photos$coords.x1,  d.photos$coords.x2,
  #popup = as.vector(popups_html), 
  group="Foto-markeringen")

# Add legend
addLegend(position = 'topright',opacity = 0.4, 
          colors = "red", 
          labels = track_legend_label,
          title = 'Legend') %>%
  
  
  m






# Cinque Terre walking routes
# ---------------------------

t1.gpx.fn <- 'data/MyTracks/gpx/23-9-2015 11_19.gpx'
t2.gpx.fn <- 'data/MyTracks/gpx/24-8-2015 12_00.gpx'

t1.track <- readOGR(t1.gpx.fn, layer = "tracks")
t2.track <- readOGR(t2.gpx.fn, layer = "tracks")

# t1.photos <- generatePhotoMarkers(
#   photo_dir = 'data/MyTracks/pictures/MyTracks14/',
#   waypoints = readOGR(t1.gpx.fn, layer = "track_points"),
#   base_url = 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks14/',
#   time_offset = 7200)

t2.photos <- generatePhotoMarkers(
  photo_dir = 'data/MyTracks/pictures/MyTracks12/',
  waypoints = readOGR(t2.gpx.fn, layer = "track_points"),
  base_url = 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks12/',
  time_offset = 7200)

m.cinque <- m.base %>% 
  
  # Add legend
  addLegend(position = 'topright',opacity = 0.4, 
            colors = c('blue', 'red'), 
            labels = c('Monterosso-Corniglia (23/09)', 'Corniglia-Vernazza (24/09)'),
            title = 'Wandelingen Cinque Terre') %>%
  
  # Add tracks
  addPolylines(data=t1.track, color='blue', group='Wandelroutes') %>%
  addPolylines(data=t2.track, color='red', group='Wandelroutes') %>%
  
  # Add photo markers
  #addMarkers(data=t1.photos, popup=t1.photos$popup_html, group='Foto-markeringen') %>%
  addMarkers(data=t2.photos, popup=t2.photos$popup_html, group='Foto-markeringen')

m.cinque


# Wandelingen regio Aoste
# -----------------------

t3.gpx.fn <- 'data/MyTracks/gpx/Italy_Congne_Lillaz.gpx'
t4.gpx.fn <- 'data/MyTracks/gpx/Italy_Gilillan-Grausson-Dessud.gpx'

t3.track <- readOGR(t3.gpx.fn, layer = "tracks")
t4.track <- readOGR(t4.gpx.fn, layer = "tracks")

t3.photos <- generatePhotoMarkers(
  photo_dir = 'data/MyTracks/pictures/MyTracks14/',
  waypoints = readOGR(t3.gpx.fn, layer = "track_points"),
  base_url = 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks14/',
  time_offset = 7200)

t4.photos <- generatePhotoMarkers(
  photo_dir = 'data/MyTracks/pictures/MyTracks15/',
  waypoints = readOGR(t4.gpx.fn, layer = "track_points"),
  base_url = 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks15/',
  time_offset = 7200)

m.aosta <- m.base %>% 
  
  # Add legend
  addLegend(position = 'topright',opacity = 0.4, 
            colors = c('blue', 'red'), 
            labels = c('Cogne-Lillaz (30/09)', 'Gimillan-Grausson (01/10)'),
            title = 'Hikes region Aosta') %>%
  
  # Add tracks
  addPolylines(data=t3.track, color='blue', group='Hiking routes') %>%
  addPolylines(data=t4.track, color='red', group='Hiking routes') %>%
  
  # Add photo markers
  addMarkers(data=t3.photos, popup=t3.photos$popup_html, group='Photo markers', 
             icon = photoIcon) %>%
  addMarkers(data=t4.photos, popup=t4.photos$popup_html, group='Photo markers', 
             icon = photoIcon)


m.aosta


# Wandeling Portofino

# Wandelingen regio Aoste
# -----------------------

t5.gpx.fn <- 'data/MyTracks/gpx/26-9-2015 13_24.gpx'

t5.track <- readOGR(t5.gpx.fn, layer = "tracks")

t5.photos <- generatePhotoMarkers(
  photo_dir = 'data/MyTracks/pictures/MyTracks13/',
  waypoints = readOGR(t5.gpx.fn, layer = "track_points"),
  base_url = 'data/MyTracks/pictures/MyTracks13/',
  time_offset = 7200)

m.portofino <- m.base %>% 
  
  # Add legend
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('red'), 
            labels = c('Portofino-San Fruttuoso (26/09)'),
            title = 'Hikes region Portofino') %>%
  
  # Add tracks
  addPolylines(data=t5.track, color='red', group='Hiking routes') %>%
  
  # Add photo markers
  addMarkers(data=t5.photos, popup=t5.photos$popup_html, 
             group='Photo markers', 
             icon = photoIcon)

m.portofino


photoIcon <- makeIcon(
  iconAnchorX = 12, iconAnchorY = 12,
  iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
)





library(htmlwidgets)
saveWidget(widget = m.aosta, file="aosta.html", selfcontained = TRUE)
saveWidget(widget = m.cinque, file="cinque.html", selfcontained = TRUE)
saveWidget(widget = m.portofino, file="portofino.html", selfcontained = TRUE)

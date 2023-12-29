library(rgdal)
library(leaflet)
library(plyr)
library(lubridate)

exif_datetime <- function(path) {
  # read out the picture-taken datetime for a file using exiftool
  
  exif_cmd <- 'exiftool -T -r -DateTimeOriginal '  
  cmd <- paste(exif_cmd, '"', path, '"', sep='')
  exif_timestamp <- system(cmd, intern = TRUE)
  
  exif_timestamp
}

# exif depends on the libexif C library, which must be installed for the package to work.
# install.packages('exif')
# https://cran.r-project.org/web/packages/exif/index.html
# Enkel JPEG's?

# Te proberen
# https://github.com/Ironholds/rgeolocate

photo_metadata <-function(photo_dir, base_url) {
  # return a dataframe with picture metadata for all pics in a directory
  # the base_url is set to provide the correctu path for the picture in 
  # the popup HTML
  
  photos_paths <- normalizePath(list.files(photo_dir, full.names = TRUE))
  photos_metadata <- ldply(photos_paths, exif_datetime)
  names(photos_metadata) <- 'photo_timestamp'
  photos_metadata$photo_timestamp <- ymd_hms(photos_metadata$photo_timestamp)
  photos_metadata$photo_full_path <- photos_paths
  photos_metadata$photo_url <- paste(base_url, basename(photos_paths), sep='')  
  
  photos_metadata
}



photo_metadata <-function(photo_dir, base_url) {
  # return a dataframe with picture metadata for all pics in a directory
  # the base_url is set to provide the correctu path for the picture in 
  # the popup HTML
  
  photos_paths <- normalizePath(list.files(photo_dir, full.names = TRUE))
  photos_metadata <- ldply(photos_paths, exif_datetime)
  names(photos_metadata) <- 'photo_timestamp'
  photos_metadata$photo_timestamp <- ymd_hms(photos_metadata$photo_timestamp)
  photos_metadata$photo_full_path <- photos_paths
  photos_metadata$photo_url <- paste(base_url, basename(photos_paths), sep='')  
  
  photos_metadata
}

nearest_waypoint <- function(time_stamp, waypoints, threshold=60) {
  # for a given datetime, return the information about the nearest timepoint
  # if time difference is larger then threshold, return no WP.
  
  waypoints$time <- ymd_hms(waypoints$time)
  
  if (min(abs(waypoints$time - time_stamp)) > threshold) {
    return(NULL)
  }
  
  wp_position <- which.min(abs(waypoints$time - time_stamp))
  wpd <- as.data.frame(waypoints)
  wp_data <- wpd[wp_position,c('track_seg_point_id', 'coords.x1', 'coords.x2', 'ele')]
  rownames(wp_data) <- NULL
  names(wp_data) <- c('track_seg_point_id', 'longitude', 'latitude', 'ele')
  wp_data
}

walk.photodata <- photo_metadata('/home/rstudio/data/hiking_pictures/MyTracks47/', 'http://www.mhermans.net/hikes/photos/isa/')

# 1_45_16 PM (eerste aan vijver Kon. Boudewijnpark fase 2)
exif_datetime('/home/rstudio/var/data/hiking_pictures/MyTracks46/26-feb_-2017 5_26_28 PM.jpeg')




# https://www.thunderforest.com/
# 
# * GPSLogger: https://play.google.com/store/apps/details?id=com.mendhak.gpslogger
# * Geo Tracker: https://play.google.com/store/apps/details?id=com.ilyabogdanovich.geotracker&hl=en

# Data uit MyTracks DB
# * http://android.stackexchange.com/a/113788
# * https://www.melaneum.com/blog/python/saving-data-from-android-sqlite-to-gpx


# - 1_45_16 PM (eerste aan vijver Kon. Boudewijnpark fase 2)
# - 1_49_36 PM (halverwege brug Kon. BDWpark fase 2)
# - 1_55_28 PM (Kleine Sint-Annastraat)
# - 2_34_05 PM (linksvoor chalet Laarbeekbos)
# - 2_37_04 PM (ingang centraal pad Laarbeekbos)

gpx.fn <- 'var/data/hiking_data/20170311_mytracks_gpx/26-2-2017 16_47.gpx'
gpx.fn <- 'var/data/hiking_data/20170311_mytracks_gpx/11-3-2017 13_44.gpx'

gpx.track <- readOGR(gpx.fn, layer = "tracks")
gpx.points <- readOGR(gpx.fn, layer = "track_points")
# stopifnot(all(dim(cinque.h1.track) == c(1,13)),
#           all(dim(cinque.h1.wp) == c(2838,26)))

m.base <- leaflet() %>%
  
  # Add tiles
  # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  
  # Layers control
  addLayersControl(
    # baseGroups = c("Topographical", "Road map", "Satellite"),
    baseGroups = c("Road map", "Satellite"),
    overlayGroups = c("Hiking routes", "Photo markers"),
    options = layersControlOptions(collapsed = FALSE))

m.base


m.walk <- m.base %>% 
  
  # Add legend
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c('blue', 'red'), 
            labels = c('Monterosso-Corniglia (23/09)',
                       'Corniglia-Vernazza (24/09)'),
            title = 'Hikes Italy, Cinque Terre') %>%
  
  # Add tracks
  addPolylines(data=gpx.track, 
               color='blue', group='Hiking routes')
  
# addPolylines(data=cinque.h2.track, 
               # color='red', group='Hiking routes') %>%
  
  # Add photo markers
  # addMarkers(data=cinque.h1.photos, 
  #            # popup=cinque.h1.photos$popup_html, 
  #            icon = photoIcon,
  #            group='Photo markers') %>%
  # addMarkers(data=cinque.h2.photos, 
  #            popup=cinque.h2.photos$popup_html,
  #            icon = photoIcon,
  #            group='Photo markers')

m.walk

library(htmlwidgets)
saveWidget(m.walk, '/home/rstudio/projects/hikrtools/voor_sylvie.html')
getwd()

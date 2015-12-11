library(rgdal)
library(leaflet)
library(lubridate)
library(sp) # voor spDists()
library(ggplot2)
library(plyr)
#library(maps)

gpx_file <- "data/MyTracks/gpx/Italy_Congne_Lillaz.gpx"
photo_dir <- 'data/MyTracks/pictures/MyTracks14/'

gpx_file <- "data/MyTracks/gpx/Italy_Gilillan-Grausson-Dessud.gpx"
photo_dir <- 'data/MyTracks/pictures/MyTracks15/'

# read GPX-files
# --------------

track <- readOGR(gpx_file, layer = "tracks")
wp <- readOGR(gpx_file, layer = "track_points")

summary(wp)


# TODO match time stamps
# ----------------------

tail(wp$time)

coordinates(wp)[100,]

wp$time_obj <- ymd_hms(wp$time)
min(wp$time_obj)

wp$time_obj <- ymd_hms(wp$time, tz='UTC') # wat is de tijdzone die MyTracks gebruikt?

photo_dt <- ymd_hms('2015-09-30 16:04:07 UTC')
wp$time_obj - photo_dt





# Plot height
# -----------

names(wp)
plot(wp$time, wp$ele)
plot(wp$time_obj, wp$ele)

p <- ggplot(as.data.frame(wp), aes(x=time_obj, y=ele))
p + geom_point() # + geom_line() + geom_smooth()
p + geom_line()


# Bereken snelheid
# ----------------

# bereken de afstand tussen opeenvolgende punten
head(wp)

m.dist <- spDists(wp, segments=TRUE)
dim(wp)
length(m.dist)
sum(m.dist) # 12.26km, klopt

head(arrive, -1)[1]
tail(arrive, -1)[1]

tdiff <- head(arrive, -1) - tail(arrive, -1)
#as.period(tdiff, 'milliseconds')

# Get images creation dates
# -------------------------

# overgekopieerde foto's hebben geen correcte creation date meer -> exif metadata
#file.info('data/MyTracks/pictures/MyTracks12/24-aug_-2015 14_39_13.jpeg')




get_exif_datetime <- function(path) {
  exif_cmd <- 'exiftool -T -r -DateTimeOriginal '  
  cmd <- paste(exif_cmd, '"', path, '"', sep='')
  output <- system(cmd, intern = TRUE)
  output
}

list.files(photo_dir)

photo_paths <- normalizePath(list.files(photo_dir, full.names = TRUE))
photos <- ldply(photo_paths, get_exif_datetime)
dates$paths <- photo_paths

names(photos) 

lbls <- names(dates)
dates <- ymd_hms(dates)
names(dates) <- lbls
dates


wp$time_obj <- ymd_hms(wp$time)

wp$time[1]

min(wp$time_obj)
min(dates)

max(wp$time_obj)
max(dates)



length(dates)

lbls <- names(dates)
tdiff <- hms("16.05.13") - hms("14.35.00")
dates <- ymd_hms(dates) - tdiff
names(dates) <- lbls

tail(dates,1)



get_nearest_coords <- function(time_stamp, waypoints, time_diff) {
  wp_times <- ymd_hms(waypoints$time)
  wp_times <- wp_times + time_diff
  min_dist <- min(abs(wp_times - time_stamp))
  wp_position <- which.min(abs(wp_times - time_stamp))
  wpd <- as.data.frame(wp)
  coords <- wpd[wp_position, c('coords.x1', 'coords.x2')]
  
  coords
}

get_nearest_coords(tail(dates,1), wp, tdiff)

tdiff <- hms("02.00.00") - hms("00.00.00") # 2u diff klopt?
photo_pos <- ldply(dates, get_nearest_coords, wp, tdiff)
names(photo_pos) <- c('fn', 'lng', 'lat')
m.gimillan_grauson <- generate_map(
  readOGR("data/MyTracks/gpx/Italy_Gilillan-Grausson-Dessud.gpx", layer = "tracks"),
  "Wandelroute Gimillan-Grauson", "Gimillan-Grauson (01/10)", photo_pos)
m.gimillan_grauson

m.cogne_lillaz <- generate_map(
  readOGR("data/MyTracks/gpx/Italy_Congne_Lillaz.gpx", layer = "tracks"),
  "Wandelroute Cogne-Lillaz", "Cogne-Lillaz (30/09)", photo_pos)
m.cogne_lillza

# Generate map
# ------------

generate_map <- function(track, track_label, track_legend_label, markers) {

  #http://localhost:8788/files/hiking_vis/data/MyTracks/pictures/MyTracks14/30-sep_-2015%2016_04_07.jpeg
  
  popup_html <- "<div>
                <img width=100%, height=100% src='PHOTOPATH' />
                </div>
                <div>Foto op $datum, $hoogte ($lat, $long).</div>"
  
  base_url <- 'http://localhost:8787/files/projects/hiking_vis/data/MyTracks/pictures/MyTracks14/'
  urls <- paste(base_url, basename(markers$fn), sep='')
  popups_html <- sapply(urls, function(x) { sub('PHOTOPATH', x, popup_html) } )
  
  map <- leaflet(track) %>%
    
    # Add tiles as baseGroup
    addProviderTiles("OpenMapSurfer.Roads",
                     group = "Wegenkaart") %>%
    addProviderTiles("Thunderforest.Landscape",
                     group = "Topografisch") %>%
    addProviderTiles("Esri.WorldImagery",
                     group = "Satelliet") %>%
    
    # Add layers as overlayGroup
    addPolylines(color="red", weight = 4,
                 popup=track_label, 
                 group = "Wandelroutes")  %>%
    
    addMarkers(
      markers$lng,  markers$lat,
      #coordinates(wp)[100,1], coordinates(wp)[100,2],
      popup = as.vector(popups_html), 
      group="Foto-markeringen") %>%
    
    # Add legend
    addLegend(position = 'topright', colors = "red", 
              labels = track_legend_label, opacity = 0.4,
              title = 'Legend') %>%
    
    # Layers control
    addLayersControl(
      baseGroups = c("Wegenkaart", "Topografisch", "Satelliet"),
      overlayGroups = c("Wandelroutes", "Foto-markeringen"),
      options = layersControlOptions(collapsed = FALSE)
    ) 
  
  map  
  
}



m.cogne_lillaz <- generate_map(
  readOGR("data/MyTracks/gpx/Italy_Congne_Lillaz.gpx", layer = "tracks"),
  "Wandelroute Cogne-Lillaz", "Cogne-Lillaz (30/09)", photo_pos)
m.cogne_lillaz

m.gimillan_grauson <- generate_map(
  readOGR("data/MyTracks/gpx/Italy_Gilillan-Grausson-Dessud.gpx", layer = "tracks"),
  "Wandelroute Gimillan-Grauson", "Gimillan-Grauson (01/10)", photo_pos)
m.gimillan_grauson


m.monterosso_corniglia <- generate_map(
  readOGR("data/MyTracks/gpx/23-9-2015 11_19.gpx", layer = "tracks"),
  "Wandelroute Monterosso-Corniglia", "Monterosso-Corniglia (23/09)", photo_pos)

m.corniglia_vernazza <- generate_map(
  readOGR("data/MyTracks/gpx/24-8-2015 12_00.gpx", layer = "tracks"),
  "Wandelroute Corniglia-Vernazza", "Corniglia-Vernazza (24/09)", photo_pos)

m.portofinovetta_sanfrutuoso <- generate_map(
  readOGR("data/MyTracks/gpx/26-9-2015 13_24.gpx", layer = "tracks"),
  "Wandelroute Portofino Vetta-San Frutuoso", "Portofino Vetta-San Frutuoso (26/09)", photo_pos)

m.monterosso_corniglia
m.corniglia_vernazza
m.portofinovetta_sanfrutuoso
m.cogne_lillaz
m.gimillan_grauson


# Reconstruct map piecemeal

track_legend_label <- 'testlbl'

m <- leaflet() %>%
  
  # Add tiles
  addProviderTiles("OpenMapSurfer.Roads", group = "Wegenkaart") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topografisch") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satelliet") %>%

  # Layers control
  addLayersControl(
    baseGroups = c("Wegenkaart", "Topografisch", "Satelliet"),
    overlayGroups = c("Wandelroutes", "Foto-markeringen"),
    options = layersControlOptions(collapsed = FALSE))

  # Add legend
  addLegend(position = 'topright',opacity = 0.4, 
            colors = "red", 
            labels = track_legend_label,
            title = 'Legend') %>%
  

m



map <- leaflet()

map %>%   addProviderTiles("OpenMapSurfer.Roads", group = "Wegenkaart")

map <- map %>%   addProviderTiles("OpenMapSurfer.Roads", group = "Wegenkaart")

# Add layers as overlayGroup
m <- m %>% addPolylines(data=track, color="blue", weight = 4,
             popup='track_label', 
             group = "Wandelroutes")

str(track)
as.data.frame(track)  

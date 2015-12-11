library(plyr)
library(lubridate)


exif_datetime <- function(path) {
  # read out the picture-taken datetime for a file using exiftool
  
  exif_cmd <- 'exiftool -T -r -DateTimeOriginal '  
  cmd <- paste(exif_cmd, '"', path, '"', sep='')
  exif_timestamp <- system(cmd, intern = TRUE)
  
  exif_timestamp
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

deg_to_dms<- function (degfloat){
  # from: http://www.edenextdata.com/?q=content/decimal-degrees-degree-minute-seconds-decimal-degrees-r-script
  deg <- as.integer(degfloat)
  minfloat <- 60*(degfloat - deg)
  min <- as.integer(minfloat)
  secfloat <- 60*(minfloat - min)
  ### Round seconds to desired accuracy:
  secfloat <- round(secfloat, digits=3 )
  ### After rounding, the seconds might become 60
  ### The following if-tests are not necessary if no 
  ### rounding is done.
  if (secfloat == 60) {
    min <- min + 1
    secfloat <- 0
  }
  if (min == 60){
    deg <- deg + 1
    min <- 0
  }
  dm<-paste(deg,min,sep="°")
  dms<-paste(dm,secfloat,sep="'")
  dms<-paste(dms,'"',sep='')
  return (dms)
}  

generatePhotoMarkers <- function(photo_dir, waypoints, base_url, time_offset=0) {
  
  # read in exif timestamps, add to metadata dataframe
  d.photos <- photo_metadata(photo_dir, base_url)
  
  # change/align timestamps if needed
  d.photos$photo_timestamp <- d.photos$photo_timestamp - duration(time_offset, 'seconds')
  
  # match timestamp with closest WP
  d.photos <- cbind(
    d.photos, 
    ldply(d.photos$photo_timestamp, .fun = nearest_waypoint, waypoints))
  
  generate_popup_html <- function(x){
    html <-  "<div>
                <a target='_blank' href='PHOTOPATH'><img width=100%, height=100% src='PHOTOPATH' /></a>
    </div>
    <div>Photo taken on DATUM, at a height of HOOGTEm wiht coordinates (LAT, LONG).</div>"
    html <- gsub('PHOTOPATH', x$photo_url, html)
    html <- gsub('DATUM', x$photo_timestamp, html)
    html <- gsub('HOOGTE', x$ele, html)
    html <- gsub('LAT', deg_to_dms(x$latitude), html)
    html <- gsub('LONG', deg_to_dms(x$longitude), html)
    html
  }
  
  d.photos$popup_html <- daply(d.photos, 1, .fun=generate_popup_html)
  
  d.photos
}

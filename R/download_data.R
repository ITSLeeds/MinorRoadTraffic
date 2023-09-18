#' Download OSM data for a named location
#'
#' @param x name passed to osmdata default "Leeds, UK"
#' @export
#'
download_osm = function(x = "Leeds, UK"){
  q = osmdata::opq(osmdata::getbb(x))
  q = osmdata::add_osm_feature(q, key = "highway")

  ### Return an OSM Overpass query as an osmdata object in sf format
  osm_raw = osmdata::osmdata_sf(q = q)
  return(osm_raw)
}

#' Download DfT Traffic Counts
#'
#' @param x name passed to osmdata default "Leeds, UK"
#' @export
#'
download_dft_aadt <- function(
    url = "https://storage.googleapis.com/dft-statistics/road-traffic/downloads/data-gov-uk/dft_traffic_counts_aadf.zip"
    ){

  dir.create(file.path(tempdir(),"dftaadt"))
  utils::download.file(url, file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.zip"))
  utils::unzip(file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.zip"),
               exdir = file.path(tempdir(),"dftaadt"))
  fls = list.files(file.path(tempdir(),"dftaadt"))

  aadt = read.csv(file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.csv"))

  unlink(file.path(tempdir(),"dftaadt"), recursive = TRUE)

  aadt = sf::st_as_sf(aadt,coords = c("Longitude","Latitude"),crs = 4326)

  return(aadt)
}


#' Make convex hull around traffic points
#'
#' @param x data frame of sf points
#' @param dist buffer distance, default 1000 metres
#' @export
#'
make_convex_hull = function(x, dist = 1000){
  # Make Polygon Around Traffic Data
  x <- sf::st_convex_hull(sf::st_union(x))

  # Buffer Polygon by 1km
  x <- sf::st_buffer(x, dist)
  return(x)
}



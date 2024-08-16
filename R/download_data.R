#' Download OSM data for a named location
#'
#' @param x name passed to osmdata default "Leeds, UK"
#' @param bbox alterative method using a bounding box
#' @export
#'
download_osm = function(x = "Leeds, UK", bbox = NULL){
  if(is.null(bbox)){
    bbox = osmdata::getbb(x)
  }
  q = osmdata::opq(bbox)
  q = osmdata::add_osm_feature(q, key = "highway")

  ### Return an OSM Overpass query as an osmdata object in sf format
  osm_raw = osmdata::osmdata_sf(q = q, quiet = FALSE)
  return(osm_raw)
}

#' Download DfT Traffic Counts
#'
#' @param url URL of DfT data
#' @export
#'
download_dft_aadt <- function(
    url = "https://storage.googleapis.com/dft-statistics/road-traffic/downloads/data-gov-uk/dft_traffic_counts_aadf.zip"
    ){

  dir.create(file.path(tempdir(),"dftaadt"))
  utils::download.file(url, file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.zip"))
  utils::unzip(file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.zip"),
               exdir = file.path(tempdir(),"dftaadt"))
  #fls = list.files(file.path(tempdir(),"dftaadt"))

  aadt = read.csv(file.path(tempdir(),"dftaadt","dft_traffic_counts_aadf.csv"))

  unlink(file.path(tempdir(),"dftaadt"), recursive = TRUE)

  aadt = sf::st_as_sf(aadt,coords = c("longitude","latitude"),crs = 4326)

  aadt$easting = NULL
  aadt$northing = NULL

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

#' Download PBCC data
#'
#' Downloads data from www.carbon.place
#'
#' @param url URL of data
#' @export
#'
download_pbcc = function(url = "https://github.com/creds2/CarbonCalculator/releases/download/1.0/PBCC_LSOA_data.zip"){
  dir.create(file.path(tempdir(),"pbcc"))
  utils::download.file(url, destfile = file.path(tempdir(),"pbcc","PBCC.zip"),
                       mode = "wb")
  utils::unzip(file.path(tempdir(),"pbcc","PBCC.zip"),
               exdir = file.path(tempdir(),"pbcc"))
  res = read.csv(file.path(tempdir(),"pbcc","PBCC_LSOA_data.csv"))
  unlink(file.path(tempdir(),"pbcc"), recursive = TRUE)
  return(res)
}


#' Download LSOA bounds
#'
#'
#' @param url URL of data
#' @export
#'
download_lsoa = function(url = "https://github.com/ITSLeeds/MinorRoadTraffic/releases/download/Input_Data/LSOA_2011.gpkg"){
  dir.create(file.path(tempdir(),"lsoa"))
  utils::download.file(url, destfile = file.path(tempdir(),"lsoa","LSOA_2011.gpkg"),
                       mode = "wb")
  res = sf::read_sf(file.path(tempdir(),"lsoa","LSOA_2011.gpkg"))
  res = sf::st_transform(res, 4326)
  return(res)
}

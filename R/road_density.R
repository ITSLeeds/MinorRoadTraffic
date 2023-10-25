#' Segments lines
#' From stplanr??
#' @param l r?
#' @segment_lenght ?
#' @export
line_segment <- function(l, segment_length = NA) {

  vals = sf::st_drop_geometry(l)
  l_length <- as.numeric(sf::st_length(l))
  if(l_length > segment_length) {
    l = lwgeom::st_geod_segmentize(l, max_seg_length = units::as_units(segment_length,"metres"))
    n_segments <- ceiling(l_length / segment_length)


    from_to_sequence = seq(from = 0, to = 1, length.out = n_segments + 1)
    line_segment_list = lapply(seq(n_segments), function(i)
      suppressWarnings(lwgeom::st_linesubstring(
        x = l,
        from = from_to_sequence[i],
        to = from_to_sequence[i + 1]
      )
    ))
    geom = sf::st_as_sfc(unlist(line_segment_list, recursive = FALSE))
    vals = vals[rep(1, length(geom)),]
    sf::st_geometry(vals) = geom
    sf::st_crs(vals) = 4326
    return(vals)
  } else {
    return(l)
  }

}




#' Calculate road density
#' #'
#' @param lines road network
#' @param zones zones for stats
#' @param segment_length max length of a road segment in metres
#' @export

road_density = function(lines, zones, segment_length = 1000){

  int_func <- function(zone, lines){
    zone <- sf::st_sfc(zone, crs = 4326)
    line <-  lines[zone,]
    if(nrow(line) > 0){
      line <- sf::st_intersection(zone, line)
      road_length <- sum(as.numeric(sf::st_length(line))) / 1000
    } else {
      road_length <- 0
    }

    #qtm(zone) + qtm(line, lines.col = "blue")
    lsoa_area <- as.numeric(sf::st_area(zone)) / 1e6

    data.frame(road_km = road_length,
               area_km2 = lsoa_area,
               road_density = road_length / lsoa_area)
  }
  density <- pbapply::pblapply(sf::st_geometry(zones), int_func, lines = lines)
  density <- dplyr::bind_rows(density)
  zones <- cbind(zones, density)

  message("Segmenting roads into sections of less than ",segment_length," metres")
  lines <- dplyr::group_split(lines, 1:nrow(lines))

  lines <- pbapply::pblapply(lines, function(x){
    line_segment(x, segment_length = segment_length)
  })

  lines <- dplyr::bind_rows(lines)

  lines <- sf::st_join(lines,
              zones[,c("area_km2","road_km", "road_density")],
              largest = TRUE
              )


  return(list(network = lines, zones = zones))
}



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

  int_func <- function(zone){
    t_zone <- zones[zone,]
    line <- lines[zone_inter[[zone]],]

    if(nrow(line) == 0){
      return(0)
    } else {
      line <- sf::st_intersection(t_zone, line)
      density <- sum(as.numeric(sf::st_length(line))) / 1000
      return(density)
    }
  }

  zone_inter <- sf::st_intersects(zones,lines)

  road_km <- pbapply::pbvapply(seq_along(zone_inter),
                               int_func,
                               FUN.VALUE = numeric(1))

  zones$road_km <- road_km

  zones <- zones |> dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry))/1e6,
                            road_density = road_km/area_km2)

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



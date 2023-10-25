#' Calculate centrality of minor roads
#'
#' @param lines road network
#' @param threshold minimum number of minor roads to include in a zone, default 5
#' @export
#'
road_centrality = function(lines, threshold = 5){

  lines_major <- lines[lines$road_type == "major", ]
  lines_minor <- lines[lines$road_type == "minor", ]

  bounds <- make_convex_hull(lines, 1000)


# Make subgraphs ----------------------------------------------------------

  road_cut <- sf::st_cast(sf::st_geometry(lines_major), "LINESTRING")
  # used for the bound for the subgraphs
  minor_points <- sf::st_cast(sf::st_geometry(lines_minor), "POINT")

  ### generate the zones split by major road
  zones <- lwgeom::st_split(bounds, road_cut)
  # use major road to cut the study bound
  zones <- sf::st_collection_extract(zones)
  # extract the zones as a data frame
  zones <- sf::st_as_sf(zones)

  zones_inter <- sf::st_contains_properly(zones, minor_points)
  # count the number of the minor road points that within subzones
  zones$npoints <- lengths(zones_inter)
  zones <- zones[zones$npoints > threshold, ]
  zones$id <- 1:nrow(zones)

  ### Free up memory
  rm(road_cut, zones_inter )
  gc()


# Calculate road centrality -----------------------------------------------

  ### prepare the crs for dodgr
  zones <- sf::st_transform(zones, 4326)
  lines_minor <- sf::st_transform(lines_minor, 4326)
  minor_points <- sf::st_transform(minor_points,4326)

  ### Loop over each zone and find the centrality of the minor road
  graphs <- list()

  for(i in zones$id){
    message(paste0("Doing Zone ",i,"/",nrow(zones)))
    zone_sub <- zones[zones$id == i, ]
    zone_sub <- sf::st_transform(sf::st_buffer(sf::st_transform(zone_sub, 27700), 0.0001), 4326)
    lines_sub <- lines_minor[zone_sub, , op = st_within]

    if(nrow(lines_sub) > 0){
      graph_sub <- dodgr::weight_streetnet(lines_sub, wt_profile = "motorcar")
      graph_sub <- dodgr::dodgr_centrality(graph_sub)
      graph_sub <- dodgr::merge_directed_graph(graph_sub)
      dodgr::clear_dodgr_cache()
      graph_sub <- dodgr::dodgr_to_sf(graph_sub)
      graph_sub$std_centrality <- graph_sub$centrality / zone_sub$npoints
      graphs[[i]] <- graph_sub
    }
  }

  graphs <- dplyr::bind_rows(graphs)
  lines_minor <- dplyr::left_join(graphs[,c("way_id" ,"centrality", "std_centrality")],
                           sf::st_drop_geometry(lines_minor),
                           by = c("way_id" = "osm_id"))

  names(lines_minor)[1] = "osm_id"
  lines_major$centrality = NA
  lines_major$std_centrality = NA

  lines_major = sf::st_transform(lines_major, 4326)

  lines = rbind(lines_major, lines_minor)

  return(lines)
}













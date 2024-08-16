#' Extract road network from OSM
#'
#' @param network raw OSM lines
#' @param bounds bounding polygon from `make_convex_hull`
#' @param road_types road types to be kept
#' @param col_names column names to be kept
#' @export
#'
extract_from_osm = function(network, bounds,
                            road_types = c("motorway","motorway_link","trunk","trunk_link","primary",
                                            "primary_link","secondary", "secondary_link",
                                            "tertiary", "tertiary_link", "unclassified","living_street","residential"),
                            col_names = c("osm_id","name","ref","highway",
                                         "junction","maxspeed","geometry")

                            ){

  ### The CRS of bounds might be different if counts from other
  ### sources are used
  if (sf::st_crs(bounds) != sf::st_crs(network)){
    bounds <- sf::st_transform(bounds,crs = sf::st_crs(network))
  }

  ### Return the osmdata within the boundary
  network <- network[bounds, ]

  ### Filter the items that highway type is in line with the 13 types,
  ### since only interested in roads not paths
  network <- network[network$highway %in% road_types, ]
  network <- network[, col_names]

  # Find Junctions
  graph <- dodgr::weight_streetnet(network)
  graph <- dodgr::dodgr_contract_graph(graph)
  graph <- dodgr::dodgr_to_sfc(graph)
  graph <- graph$geometry
  dodgr::clear_dodgr_cache()


  points <- sf::st_cast(graph, "POINT")
  junctions <- points[duplicated(points)]
  junctions <- sf::st_as_sf(junctions)


  return(list(network = network,
              junctions = junctions))


}

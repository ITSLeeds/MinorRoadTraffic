#' Extract road network from OSM
#'
#' @param osm raw OSM data from `download_osm`
#' @param bounds bounding polygon from `make_convex_hull`
#' @param road_types road types to be kept
#' @param col_names column names to be kept
#' @export
#'
extract_from_osm = function(osm, bounds,
                            road_types = c("motorway","motorway_link","trunk","trunk_link","primary",
                                            "primary_link","secondary", "secondary_link",
                                            "tertiary", "tertiary_link", "unclassified","living_street","residential"),
                            col_names = c("osm_id","name","ref","highway",
                                         "junction","maxspeed","geometry")

                            ){

  ### Return line, points, polys layer component respectively
  lines <- osm$osm_lines
  points <- osm$osm_points
  polys <- osm$osm_polygons # needed for roundabouts

  rm(osm)

  ### Change CRS to British national grid 27700
  lines <- st_transform(lines, 27700)
  points <- st_transform(points, 27700)
  polys <- st_transform(polys, 27700)

  ### Return the osmdata within the boundary
  lines <- lines[bounds, ]
  points <- points[bounds, ]
  polys <- polys[bounds, ]


  ### Filter the items that highway type is in line with the 13 types,
  ### since only interested in roads not paths
  polys <- polys[polys$highway %in% road_types, ]
  lines <- lines[lines$highway %in% road_types, ]

  ### Cast sf object polys into Linestring and only save 7 variables
  polys <- st_cast(polys, "LINESTRING")
  polys <- polys[, col_names]
  lines <- lines[, col_names]

  ### Put two layers that contain road types together
  lines <- rbind(lines, polys)
  rm(polys)

  # ################################################################# #
  #### PREP The Road Junction                                      ####
  # ################################################################# #

  ### OSM Points are both nodes that make up lines/polygons, and objects
  ### e.g. shops.
  ### remove points that are not nodes on the line
  ### node points on the line have no tags

  ### Get column names other than osm_id, and highway which is just for junction
  ### types, and crossing info which can be junction between cycle way and road
  col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing",
                                                   "crossing_ref","created_by","geometry")]
  points.sub <- points
  points <- points[, c("osm_id","highway")]

  ### change it into data.frame then can be calculated, and discard the geometry
  ### to increase processing efficiency
  points.sub <- as.data.frame(points.sub)
  points.sub$geometry <- NULL
  points.sub <- points.sub[, col.names]

  ### Find the points with tags and change the amount of tags into integer
  rowsum <- as.integer(rowSums(!is.na(points.sub)))

  rm(points.sub, col.names)

  ### Remove points with any tags
  points <- points[rowsum == 0, ]

  ### Check highway tag to remove things like traffic lights
  unique(points$highway)
  points <- points[is.na(points$highway) | points$highway %in%
                     c( "mini_roundabout", "crossing", "motorway_junction",
                        "turning_circle", "give_way", "turning_loop","traffic_signals"), ]

  # Delete unnecessary column timely
  points <- points[,c("osm_id","geometry")]

  ### Looking for points that intersect lines
  inter <- st_intersects(points, lines) #return the value of points
  len <- lengths(inter)
  junctions <- points[len >= 2, ] # Only keep points that intersect at least 2 lines
  # i.e. a junction

  ### Remove any duplicated points
  junctions <- junctions[!duplicated(junctions$geometry), ]

  return(list(network = lines,
              points = points,
              junctions = junctions))


}

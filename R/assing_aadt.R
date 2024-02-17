#' Fill in missing road references
#'
#' Sometimes OSM roads are missing the ref value (e.g. M25). This fills in the
#' missing values based on nearest neighbours.
#'
#' @param lines data frame of road network from `extract_from_osm`
#' @param major_ref names of major road types
#' @param nearest_neighbour number of nearest neighbours to search for, default
#'   = 30
#' @export
#'
osm_fill_ref = function(lines,
                        nearest_neighbour = 30,
                       major_ref = c("motorway","motorway_link","primary",
                                     "primary_link","trunk","trunk_link")
                       ){

  lines$road_type <- dplyr::if_else(lines$highway %in% major_ref,"major","minor")

  lines_major <- lines[lines$road_type == "major", ]
  lines_minor <- lines[lines$road_type == "minor", ]
  rm(lines)

  ### Get the centroid of each major lines
  lines_major_cents <- sf::st_coordinates(sf::st_centroid(lines_major))

  ### Find the nearest neighbour centroids , and find potentially duplicated ones
  nn = RANN::nn2(lines_major_cents, k = nearest_neighbour)

  lines_major$highway[lines_major$highway == "motorway_link"] <-  "motorway"
  lines_major$highway[lines_major$highway == "primary_link"] <-  "primary"
  lines_major$highway[lines_major$highway == "trunk_link"] <-  "trunk"

  for(k in 1:2){
    for(i in 1:nrow(lines_major)){
      if(is.na(lines_major$ref[i])){ #find the item whose ref equals to NA
        for(j in 2:nearest_neighbour){ #starts from 2 because j=1 is the point itself
          idx <- nn$nn.idx[i,j] # allocate the row number of the nearest centroid
          if(lines_major$highway[idx] == lines_major$highway[i]){
            if(!is.na(lines_major$ref[idx])){
              lines_major$ref[i] <- lines_major$ref[idx]
              break
            }
          }
        }
      }
    }
  }

  lines = rbind(lines_major, lines_minor)
  return(lines)
}


#' Fill in missing road references
#'
#' Sometimes OSM roads are missing the ref and name value (e.g. M25). This fills
#' in the missing values based on the adjacent links by an iterative process.
#'
#' @param lines data frame of road network from `extract_from_osm`
#' @param major_ref names of major road types
#' @param max_iter number of iterations (default 10)
#' @export
#'
osm_fill_ref2 = function(lines,
                         major_ref = c("motorway",
                                       "motorway_link",
                                       "primary",
                                       "primary_link",
                                       "trunk",
                                       "trunk_link"),
                         max_iter = 10) {
  lines$road_type <-
    dplyr::if_else(lines$highway %in% major_ref, "major", "minor")


  lines_major <- lines[lines$road_type == "major",]
  lines_minor <- lines[lines$road_type == "minor",]

  lines_major$highway <-
    gsub(pattern = "_link$",
         replacement = "",
         lines_major$highway)


  graph <- dodgr::weight_streetnet(
    lines,
    wt_profile = "motorcar",
    keep_cols = c("name",
                  "ref",
                  "highway",
                  "junction",
                  "road_type")
  )

  df_graph <- graph |>
    dodgr_to_sf() |>
    st_drop_geometry() |>
    select(from_id, to_id, highway, way_id, name, ref, junction)

  i <- 1
  check_len <- TRUE

  while (i <= max_iter & check_len) {
    print(i)
    major_na_ids <-
      df_graph$way_id[(is.na(df_graph$name) |
                         is.na(df_graph$ref)) &
                        df_graph$way_id %in% lines_major$osm_id] |>
      unique()

    lapply(major_na_ids,
           function(t_id) {
             t_nodes <-
               df_graph[df_graph$way_id == t_id, c("from_id", "to_id")] |>
               as.matrix() |>
               as.vector() |>
               unique()

             t_fill <- df_graph |>
               dplyr::filter(from_id %in% t_nodes |
                               to_id %in% t_nodes,
                             !(way_id %in% major_na_ids)) |>
               dplyr::summarise(occur = dplyr::n(), .by = c("name", "ref")) |>
               tidyr::drop_na() |>
               dplyr::arrange(dplyr::desc(dplyr::pick("occur"))) |>
               head(n = 1)

             if (nrow(t_fill) > 0) {
               if (is.na(unique(df_graph$name[df_graph$way_id == t_id]))) {
                 df_graph$name[df_graph$way_id == t_id] <<- t_fill$name
                 lines_major$name[lines_major$osm_id == t_id] <<-
                   t_fill$name
               }

               if (is.na(unique(df_graph$ref[df_graph$way_id == t_id]))) {
                 df_graph$ref[df_graph$way_id == t_id] <<- t_fill$ref
                 lines_major$ref[lines_major$osm_id == t_id] <<-
                   t_fill$ref
               }
             }
             return(NULL)
           })

    i = i + 1
    check_len <- length(major_na_ids) > 0
  }

  lines = rbind(lines_major, lines_minor)

  return(lines)
}


#' Assign AADT data to road network
#'
#' @param lines data frame of road network from `extract_from_osm`
#' @param junctions data frame of junction points from `extract_from_osm`
#' @param traffic AADT counts
#' @param bounds boundaries of study area
#' @param major_ref names of major road types

#' @export
#'
assign_aadt_major = function(lines, junctions, traffic, bounds,
                       major_ref = c("motorway","motorway_link","primary",
                                      "primary_link","trunk","trunk_link")){

  # Transform to British National Grid
  lines = sf::st_transform(lines, 27700)
  junctions = sf::st_transform(junctions, 27700)
  traffic = sf::st_transform(traffic, 27700)
  bounds = sf::st_transform(bounds, 27700)

  lines_major <- lines[lines$road_type == "major", ]
  lines_minor <- lines[lines$road_type == "minor", ]

  # Assign Traffic Counts to the roads
  ### Prepare the boundary
  bound_sfg <- sf::st_cast(bounds, "POLYGON")
  bound_sfc <- sf::st_sfc(bound_sfg)

  ### Separate Major and Minor Roads of the AADT Data
  traffic_major <- traffic[traffic$Road_type=="Major", ]
  traffic_minor <- traffic[traffic$Road_type=="Minor", ]

  ### Allocate traffic counts to the major roads
  roadnames <- unique(traffic_major$Road_name)
  lines.nona <- lines_major[!is.na(lines_major$ref),]

  #Create a working dataset without nas
  lines.nona <- lines.nona[,c("osm_id","ref")] #Dump unneeded data
  res.major <- pbapply::pblapply(1:length(roadnames), get.aadt.major,
                      traffic_major = traffic_major, lines.nona = lines.nona,
                      roadnames = roadnames, bound_sfc = bound_sfc)
  #apply a function over a list or vector
  res.major <- dplyr::bind_rows(res.major)
  res.major <- res.major[!is.na(res.major$osm_id), ]

  ### remove any duplicates
  res.major <- res.major[!duplicated(res.major$osm_id),]

  ### Join onto the original lines data
  lines_major <- dplyr::left_join(lines_major,res.major, by = c("osm_id" = "osm_id"))
  rm(lines.nona, roadnames, res.major)

  ### Find Junctions between minor and major roads
  minor_int <- sf::st_intersects(junctions, lines_minor)
  major_int <- sf::st_intersects(junctions, lines_major)

  minor_int = lengths(minor_int)
  major_int = lengths(major_int)
  both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

  junc_majmi = junctions[both_int,]

  ### Match Major Road AADT onto junctions
  junc_majmi <- sf::st_join(junc_majmi, lines_major[ ,"traffic_flow"])
  junc_majmi <- junc_majmi[!duplicated(junc_majmi$geom), ]

  lines_minor$traffic_flow = NA

  return(list(network = rbind(lines_major,lines_minor),
              junctions_major_minor = junc_majmi
              ))

}





#' Assign AADT data to road network
#'
#' @param e row number
#' @noRd
#'
get.aadt.major <- function(e, traffic_major, lines.nona, roadnames, bound_sfc){

  traffic.sub <- traffic_major[traffic_major$Road_name == roadnames[e],]
  traffic.sub <- traffic.sub[!duplicated(traffic.sub$geometry),]
  lines.sub <- lines.nona[lines.nona$ref == roadnames[e],]

  ### need at least 2 points to make voronoi polygons, a cell consisting of
  ### all points of the plane closer to a certain seed than to any other
  if(nrow(traffic.sub) > 1){

    voronoi <- sf::st_voronoi(sf::st_combine(traffic.sub), envelope = bound_sfc)
    voronoi <- sf::st_collection_extract(voronoi)
    voronoi <- sf::st_as_sf(voronoi)
    voronoi <- sf::st_join(voronoi, traffic.sub) #based on the geom

  }else{
    ### Make a 15000 buffer around the point
    voronoi <- sf::st_buffer(traffic.sub, 15000)
  }

  ### Find Intersections of roads with voronoi polygons
  inter <- sf::st_intersects(lines.sub,voronoi)
  ### Get aadt and ncycle values 1:nrow(lines.sub)
  lines.sub$traffic_flow <-sapply(1:nrow(lines.sub),function(x)
  {as.numeric(round(mean(voronoi$traffic_flow[inter[[x]]])),0)})

  ### Remove Unneeded Data
  lines.sub <- as.data.frame(lines.sub)
  lines.sub <- lines.sub[,c("osm_id","traffic_flow")]

  return(lines.sub)
}



#' Aim: To calculate AADT for minor roads
#' This script assumes the data has already been cleaned and the AADT for
#' major roads assigned
#' Rework of original file dropping the splitting an buffering stage
#'
#' @param lines data frame of road network from `extract_from_osm`
#' @param junc_majmi data frame of junctions between major and minor roads
#' @export
#'
minor_roads_distance = function(lines, junc_majmi){

  lines <- sf::st_transform(lines, 4326)
  junc_majmi <- sf::st_transform(junc_majmi, 4326)

  # Remove tiny sub-graphs - these are too small to consider and often are disconnected from the road network
  pregrap <- dodgr::weight_streetnet(lines, wt_profile = "motorcar")
  pregrap_tab <- as.data.frame(table(pregrap$component))
  pregrap_tab <- pregrap_tab[pregrap_tab$Freq != max(pregrap_tab$Freq),]
  pregrap <- pregrap[pregrap$component %in% pregrap_tab$Var1,]

  lines_minor <- lines[lines$road_type == "minor",]
  lines_minor <- lines_minor[!lines_minor$osm_id %in% pregrap$way_id, ]

  ### Get mid-point of minor roads, i.e. centroid on the line
  point_minor <- as.data.frame(sf::st_coordinates(lines_minor))
  point_minor <- dplyr::group_by(point_minor, L1) # Based on L1 to group the junc_minor
  point_minor <- dplyr::summarise(point_minor,
                                  X = X[round(dplyr::n()/2)],
                                  Y = Y[round(dplyr::n()/2)])
  # use the n/2 item as the group represent

  ### Make dodgr graph of minor roads
  graph <- dodgr::weight_streetnet(lines_minor, wt_profile = "motorcar")
  # use motorcar mode to weight the minor road

  graph_ids <- graph[ ,c("from_id","from_lon","from_lat","component")]
  # focus on the certain start point rather than the end points
  graph_ids <- unique(graph_ids)

  junc_majmi <- cbind(junc_majmi, sf::st_coordinates(junc_majmi))
  junc_majmi <- dplyr::left_join(junc_majmi, graph_ids, by = c("X" = "from_lon",
                                                        "Y" = "from_lat"))
  # link the start point which is the majmi junction to the data.frame
  point_minor <- dplyr::left_join(point_minor, graph_ids, by = c("X" = "from_lon",
                                                          "Y" = "from_lat"))
  # link the start point which is the minor junction to the data.frame


  ### For each minor road , find the nearest (in time) junction
  dists <- dodgr::dodgr_times(graph,
                       from = junc_majmi$from_id,
                       to = point_minor$from_id,
                       shortest = FALSE)
  notna <- colSums(!is.na(dists))
  notna <- as.data.frame(notna)

  message(sum(notna$notna==0)," roads are disconneted from the major road junctions")


  nearest_junc <- list()
  dist_junc <- list()
  for(i in 1:ncol(dists)){
    sub <- dists[,i]
    sub <- sub[!is.na(sub)]
    if(length(sub) == 0){
      nearest_junc[[i]] <- NA
      dist_junc[[i]] <- NA
    } else {
      min_name <- names(sub)[sub == min(sub, na.rm = TRUE)]
      min_dist <- min(sub, na.rm = TRUE)
      # find the minimum value and return the name of the minimum value
      if(length(min_name) == 0){
        min_name <- NA
        min_dist <- NA
        # in case that the name of the junction lost
      }
      if(length(min_name) > 1){
        min_name <- min_name[1]
        min_dist <- min_dist[1]
        # consider the condition that have more than one same min value
      }
      nearest_junc[[i]] <- min_name
      dist_junc[[i]] <- min_dist
    }

  }
  nearest_junc <- unlist(nearest_junc)
  dist_junc <- unlist(dist_junc)

  lines_minor$nearest_junc <- nearest_junc
  lines_minor$nearest_junc_dist <- dist_junc
  lines_minor$major_flow <- junc_majmi$traffic_flow[match(lines_minor$nearest_junc,
                                                          junc_majmi$from_id)]


  #lines_minor <- sf::st_transform(lines_minor, 27700)
  lines_minor <- lines_minor[!is.na(lines_minor$major_flow), ]

  lines_major <- lines[lines$road_type == "major",]
  lines_major$nearest_junc <- NA
  lines_major$nearest_junc_dist <- NA
  lines_major$major_flow <- NA

  #lines_major <- sf::st_transform(lines_major, 27700)

  lines = rbind(lines_major, lines_minor)

  return(lines)


}

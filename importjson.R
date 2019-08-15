### Only run this once to install maxrectangle
# devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')
#*#*#*#* Not used anymore?

library(maxrectangle)   #*#*# Not used anymore? 
library(geojsonsf)
library(jsonlite)
library(readr)
library(MASS)
library(sp)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggrepel)
library(sf)
library(lwgeom)
library(smoothr)
library(mapview)
library(beepr)


# Note: if you are running long code and want the system to play a sound when there is an error, you can use this
options(error = function(){beep(3)})


# For the shapefiles, we are working in CRS: 102643 (NAD 1983 State Plane California, units = us-ft)
# This projection is suited for San Francisco Bay Area and the units are in US survey feet
# All sf and spatial objects should be using this projection during R analysis. Can be transformed to a more common projection like WGS 84 for export.


### TERMINOLOGY USED IN THIS SCRIPT (Capitalization SENSITIVE)
# SF: simple feature object
# POINT: SF point object
# LINE: SF line object
# POLYGON: SF polygon object
# MULTIXX: SF multipoint / multiline object
# Points / Vertices: The POINTs that form the POLYGON for a parcel
# Edges: The LINEs that form the POLYGON for a parcel. Note the difference from the term "Side."
# Roadway: Any type of road (lane, avenue, court, street)
# Address Roadway: The specific road segment that is adjacent to a parcel, which shares the parcel address name. More relevant for corner parcels which may be adjacent to multiple 
#    streets. E.g. 234 McNair Street may be adjacent to McNair Street and McNair Court, but the address roadway is McNair Street. 
# Block: A collection of adjacent parcels, which can be outlined by its surrounding streets.
# Edge type: Descriptor of where a parcel edge is with respect to its address roadway
#   - Front: The parcel edge(s) closest to the address roadway
#   - Rear: The parcel edge furthest away from address roadway. In most cases, the rear edge should be near parallel to the front edge.
#   - Side: Remaining edges, which are separated into:
#       - Street: (Only relevant for corner parcels.) Edges adjacent to a street that are NOT front.
#       - Interior: For corner parcels, side edges that are not street side. For other parcels, all side edges are interior.

## Object: Single Family Home (SFH) parcels in EPA. 
## Source: Shapefile was generated in QGIS. parcels arranged in ascending order by APN.
## Format: SF POLYGON objects
## Purpose: Used in geospatial analysis to identify parcel edges by front, rear, side (interior), side (corner/street), then for setbacks.
parcels <- read_sf(dsn = "epaparcels", layer = "epaparcelsSFH_redo") %>% st_transform(., crs = 102643) %>% arrange(APN)

## Object: List of APNs for 3920 SFH parcels.
## Source: Assessor data, preprocessed in Excel
## Format: Numeric vector
## Purpose: Quality check on parcel source to make sure the two lists of APN are not mismatched.
list_apn <- read_csv("apn.csv")
# APNs are read in as numbers, but need to add leading "0" to match format in 'parcels'. Turned into string vector
list_apn <- paste0( "0", list_apn$apn)
parcels <- parcels %>% filter(APN %in% list_apn)

## Object: List of streets from SFH addresses
## Source: Assessor data, preprocessed in Excel
## Format: String vector
## Purpose: Join parcels to their address street. This will be necessary for linking parcels to their respective roadway for identifying the front edge of parcels.
street <- read_csv("apn_street.csv")
street$apn <- paste0( "0", street$apn)
parcels <- parcels %>% inner_join(street, by = c("APN" = "apn"))

## Object: East Palo Alto Street network
## Source: Filtered from San Mateo County Streets network.
## Format: SF LINE objects
## Purpose: Link each parcel to their respective roadway to identify front edge of parcels.
# Note: Due to variations in name capitalization (e.g. McNair vs. Mcnair) or spelling errors, some manual edits were necessary for joining all parcels
roads <- read_sf("epa_roads_adj_NAD83.geojson") %>% select(name = FULLNAME) %>% arrange(name) %>% st_set_crs(102643)
# Reformat so all lines for one street name (e.g. "Maple Ln") are represented by one MULTILINE object. Distinguish between Ln, St, Ave, etc. with the same name
# Technical help: Combining sf objects by attribute https://github.com/r-spatial/sf/issues/290
roads_combined <- st_sf(name = roads$name %>% unique() %>% .[1:161], 
                        geometry = roads %>% split(.$name) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())

## Object: Parcel points adjacent to streets, hereby coined "block points"
## Source: Result of geoprocessing done in QGIS. parcels dissolved by block, and vertices extracted from dissolved polygons.
## Format: SF POINT objects
## Purpose: These points are needed to determine which parcel edges are front facing, and to distinguish interior side from street side edges.
blocks <- read_sf(dsn = "epablocks", layer = "front_vertices_redo") %>% st_transform(., crs = 102643)
blockPts <- st_coordinates(blocks)[,1:2]
# Arrange the block points by latitude and longitude. Needs to be ordered for binary search later.
blockPts <- blockPts[order(blockPts[,1], blockPts[,2]),] %>% unique()


## Function: Checks if parcel vertex matches a block point. Flagged vertices are candidates for identifying front and street side edges. Uses a binary search on sorted list of coordinates.
## Params:
##   point: x and y coordinates given as vector of length 2
## Returns: 
##   TRUE if corresponds to a block point. FALSE if not.
isBlock <- function(point){
  index <- findInterval(point[1] - 1, blockPts[,1])  # This index may be +/- 1 from the actual value
  if (index > 0) {index <- index - 1}                   # Move back one
  for (i in 1:3){                      # Make sure a minimum of three iterations done before stopping
    while((index > 0) && (index <= dim(blockPts)[1]) && (abs(point[1] - blockPts[index,1]) <= 1.5)){         # search all coordinates w/ same X coord
      if(abs(point[2] - blockPts[index,2]) <= 1.5){return(TRUE)}
      index <- index + 1
    }
    index <- index + 1
  }
  return(FALSE)
}

## Function: Removes excessive collinear vertices from parcel
## Params:
##   Parcel coordinates, given as n by 2 numeric array
## Returns:
##   Modified parcel coordinates, n - m by 2 numeric array
removeCollinear <- function(arr){
  slope <- vector(mode = "double", length = dim(arr)[1]-1)
  for (j in 1:length(slope)){
    slope[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
  }
  i <- 2
  while (i <= length(slope)){
    if (abs(atan2(sin(slope[i] - slope[i-1]), cos(slope[i] - slope[i-1]))/pi*180) < 5){
      arr <- arr[-i,]
      slope[i-1] <- (slope[i]+slope[i-1])/2
      slope <- slope[-i]
      i <- i - 1            # adjust index, since we removed a point
    }
    i <- i + 1              # increment index
  }
  if (abs(atan2(sin(slope[1] - slope[length(slope)]), cos(slope[1] - slope[length(slope)]))/pi*180) < 5) { # Check first and last edge
    arr <- arr[-1,]        # Remove first point
    arr[dim(arr)[1],] <- arr[1,]      # Replace last point with new first point
  }
  return(arr)
}

## Object: Coordinates of parcel vertices
## Format: Vector of lists. Each list contains an array of coordinates. Each array has columns: X, Y, block pt marker. For third column, 0 means not a block point, 1 means block point.
## Purpose: Easy access to parcel vertices, which will later be labeled as block points or not.
parcelXY <- vector("list", nrow(parcels))
for (i in 1:nrow(parcels)){
  parcelXY[[i]] <- st_coordinates(parcels[i,])[,1:2] %>% unique()
  parcelXY[[i]] <- rbind(parcelXY[[i]], parcelXY[[i]][1,])
}

# Remove collinear points from parcels
for (i in 1:length(parcelXY)){
  arr <- parcelXY[[i]]
  parcelXY[[i]] <- removeCollinear(arr)
}

# Now, we need to identify block points on each parcel. The information will be stored (1) directly as a marker in the parcel object and (2) keeping track of 
# number of block points in "numBlock object."
numBlock <- vector("double", length(parcelXY))
for (i in 1:length(parcelXY)){
  poly <- parcelXY[[i]]
  # poly <- rbind(poly, poly[1,])
  parcelXY[[i]] <- cbind(poly, vector(length = nrow(poly)))
  for (j in 1:(nrow(poly)-1)){
    if (isBlock(poly[j,])){
      parcelXY[[i]][j,3] <- TRUE
      numBlock[i] <- numBlock[i] + 1
    }
  }
}

## Object: Building footprints for each SFH parcel
## Source: Patty Fronteira
## Format: JSON objects read in as SF POLYGON objects
## Purpose: Largest building is kept for analysis, since ADU cannot overlap with existing building.
bldgs <- read_sf(dsn = "epabldg", layer = "epabldgs_byAPN_redo") %>% st_transform(., crs = 102643) %>% arrange(APN)
# Clean up buildings. For each parcel, clip the buildings to parcel (and if there are multiple buildings, take largest building)
bldg_all <- NULL
# Retain flags from chunks
flags <- as.data.frame(matrix(nrow = length(parcelXY), ncol = 2))
colnames(flags) <- c("APN","NoBldg")
flags$APN <- parcels$APN
flags$NoBldg <- FALSE
for (i in 1:nrow(parcels)){
  index <- match(parcels$APN[i], bldgs$APN)
  # No building available, add an empty polygon
  if (is.na(index)){
    sf <- st_sf(APN = parcels$APN[i], geometry = st_sfc(st_polygon(list()), crs = 102643))
    flags$NoBldg[i] <- TRUE
  }
  else{
    # Clip the buildings to boundary of each parcel
    geom <- st_union(bldgs$geometry[index]) %>% st_intersection(parcels$geometry[i]) %>% st_cast("POLYGON")
    if (length(geom) == 1){sf <- st_sf(APN = parcels$APN[i], geometry = geom)}
    else{
      # Sort buildings by area
      areas <- st_area(geom)
      geom <- geom[order(areas, decreasing = TRUE)]
      # Keep the largest building
      geom <- geom[1]
      sf <- st_sf(APN = parcels$APN[i], geometry = geom)
    }
  }
  bldg_all <- rbind(bldg_all, sf)
}
#~#~# Quality Check
# These can be used to check if the identification of block points is working properly
# sum(numBlock > 0)
# table(numBlock)



## Function: Parcel vertices are reindexed to make sure that block points are not "wrapping around" and creating two "chunks" of block points.
## Chunk = consecutive block points
## False chunk = consecutive non-block points
# Example:
# Original parcel index  1 2 3 4 5
# Block point?           Y N N Y Y  (Two chunks, one false chunk)
# Reordered parcel index 4 5 1 2 3
# Block point?           Y Y Y N N  (One chunk, one false chunk)
# Each parcel is supposed to only have one "chunk" of block points. This reordering ensures that the data quality is good and is used to flag
# any parcels that have more than one chunk after reordering for manual review.
## Params:
##   i: index of parcel in parcel object.
## Returns:
##   modParcel: parcel object with modified indexing
##   flag: TRUE if more than one chunk and needs manual review. FALSE if no problems with modParcel
reIndex <- function(i){
  chunks <- 0
  false_chunks <- 0
  preVal <- FALSE
  arr <- parcelXY[[i]][-nrow(parcelXY[[i]]),]
  # print(arr)
  # If there are more than 1 chunk, unmark block points that are isolated
  for (j in 1:nrow(arr)){
    # print(j)
    if (arr[j,3] == 1 && arr[(j - 2) %% nrow(arr) + 1,3] == 0 && arr[j %% nrow(arr) + 1,3] == 0){
      arr[j,3] <- 0
    }
  }
  for (j in 1:nrow(arr)){
    if (j == 1 && arr[j,3] == 1){chunks <- chunks + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks <- chunks + 1}
    else if (preVal == TRUE && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    preVal <- arr[j,3]
  }
  offset <- 0
  # First pass: if there are more true chunks, bring true chunk at end of index to the front
  if (false_chunks < chunks){
    for (j in nrow(arr):1){
      if (arr[j,3] == 0){
        break
      }
      offset <- offset + 1
    }
    # Reorder parcel by shifting indices back by offset (in beginning example, offset = 2)
    arr <- rbind(arr[-1:-j,], arr[1:j,])
  }
  # print(arr)
  # Second pass: if there is a false chunk at the beginning, bring it to the back
  chunks <- 0
  false_chunks <- 0
  preVal <- FALSE
  for (j in 1:nrow(arr)){
    if (j == 1 && arr[j,3] == 1){chunks <- chunks + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks <- chunks + 1}
    else if (preVal == TRUE && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    preVal <- arr[j,3]
  }
  offset <- 0
  for (j in 1:nrow(arr)){
    if (arr[j,3] == 1){
      break
    }
    offset <- offset + 1
  }
  if (j != 1) {arr <- rbind(arr[-1:-offset,], arr[1:offset,])}
  # print(arr)
  # Third pass, flag any parcels with more than one chunk
  chunks <- 0
  false_chunks <- 0
  preVal <- FALSE
  for (j in 1:dim(arr)[1]){
    if (j == 1 && arr[j,3] == 1){chunks <- chunks + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks <- chunks + 1}
    else if (preVal && arr[j,3] == 0){false_chunks <- false_chunks + 1}
    preVal <- arr[j,3]
  }
  offset <- 0
  for (j in 1:nrow(arr)){
    if (arr[j,3] == 1){
      break
    }
    offset <- offset + 1
  }
  if (j != 1) {arr <- rbind(arr[-1:-offset,], arr[1:offset,])}
  # print(arr)
  arr <- rbind(arr, arr[1,])
  arr[nrow(arr), 3] <- 0
  flag <- chunks > 1
  return(list(arr, flag))
}

# Run reIndex on all parcels
for (i in 1:length(parcelXY)){
  print(i)
  parcelXY[[i]] <- reIndex(i)[[1]]
  flags$MultiChunk[i] <- reIndex(i)[[2]]
}

## Function: Extract block points as a separate array
## Params:
##   index: index of parcel
##   type: "Original" returns the original, "Mod" returns the block points after the front points were identified
## Returns:
##   n by 2 array of block point coordinates
parcelBlock <- function(index, type = "Original"){
  if (type == "Original"){
    if (numBlock[index] > 0){
      return (parcelXY[[index]][(parcelXY[[index]][,3] == 1),1:2])
    }
    else{
      print("This parcel has no block points. User needs to manually identify.")
      return (NULL)
    }
  }
  else if (type == "Mod"){
    if (numBlock[index] > 0){
      return (modParcel[[index]][(modParcel[[index]][,3] == 1),1:2])
    }
    else{
      print("This parcel has no block points. User needs to manually identify.")
      return (NULL)
    }
  }
  else{
    print("Type is wrong. Enter either Original or Mod")
    return (NULL)
  }
}

## Function: Plot parcel with front points bolded
## Params:
##   index: index of parcel
##   type: "Original" returns the original, "Mod" returns the block points after the front points were identified
## Returns:
##   plot
plotParcel <- function(index, type = "Original"){
  if (type == "Original"){
    eqscplot(parcelXY[[index]][,1:2], type='l')
    points(parcelBlock(index), pch = 16)
  }
  else if (type == "Mod"){
    eqscplot(modParcel[[index]][,1:2], type='l')
    points(parcelBlock(index, "Mod"), pch = 16)
  }
  else{
    print("Type is wrong. Enter either Original or Mod")
    return (NULL)
  }
}

# Mark parcels that are "landlocked," aka do not have any vertices identified as block points.
flags$Landlocked <- FALSE
flags$OneBlockPt <- FALSE
for (i in 1:length(parcelXY)){
  print(i)
  if (is.null(parcelBlock(i))) {flags$Landlocked[i] <- TRUE}
  if (sum(parcelXY[[i]][,3]) == 1) {flags$OneBlockPt[i] <- TRUE}
  #~#~# Quality Check
  # Check if front edge vertices are properly marked
  # else {
  #   plotParcel(i)
  # }   # Change it to plot the parcel and add the front points as dark circles
  # Sys.sleep(0.05)  # Pause and continues automatically
  # invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
}

## Function: Obtain slopes for edges connecting parcel's block point. The slopes will be used for corner parcels in combination
##   with the roadway network to properly identify the front edge, and the street edge
## Params:
##   i: index of parcel
## Returns:
##   corner parcel: double vector of angles in radians
##   not a corner parcel: NULL
# cornerSlopes <- function(i){
#   if(sum(parcelXY[[i]][,3]) < 3){return(NULL)}
#   arr <- parcelBlock(i)
#   slope <- vector("double", nrow(arr) - 1)
#   for (j in 1:length(slope)){
#     slope[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
#   }
#   return(slope)
# }

## Function: finds shortest distance between a point and a line segment
## Params:
##   x,y: coordinates for point
##   x1,y1: coordinates for first point representing line segment
##   x2,y2: coordinates for second point representing line segment
##   type: specifies which distance is returned
## Returns:
##   for "Norm": normal distance. If point is outside of line segment, then shortest distance to the projected line segment
##   for "True": true distance. If point is outside of line segment, then the distance from point to the closer point of the line segment
## Source for Norm: https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points
shortestDist <- function(x, y, x1, y1, x2, y2, type = "Norm"){
  if (type == "Norm"){
    return (abs((y2-y1) * x - (x2-x1) * y + x2*y1 - y2*x1)/sqrt((y2 - y1)^2 + (x2 - x1)^2))
  }
  else if(type == "True"){
    px <- x2 - x1
    py <- y2 - y1
    norm <- px * px + py * py
    u <- ((x - x1) * px + (y - y1) * py) / norm
    if (u > 1) {u <- 1}
    else if (u < 0) {u <- 0}
    xx <- x1 + u * px
    yy <- y1 + u * py
    dx <- xx - x
    dy <- yy - y
    return (sqrt(dx * dx + dy * dy))
  }
  else{
    print("Type is wrong. Enter either Norm or True")
    return (NULL)
  }
}






## Object: Parcel SF objects after cleaning up points
## Source: Coordinates pulled from parcelXY
## Format: SF object
## Purpose: parcels but without excess coordinates
modparcels <- NULL
for (i in 1:nrow(parcels)){
  print(i)
  poly <- st_polygon(list(parcelXY[[i]][,1:2]))
  sfc <- st_sfc(poly, crs = 102643)
  sf <- st_sf(geometry = sfc, address = parcels$address[i], APN = parcels$APN[i])
  modparcels <- rbind(modparcels, sf)
}


## Object: Parcel cooordinates with modified markers for front block points and side block points
## Source: Modified from parcelXY
## Format: Third column has been modified. 0 = non block point, 1 = front point, 2 = street side point
## Purpose: Corner parcels need a distinction between front and street side edges, since they face more than one street. 
modParcel <- parcelXY
# ## Object: 
# ## Source: 
# ## Format: 
# ## Purpose: 
# slopeDiffs <- vector("double", length(parcelXY))
# ## Object: 
# ## Source: 
# ## Format: 
# ## Purpose: 
# orderRead <- vector("character", length(parcelXY))

## Function: Find closest roadway segment linked to each parcel's address
## Params:
##    i: index of parcel
## Returns: 2 x 2 XY coordinates of roadway segment closest to parcel
closestRoad <- function(i){
  par_cent <- as.vector(st_coordinates(st_centroid(modparcels[i,])))
  roadname <- modparcels$address[i]
  ind <- match(tolower(roadname), tolower(roads_combined$name))  # Compare everything as lowercase
  if (is.na(ind)){   # Error if streets don't match
    print("Street not found")
    print(roadname)
    return(NULL)
  }
  # Extract associated road, separate out invidual linestrings
  road <- st_geometry(roads_combined[ind,]) %>% st_cast("LINESTRING")
  # Find closest point on roadway to parcel centroid, using true distance NOT norm distance
  minDist <- 1e99
  seg <- vector(length = 2)
  for (j in 1:length(road)){
    linestring <- st_coordinates(road[j])[,1:2]
    for (k in 1:(nrow(linestring)-1)){
      dist <- shortestDist(par_cent[1], par_cent[2], linestring[k,1], linestring[k,2], linestring[k+1,1], linestring[k+1,2], "True")
      if (dist < minDist){
        minDist <- dist
        seg <- linestring[k:(k+1),]
      }
    }
  }
  return(seg)
}

# Add a flag for when bldg front is improperly marked
flags$BldgFrontFail <- FALSE

## Function: Identify front of building for parcel
##   Find two building points closest in true distance to that roadway segment. Mark the front edge of building.
## Params:
##   i: index of parcel
## Returns:
##   2 x 2 array of XY coordinates of building's front edge
idBldgFront <- function(i){
  #   Filter down to closest roadway segment. 
  roadSeg <- closestRoad(i)
  #   Find two building points closest in true distance to roadway segment.
  bldg <- st_geometry(bldg_all[i,])
  bldgcoord <- st_coordinates(bldg)[,1:2]
  bldgcoord <- bldgcoord[-nrow(bldgcoord),]
  index1 <- 0
  minDist1 <- 1e99  # #1 shortest
  index2 <- 0
  minDist2 <- 1e99  # #2 shortest
  for (j in 1:nrow(bldgcoord)){
    dist <- shortestDist(bldgcoord[j,1], bldgcoord[j,2], roadSeg[1,1], roadSeg[1,2], roadSeg[2,1], roadSeg[2,2], "True")
    if (dist <= minDist1){
      minDist2 <- minDist1
      index2 <- index1
      minDist1 <- dist
      index1 <- j
    }
    else if (dist <= minDist2){
      minDist2 <- dist
      index2 <- j
    }
  }
  #   If the two identified points are not adjacent to each other, flag parcel for review
  if (abs(index1 - index2) != 1){
    if ((index1 == 1 || index2 == 1) && (index1 == nrow(bldgcoord) || index2 == nrow(bldgcoord))){}
    else{
      flags$BldgFrontFail[i]
      print("Failed to properly identify building front. The two marked points are not adjacent.")
      return(NULL)
    }
  }
  return(rbind(bldgcoord[index1,], bldgcoord[index2,]))
}

## Function: Cut out front yard using building front to split parcel. Keep the portion with the building on it. Re-mark the points for labelling edges.
## Params:
##   i: index of parcel
## Returns:
##   new ModParcel (n x 3 coordinate). For column 3: 0 = original non-block point. 1 = street edge vertices. 2 = new vertices (front-facing)
##   new parResult SF POLYGON object, with front yard cut out. This object will be used for additionally geoprocessing to remove setbacks to find buildable_area.
cutFrontYard <- function(i){
  roadSeg <- closestRoad(i)
  bldgFront <- idBldgFront(i)
  if(is.null(bldgFront) || is.null(roadSeg)){return(NULL)}
  par_cent <- as.vector(st_coordinates(st_centroid(modparcels[i,])))
  offset <- colMeans(roadSeg) - par_cent
  # Create a polygon that will erase the "front yard" using bldgFront and offset
  bldgFrontVec <- bldgFront[2,] - bldgFront[1,]
  eraser <- rbind(bldgFront[1,] - 100 * bldgFrontVec,
                  bldgFront[2,] + 100 * bldgFrontVec,
                  bldgFront[2,] + 100 * bldgFrontVec + offset,
                  bldgFront[1,] - 100 * bldgFrontVec + offset,
                  bldgFront[1,] - 100 * bldgFrontVec)
  
  eqscplot(eraser, type = 'l', col = "red")
  lines(parcelXY[[i]][,1:2], type = 'l', col = "blue")
  frontyardsf <- st_sfc(st_polygon(list(eraser)), crs = 102643)
  return(st_difference(buildable_area[i,], frontyardsf))
}

######## Rdata of all the raw data
##########################################
save.image("A2.RData")
##########################################


buildable_area <- modparcels
for (i in 1:nrow(modparcels)){
  if (sum(as.numeric(flags[i,-1])) == 0){
    print(i)
    result <- cutFrontYard(i)
    if (!is.null(result)){
      buildable_area[i,] <- result
    }
  }
}

#Mapview object that reviews result of front yard removal
mapNoFrontYard <- mapview(modparcels, col.regions = "red") + mapview(buildable_area) + mapview(bldg_all)

####### Result of removing front yard (needs to be examined for quality)
##########################################
save.image("B2.RData")
##########################################

## Function: Using the parcels with removed front yard, match to original parcel to sort block points and identify street edges for corner lots.
## Params:
##   i: index of parcel
## Returns:
##   Labeled parcel
sortCorner <- function(i){
  new <- st_coordinates(buildable_area[i,])[,1:2]
  new <- cbind(new, marker = vector(length = nrow(new)))
  orig <- parcelXY[[i]]
  for (j in 1:nrow(new)){
    matchX <- match(new[j,1],orig[,1])
    matchY <- match(new[j,2],orig[,2])
    # Case 1: No match, is a new point. Mark as a new "front-facing" vertex = 2
    if (is.na(matchX && matchY)){
      new[j,3] <- 2
    }
    # Case 2: Match, use the marker from original parcels. 0 = not-street facing, 1 = street-facing
    else if (matchX == matchY){
      new[j,3] <- orig[matchX,3]
    }
  }
  return(new)
}

##########################################
save.image("C2.RData")
##########################################

for (i in 1:length(modParcel)){
  modParcel[[i]] <- sortCorner(i)
}
beep(3)

## Function: Check if new parcel is marked properly. There should only be one consecutive chunk of vertices marked as "2" or front-facing
## Params:
##   i: index of parcel
## Returns:
##   Labeled parcel
checkCornerIndex <- function(i){
  arr <- modParcel[[i]]
  arr <- arr[-nrow(arr),]
  offset <- 0
  # First pass: move any consecutive "2"s at front to the back
  for (j in 1:nrow(arr)){
    if (arr[j,3] != 2){
      break
    }
    offset <- offset + 1
  }
  # Reorder parcel by shifting indices back by offset (in beginning example, offset = 2)
  arr <- rbind(arr[-1:-j,], arr[1:j,])
  chunks <- 0
  preVal <- FALSE
  for (j in 1:nrow(arr)){
    if (j == 1 && arr[j,3] == 2){chunks <- chunks + 1}
    else if (preVal == FALSE && arr[j,3] == 2){chunks <- chunks + 1}
    preVal <- arr[j,3] == 2
  }
  return (chunks)
}

flags$FrontYardCutFail <- FALSE
for (i in 1:length(modParcel)){
  chunks <- checkCornerIndex(i)
  if (chunks > 1){flags$FrontYardCutFail[i] <- TRUE}
}

################################################################
save.image("C2.RData")
################################################################

flags$RearIDFail <- FALSE
## Function: Create an object to track the labels (front, street, side, rear) for each parcel.
##   Front-facing: 2 to 2 only
##   Street-facing: 1 to 1 and 1 to 2
##   Side or rear facing: all other combinations. Rear side is determined as furthest (true distance) from front centroid
## Params:
##   i: index of parcel
## Returns:
##   edges: character vector with n elements (1 for each side, no wrap-around element). Each element corresponds to edge between point j and j+1
# If edges somehow tie, flag it and choose the first edge
idEdges <- function(i){
  par <- modParcel[[i]]
  edges <- vector("character", nrow(modParcel[[i]]) - 1)
  for (j in 1:length(edges)){
    # Case 1: Front-facing: 2 to 2 only
    if (par[j,3] == 2 && par[j+1,3] == 2){
      edges[j] <- "Front"
    }
    # Case 2: Street-facing: 1 to 1 and 1 to 2
    else if ((par[j,3] == 1 && par[j+1,3] == 1) || (par[j,3] + par[j+1,3] == 3)){
      edges[j] <- "Street"
    }
    else{
      edges[j] <- "Side"
    }
  }
  # To find rear edge, find edge where the midpoint is furthest from closestRoad
  road <- closestRoad(i)
  maxDist <- 0
  maxInd <- NULL
  for (j in 1:length(edges)){
    if (edges[j] != "Side"){next}
    side <- (par[j,] + par[j+1,]) / 2
    dist <- shortestDist(side[1], side[2], road[1,1], road[1,2], road[2,1], road[2,2], "True")
    # Case 1: further distance, assign new furthest edge
    if (dist > maxDist){
      maxDist <- dist
      maxInd <- j
    }
    # Case 2: distance matches, keep all furthest edges ###### Pick the longest edge
    else if (dist == maxDist){
      print(paste("Tied! j = ", j))
      maxInd <- c(maxInd, j)
    }
    # Case 3: distance is shorter. Keep moving.
  }
  if (length(maxInd) > 1){
    print("Warning! More than one street chunk")
    flags$RearIDFail[i] <- TRUE
    maxInd <- maxInd[1]
  }
  edges[maxInd] <- "Rear"  
  return(edges)
}

# Run idEdges on all parcels
for (i in 1:length(modParcel)){
  print(i)
  if (sum(as.numeric(flags[i,-1])) > 0){next}
  edges <- c(idEdges(i),"")
  modParcel[[i]] <- as_tibble(cbind(as_tibble(modParcel[[i]]), edges))
}

# Helper Function: buffer for one edge
# Input: parcel
# Param: edge_index, buffer distance
# Returns a buffer around the edge with distance dist
# Returns as a st_polygon

## Function: Create a buffer based on a polygon edge and dist. This will be used to remove setback areas from buildable_area
## Params:
##   st_par: SF POLYGON parcel object
##   ind: index of parcel 
##   dist: buffer distance
## Returns:
##   SF POLYGON object representing buffer that needs to be subtracted
buffer <- function(st_par, ind, dist){
  par <- st_coordinates(st_par)[,1:2]
  edge <- par[ind:(ind+1),]      # index and next point
  # Convert coordinates into st_linestring
  line <- st_sfc(st_linestring(edge), crs = 102643)
  # Buffer line seg by dist
  buffered <- st_buffer(line, dist, nQuadSegs = 2, endCapStyle = "SQUARE") %>% st_make_valid()
  return(buffered)
}

# Side, street, and rear buffers
# Input: parcel, edgeID (side, rear, street), building footprint, front points
# Param: side buffer dist, rear buffer dist
# Draw parallel lines that are [buffer dist] away from each side
# Select parallel lines that are closer to parcel centroid, so lines are going inward, not outward
# Find intersection between adjacent lines to reconstruct buffered parcel
# Check whether buffer overlaps with building. If it does, adjust so that there is no intersection in result
# Output: new parcel coordinates, array (n by 2)
## Function: Complete all buffers to remove side, street, rear, building setbacks.
## Params:
##   par: SF POLYGON buildable_area (after front yard cut)
##   edges: character vector label of what type of edges
##   bldg: SF POLYGON building footprint
##   side_dist/rear_dist/street_dist: distance in feet according to zoning codes
## Returns:
##   SF POLYGON object representing buildable area after all setbacks removed
allBuffers <- function(st_par, edges, st_bldg, side_dist, rear_dist, street_dist, bldg_dist = 0){
  #result <- st_difference(st_buffer(st_par,0), st_buffer(st_bldg, bldg_dist,nQuadSegs = 2, endCapStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9)) #%>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_make_valid() %>% st_set_precision(1e9)
  # result <- st_intersection(result, result)
  for (i in 1:(nrow(edges)-1)){
    if (edges[i,] == "Front"){next}
    else if (edges[i,] == "Side"){
      buff <- buffer(st_par, i, side_dist) #%>% st_set_precision(1e9)
      # result <- st_intersection(result, result)
      result <- st_difference(st_buffer(result, 0), buff)
      plot(result, col = "red")
      #result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), buff) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
    }
    else if (edges[i,] == "Street"){
      buff <- buffer(st_par, i, street_dist) #%>% st_set_precision(1e9)
      # result <- st_intersection(result, result)
      result <- st_difference(st_buffer(result, 0), buff)
      plot(result, col = "red")
      #result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), buff) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
    }
    else if (edges[i,] == "Rear"){
      buff <- buffer(st_par, i, rear_dist) #%>% st_set_precision(1e9)
      # result <- st_intersection(result, result)
      result <- st_difference(st_buffer(result, 0), buff)
      plot(result, col = "red")
      #result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), buff) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
    }
  }
  # Clean up. Get rid of any non-POLYGON SF object, and POLYGONs with areas less than 160
  return (result)
  if(st_is_empty(result)){
    return (result)
  }
  else if (st_geometry_type(result) == "GEOMETRYCOLLECTION"){
    newResult <- st_sfc(st_polygon(list()), crs = 102643)
    resultParts <- st_cast(result)
    for (j in 1:length(resultParts)){
      part <- st_sfc(resultParts[[j]], crs = 102643)
      type <- st_geometry_type(part)
      if (type == "POLYGON" || type == "MULTIPOLYGON"){
        if (st_area(part) > units::set_units(160, US_survey_foot^2)){
          newResult <- st_union(newResult, part)
        }
      }
    }
    result <- newResult
  }
  return (result)
}

################################################################
save.image("D2.RData")
################################################################


# Find the available area after buffers. No building set back. Side = 5 ft, Rear = 10 ft, Street = 12 ft
buildable_area_a <- NULL
#Errors given for: 583, 1721, 3423
flags$BufferFail <- FALSE
for (i in 1:length(modParcel)){
  if (sum(as.numeric(flags[i,-1])) == 0){
    print(i)
    st_par <- st_geometry(buildable_area[i,])
    edges <- modParcel[[i]][,4]
    st_bldg <- st_geometry(bldg_all[i,]) %>% st_buffer(0)
    geom <- tryCatch(
      {allBuffers(st_par, edges, st_bldg, 5, 10, 12, 0)},
      error = function(e){
        flags$BufferFail[i] <<- TRUE       # Global assignment needed in tryCatch, otherwise doesn't assign
        st_sfc(st_polygon(), crs = 102643)
      }
    )
    if (length(geom) == 0){
      sf <- st_sf(APN = parcels$APN[i], geometry = st_sfc(st_polygon(), crs = 102643))
    }
    else{
      sf <- st_sf(APN = parcels$APN[i], geometry = geom, crs = 102643)
    }
    buildable_area_a <- rbind(buildable_area_a, sf)
  }
  else{
    sf <- st_sf(APN = parcels$APN[i], geometry = st_sfc(st_polygon(), crs = 102643))
    buildable_area_a <- rbind(buildable_area_a, sf)
  }
}

############################################
save.image("E2.RData")
############################################

#####RESUME HERE!!!!#################

prepPoly <- function(poly){
  minX <- min(poly[,1])              # get the smallest x coord
  minY <- min(poly[,2])              # get the smallest y coord
  poly[,1] <- poly[,1]- minX         # adjust for relative coordinates
  poly[,2] <- poly[,2]- minY         # adjust for relative coordinates
  return(list(poly,minX,minY))
}

##### misfits are already filtered out
# misfits <- cbind(misfits, APN = parcels$properties.APN)
# 
# # misfits identified on buildable_area_a
# for (i in 1:nrow(buildable_area_a)){
#   buildable_area_a$misfit[i] <- misfits[which(buildable_area_a$APN[i] == misfits, TRUE)[1],1]
# }

buildable_area_a$valid <- st_is_valid(buildable_area_a)
buildable_area_a$xmin <- 0
buildable_area_a$ymin <- 0

# Get XMin and YMin and shift coordinates (avoids numerical errors)
buildable <- buildable_area_a
buildable$xmin <- 0
buildable$ymin <- 0
for (i in 1:nrow(buildable)){
  if (st_geometry_type(buildable_area_a[i,]) == "GEOMETRYCOLLECTION"){
    buildable$valid[i] <- FALSE
    next
  }
  if (st_is_empty(buildable_area_a[i,])){
    buildable$valid[i] <- FALSE
    next
  }
  print(i)
  coord <- st_coordinates(buildable[i,])[,1:2]
  buildable$xmin[i] <- min(coord[,1])
  buildable$ymin[i] <- min(coord[,2])
  st_geometry(buildable[i,]) <- st_geometry(buildable[i,]) - c(buildable$xmin[i], buildable$ymin[i])
}
############################################
save.image("F2.RData")
############################################


# Apply minimum 8 x 20 area to get rid of unviable spaces
# Merge the possible rectangles together
buildable_aadu <- NULL
flags$ViableSpaceFail <- FALSE
for (i in 1:nrow(buildable)){
  if (buildable$valid[i] == FALSE){
    sf <- st_sf(APN = buildable$APN[i], message = "Invalid geometry", geometry = st_sfc(st_polygon(), crs = 102643))
    buildable_aadu <- rbind(buildable_aadu, sf)
    next
  }
  sf <- tryCatch(
    {
      poly <- st_geometry(buildable[i,]) %>% st_cast("POLYGON")
      result <- st_sfc(st_polygon(), crs = 102643)
      msg <- ""
      print(i)
      for (j in 1:length(poly)){
        print(j)
        # Skip this one if the area is smaller than 160 SF (minimum area for ADU) or if there is no geometry
        if (as.numeric(st_area(poly[j])) < 160){
          msg <- paste(msg, "Area less than 160")
          next
        }
        if (st_is_empty(poly[j])){
          msg <- paste(msg, "Empty geometry")
          next
        }
        if (!st_is_valid(poly[j])){
          msg <- paste(msg, "Invalid geometry")
          next
        }
        rect <- largestRect(poly[j], print = TRUE)
        if (is.character(rect)){
          msg <- paste(msg, rect)
        }
        else{
          result <- st_union(result, rect)
        }
      }
      if (length(result) == 0 || st_is_empty(result)){
        st_sf(APN = buildable$APN[i], message = msg, geometry = st_sfc(st_polygon(), crs = 102643))
      }
      else{
        st_sf(APN = buildable$APN[i], message = "Success", geometry = result)
      }
    },
    error = function(e){
      flags$ViableSpaceFail[i] <<- TRUE
      st_sf(APN = buildable$APN[i], message = msg, geometry = st_sfc(st_polygon(), crs = 102643))
    }
  )
  buildable_aadu <- rbind(buildable_aadu, sf)
}

buildable_aadu_no_offset <- buildable_aadu
# Return geometries to original coordinates, no offsets
for(i in 1:nrow(buildable_aadu_no_offset)){
  print(i)
  offset <- c(buildable$xmin[i], buildable$ymin[i])
  st_geometry(buildable_aadu_no_offset[i,]) <- st_geometry(buildable_aadu_no_offset[i,]) + offset
  buildable_aadu_no_offset[i,] <- fill_holes(buildable_aadu_no_offset[i,], 1e5)
}

##############################################
save.image("G2.RData")
load("G2.Rdata")
#############################################

# Derek's additional formatting on mapview

projection <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
parcels %<>% st_transform(projection)
bldg_all %<>% st_transform(projection)
buildable_adu_no_offset %<>% st_transform(projection)

parcels <- parcels
ExistingBuilding <- bldg_all
AttachedADU <- buildable_adu_no_offset %>% filter(message == "Success")

map <- mapview(parcels, alpha.regions = 0, legend = FALSE) + mapview(ExistingBuilding, col.regions = "grey", legend = FALSE) + mapview(AttachedADU, col.regions = "green", legend = TRUE)
map
mapshot(map, "attached_adu.html")




# https://github.com/r-spatial/sf/issues/290

buildable_area_a_combined <- buildable_area_a %>% filter(inBox == TRUE)
buildable_area_a_combined <- st_sf(APN = buildable_area_a_combined$APN %>% unique(), geometry = buildable_area_a_combined %>% split(.$APN) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())
bldg0_buildable_combine <- st_sf(APN = bldg0_buildable$APN %>% unique(), geometry = bldg0_buildable %>% split(.$APN) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())

EPAbbox <- st_bbox(st_combine(bldg_all)) + c(-1000, -1000, 1000, 1000)

# Check which buildable_area_a results need to be fixed. Filter out the ones that show up outside of the EPAbbox
test <- buildable_area_a
buildable_area_a$inBox <- FALSE
for (i in 1:nrow(buildable_area_a)){
  print(i)
  bbox <- st_bbox(buildable_area_a[i,])
  if (is.na(bbox[1])){next}
  if (bbox[1] >= EPAbbox[1] && bbox[2] >= EPAbbox[2] && bbox[3] <= EPAbbox[3] && bbox[4] <= EPAbbox[4]){
    buildable_area_a$inBox[i] = TRUE
  }
}

mapview(buildable_area_a_combined)

for i in 1:nrow(bldg0_buildable_combine){
  offset
}

# The function will plot the total buildable area in green, and the result in a transparent red
# The result will look like a brown-green

#########EXPLORE HERE#################################


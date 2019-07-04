### Only run this once to install maxrectangle
# devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)
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

options(error = function(){beep(3)})
# Transform to CRS: 102643 (NAD 1983 State Plane California, units = us-ft)
parcels <- read_sf(dsn = "epaparcels", layer = "epaparcelsSFH_redo") %>% st_transform(., crs = 102643)

# Way to view the shapefiles
# mapview(parcels) + mapview(bldgs)

# Order by APN
parcels <- parcels %>% arrange(APN)


### Note: In order for the find_lr function to work as we intend (pulling all possible buildable area), we need to change the largestRect.coffee in the R library "3.5/maxrectangle/js" folder

# To save current environment
# save.image(".RData")

### parcels
# file_parcel <- "epaparcels_clean_NAD83.geojson"
# parcels <- fromJSON(txt=file_parcel, flatten = TRUE, simplifyDataFrame = TRUE)
# parcels <- parcels$features

# Filter down to 3920 SFH parcels
list_apn <- read_csv("apn.csv")
list_apn <- paste0( "0", list_apn$apn)
parcels <- parcels %>% filter(APN %in% list_apn)

### Join parcels to their address streets
street <- read_csv("apn_street.csv")
street$apn <- paste0( "0", street$apn)
parcels <- parcels %>% inner_join(street, by = c("APN" = "apn"))

### roads
# file_roads <- "epa_roads_adj_NAD83.geojson"
# roads <- fromJSON(txt=file_roads, flatten = TRUE, simplifyDataFrame = TRUE)
# 
roads <- read_sf("epa_roads_adj_NAD83.geojson") %>% select(name = FULLNAME) %>% arrange(name)
# Combining sf objects by attribute https://github.com/r-spatial/sf/issues/290
roads_combined <- st_sf(name = roads$name %>% unique() %>% .[1:161], 
                        geometry = test %>% split(.$name) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())

# # Initialize column for storing road segments
# parcels[,"list_roads"] <- NA
# for (i in 1:length(parcel)){
#   parcels$list_roads[i] <- list(roads %>% filter(toupper(properties.FULLNAME) == toupper(parcels$address[[i]])))
# }


### coordinates of front
# file_blocks <- "front_vertices_cleaned_NAD83.geojson"
blocks <- read_sf(dsn = "epablocks", layer = "front_vertices_redo") %>% st_transform(., crs = 102643)
blockPts <- st_coordinates(blocks)[,1:2]
blockPts <- blockPts[order(blockPts[,1], blockPts[,2]),] %>% unique()

# blocks <- fromJSON(txt=file_blocks, flatten = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
# blocks <- blocks$features$geometry.coordinates
# blockPts <- array(data = 0, dim = c(length(blocks), 2))
# for(i in 1:length(blocks)){
#   if(is.null(blocks[[i]]) || typeof(blocks[[i]]) != "double"){next}
#   blockPts[i,1] <- blocks[[i]][1]
#   blockPts[i,2] <- blocks[[i]][2]
# }
# blockPts <- blockPts[blockPts[,1] != 0,]
# blockPts <- blockPts[order( blockPts[,1], blockPts[,2]),]
# blockPts <- unique(blockPts)

######## Rdata of all the raw data
##########################################
save.image("A2.RData")
##########################################

# ### parcels
# file_parcel <- "epaparcels_clean.geojson"
# parcels <- read_sf(file_parcel)
# parcels <- parcels %>% select(APN, geomPar = geometry)
# # # Filter down to 3920 SFH parcels
# # file_apn <- "apn.csv"
# # apn_list <- read_csv(file_apn)
# # apn_list <- apn_list %>% transmute(apn = paste0("0", apn))
# # parcels <- filter(parcels, APN %in% apn_list$apn)
# ### Join parcels to their address streets
# file_street <- "apn_street.csv"
# street <- read_csv(file_street)
# street$apn <- paste0( "0", street$apn)
# parcels <- parcels %>% inner_join(street, by = c("APN" = "apn")) %>% arrange(APN)
#
# ### roads
# file_roads <- "epa_roads_adj.geojson"
# roads <- read_sf(file_roads)
# roads <- roads %>% aggregate(list(roads$FULLNAME), function(x) x[1]) %>% select(street = FULLNAME, roads = geometry)
# roads <- roads %>% full_join((parcels %>% st_set_geometry(NULL)), by = c("street" = "address")) %>% filter(!is.na(APN)) %>% arrange(APN)
#
# ### coordinates of front
# file_blocks <- "front_vertices_cleaned.geojson"
# blocks <- read_sf(file_blocks)
# blocksPts <- blocks %>% select(geometry) %>% unique() %>% st_coordinates()
# blocksPts <- as.array(test[,1:2])

# Check if point is at front
# Inputs one point (x, y coordinate 1 x 2 vector)
# Outputs a boolean
isFront <- function(point){
  # Check if point matches blocks
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

# Remove unnecessary points from parcel (if two adj line segments are collinear, remove middle point)
# Inputs a polygon (n by 2 array)
# Outputs a modified polygon (n by 2 array)
removeCollinear <- function(arr){
  slope <- vector(mode = "double", length = dim(arr)[1]-1)
  for (j in 1:length(slope)){
    slope[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
  }
  # print(slope)
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
    # print(arr)
    arr <- arr[-1,]        # Remove first point
    # print(arr)
    arr[dim(arr)[1],] <- arr[1,]      # Replace last point with new first point
  }
  return(arr)
}

# Create a variable that works directly with parcel coordinates (vector of lists)
parcel <- vector("list", nrow(parcels))
for (i in 1:nrow(parcels)){
  # print(i)
  parcel[[i]] <- st_coordinates(parcels[i,])[,1:2] %>% unique()
  parcel[[i]] <- rbind(parcel[[i]], parcel[[i]][1,])
}

# Remove collinear points from parcels
for (i in 1:length(parcel)){
  # print(i)
  arr <- parcel[[i]]
  parcel[[i]] <- removeCollinear(arr)
}

# Find number of landlocked parcels (all points are touching another parcel)
# First, pull all the coordinates for the first polygon for each parcel
checkFront <- vector("logical", length(parcel))   # Keep track of whether a parcel touches the front
numFront <- vector("double", length(parcel))      # Keep track of number of points that touch the front
# indexFront <- vector("list", length(parcel))      # Keep track of index of points that touch the front
for (i in 1:length(parcel)){
  print(i)
  poly <- parcel[[i]]
  # poly <- rbind(poly, poly[1,])
  parcel[[i]] <- cbind(poly, vector(length = nrow(poly)))
  for (j in 1:(nrow(poly)-1)){
    if (isFront(poly[j,])){
      checkFront[i] <- TRUE
      parcel[[i]][j,3] <- TRUE
      numFront[i] <- numFront[i] + 1
    }
  }
}

sum(checkFront)
table(numFront)

# Front indices may be out of order if the polygon starting point is in the middle of the front
chunks <- vector("double", length(parcel))              # chunks of points marked as front
false_chunks <- vector("double", length(parcel))        # chunks of points not marked as front (used to ID problems)
# offsetFront <- vector("double", length(parcel))         # what number of points to move to the back
# deleteFront <- vector("double", length(parcel))         # what index point to remove
chunks2 <- vector("double", length(parcel))              # chunks of points marked as front
false_chunks2 <- vector("double", length(parcel))        # chunks of points not marked as front (used to ID problems)
chunks3 <- vector("double", length(parcel))              # chunks of points marked as front
false_chunks3 <- vector("double", length(parcel))        # chunks of points not marked as front (used to ID problems)
for (i in 1:length(parcel)){
  print(i)
  preVal <- FALSE
  arr <- parcel[[i]][-nrow(parcel[[i]]),]
  # print(parcel[[i]])
  # print(arr)
  for (j in 1:nrow(arr)){
    if (j == 1 && arr[j,3] == 1){chunks[i] <- chunks[i] + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks[i] <- false_chunks[i] + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks[i] <- chunks[i] + 1}
    else if (preVal && arr[j,3] == 0){false_chunks[i] <- false_chunks[i] + 1}
    preVal <- arr[j,3]
  }
  # Track which parcel's front vertices need reordering
  # If false_chunks < chunks, reorder by length of first true chunk
  offset <- 0
  if (false_chunks[i] < chunks[i]){
    for (j in dim(arr)[1]:1){
      if (arr[j,3] == 0){
        break
      }
      offset <- offset + 1
    }

    # Reorder parcel
    arr <- rbind(arr[-1:-j,], arr[1:j,])
  }

  # Second check
  preVal <- FALSE
  for (j in 1:dim(arr)[1]){
    if (j == 1 && arr[j,3] == 1){chunks2[i] <- chunks2[i] + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks2[i] <- false_chunks2[i] + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks2[i] <- chunks2[i] + 1}
    else if (preVal && arr[j,3] == 0){false_chunks2[i] <- false_chunks2[i] + 1}
    preVal <- arr[j,3]
  }

  # New code
  offset <- 0
  for (j in 1:dim(arr)[1]){
    if (arr[j,3] == 1){
      break
    }
    offset <- offset + 1
  }
  if (j != 1) {arr <- rbind(arr[-1:-offset,], arr[1:offset,])}

  if (chunks2[i] == 2){
    for (j in 2:(dim(arr)[1]-1)){
      if (isTRUE(arr[j,3] == 1 && (arr[j-1,3] == 0 && arr[j+1,3] == 0))){
        arr[j,3] <- 0
      }
    }
  }
  preVal <- FALSE
  for (j in 1:dim(arr)[1]){
    if (j == 1 && arr[j,3] == 1){chunks3[i] <- chunks3[i] + 1}
    else if (j == 1 && arr[j,3] == 0){false_chunks3[i] <- false_chunks3[i] + 1}
    else if (preVal == FALSE && arr[j,3] == 1){chunks3[i] <- chunks3[i] + 1}
    else if (preVal && arr[j,3] == 0){false_chunks3[i] <- false_chunks3[i] + 1}
    preVal <- arr[j,3]
  }
  parcel[[i]] <- rbind(arr, arr[1,])
  parcel[[i]][nrow(parcel[[i]]),3] <- 0
}

# table(chunks)
# table(false_chunks)
# table(chunks2)
# table(false_chunks2)
# table(chunks3)
# table(false_chunks3)
#
# View(cbind(chunks, false_chunks))
# View(cbind(chunks2, false_chunks2))

# Function to extract parcel front points
parcelFront <- function(index, type = "Original"){
  if (type == "Original"){
    if (checkFront[index]){
      return (parcel[[index]][(parcel[[index]][,3] == 1),1:2])
    }
    else{
      print("This parcel has no front points. User needs to manually identify.")
      return (NULL)
    }
  }
  else if (type == "Mod"){
    if (checkFront[index]){
      return (modParcel[[index]][(modParcel[[index]][,3] == 1),1:2])
    }
    else{
      print("This parcel has no front points. User needs to manually identify.")
      return (NULL)
    }
  }
  else{
    print("Type is wrong. Enter either Original or Mod")
    return (NULL)
  }
}

# Function to plot parcel with front points bolded
plotParcel <- function(index, type = "Original"){
  if (type == "Original"){
    eqscplot(parcel[[index]][,1:2], type='l')
    points(parcelFront(index), pch = 16)
  }
  else if (type == "Mod"){
    eqscplot(modParcel[[index]][,1:2], type='l')
    points(parcelFront(index, "Mod"), pch = 16)
  }
  else{
    print("Type is wrong. Enter either Original or Mod")
    return (NULL)
  }
}

# Keep track of parcels that need to be manually fixed
misfits <- vector("logical", length(parcel))

# Check if front edge is properly extracted
for (i in 1:length(parcel)){
  print(i)
  if (is.null(parcelFront(i))) {misfits[i] <- TRUE}
  numFront[i] <- sum(parcel[[i]][,3])
  if (numFront[i] < 2) {misfits[i] <- TRUE}
  # else {
  #   plotParcel(i)
  # }   # Change it to plot the parcel and add the front points as dark circles
  # Sys.sleep(0.05)  # Pause and continues automatically
  # invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
}

# Identify corner lots
cornerStats <- vector(mode = "list", length(parcel))
for (i in 1:length(parcel)){
  print(i)
  if(is.null(parcelFront(i))) {next}              # Skip landlocked
  # else if (dim(parcelFront(i))[1] < 3) {next}     # Skip parcels with 1 or 2 front points (most likly not a corner)
  else if (length(parcelFront(i)) < 6) {next}
  arr <- parcelFront(i)                           # Get the front points

  # Find slopes
  slope <- vector(mode = "double", length = dim(arr)[1]-1)
  for (j in 1:length(slope)){
    slope[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
  }
  totslo1 <- atan2(arr[dim(arr)[1],2]-arr[1,2], arr[dim(arr)[1],1]-arr[1,1])
  totslo2 <- (totslo1 + pi) %% 2*pi
  # Save slopes
  cornerStats[[i]]$slopes <- slope
  # Average slope
  cornerStats[[i]]$avg <- mean(slope)
  # Difference in slope between first segment and hypotenuse (first to last point)
  cornerStats[[i]]$totdiff1 <- atan2(sin(totslo1-slope[1]),cos(totslo1-slope[1]))
  # Difference in slope between last segment and hypotenuse (first to last point)
  cornerStats[[i]]$totdiff2 <- atan2(sin(totslo2-slope[length(slope)]+pi),cos(totslo2-slope[length(slope)]+pi))
  # Difference in slope between first segment and last segment
  cornerStats[[i]]$segdiff <- atan2(sin(slope[length(slope)]-slope[1]), cos(slope[length(slope)]-slope[1]))
}


## Look at the break down of slope differences across parcels to figure out what is a good threshold
segDiff <- array(dim = c(length(parcel),2))
for (i in 1:length(parcel)){
  if (is.null(cornerStats[[i]])) {
    segDiff[i,1] <- NA
  }
  else {
    segDiff[i,1] <- abs(cornerStats[[i]]$segdiff)/pi*180
  }
}
segDiff[,2] <- 1:length(parcel)
colnames(segDiff) <- c("diff", "index")
segDiff <- na.omit(segDiff)

tibble(val = segDiff[,1]) %>%
  ggplot(., aes(x = val)) +
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..)), binwidth = 5) +
  scale_x_reverse(
    breaks = seq(-180, 180, 30)
    # , limits = c(90, 0)
    ) +
  scale_y_continuous(labels = scales::percent)

## Look at break down of totdiff
totDiff <- array(dim = c(length(parcel),2))
for (i in 1:length(parcel)){
  if (is.null(cornerStats[[i]])) {
    totDiff[i,1] <- NA
  }
  else {
    totDiff[i,1] <- abs(min(cornerStats[[i]]$totdiff1, cornerStats[[i]]$totdiff2))/pi*180
  }
}
totDiff[,2] <- 1:length(parcel)
colnames(totDiff) <- c("totdiff", "index")
totDiff <- na.omit(totDiff)

tibble(val = totDiff[,1]) %>%
  ggplot(., aes(x = val)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 5) +
  scale_y_continuous(labels = scales::percent) #+
  # scale_x_continuous(limits = c(0, 90))


isCorner <- vector("logical", length(parcel))
for (i in 1:length(parcel)){
  if (is.null(cornerStats[[i]])) {next}
  # if mostly straight, skip   *********************** try taking out this test
  # if (abs(cornerStats[[i]]$totdiff1) < (20/180*pi) || abs(cornerStats[[i]]$totdiff2) < (20/180*pi)) {next}
  # if first and last segment have more than 60 degrees diff, mark as corner
  # if (abs(cornerStats[[i]]$segdiff) >= (40/180*pi)) {isCorner[i] <- TRUE}
  else {isCorner[i] <-  TRUE}
}

# Helper function: normal distance between a point and a line segment
# (x,y) is the point, (x1,y2) and (x2,y2) make the line segment
#
# https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points
perpDist <- function(x, y, x1, y1, x2, y2){
  return (abs((y2-y1) * x - (x2-x1) * y + x2*y1 - y2*x1)/sqrt((y2 - y1)^2 + (x2 - x1)^2))
}

# Helper function: shortest distance between a point and a line segment
# (x,y) is the point, (x1,y2) and (x2,y2) make the line segment
#
# https://stackoverflow.com/a/6853926
distPtLineSeg <- function(x, y, x1, y1, x2, y2){
  A <- x - x1
  B <- y - y1
  C <- x2 - x1
  D <- y2 - y1
  dot <- A * C + B * D
  len_sq <- C * C + D * D
  param <- -1
  if (len_sq != 0){  # in case of 0 length line
    param <- dot / len_sq
  }
  xx <- 0
  yy <- 0
  if (param < 0){
    xx <- x2
    yy <- y1
  }
  else if (param > 1){
    xx <- x2
    yy <- y2
  }
  else{
    xx = x1 + param * C
    yy = y1 + param * D
  }
  dx <- x - xx
  dy <- y - yy
  return (sqrt(dx * dx + dy * dy))
}

# Check if 2 line segments intersect (line 1 made by point and slope) (line 2 made by two points)
# Input: point 1 of line segment, point 2 of line segment, point of line, slope of line
# Input: p1, p2, pt are vectors (length 2), slope is in degrees
# Output: returns true/false
# https://stackoverflow.com/a/565282/68063
checkIntersect <- function(p1, p2, pt, slope) {
  q <- p1
  s <- p2 - p1
  # if (s[1]*s[1]+s[2]*s[2] < 1e-5) {return (FALSE)} # if two points are too close together,
  p <- pt
  r <- c(cos(slope / 180 * pi), sin(slope / 180 * pi))
  if (crossprod2D(r, s) == 0) {
    # lines are parallel and do not intersect
    return(NULL)
  }
  t <- crossprod2D((q - p), s) / crossprod2D(r, s)
  u <- crossprod2D((q - p), r) / crossprod2D(r, s)
  if (u <= 1 && u >= 0) {
    return(p + t * r)
  }
  else
    return(NULL)
}

# Check if 2 lines intersect (each one consists of (x1, y1, r_x, r_y))
# Input: line1, line2 (length 4 vector)
# Output: returns point of intersection. if no intersection, returns NULL
# https://stackoverflow.com/a/565282/68063
intersectLines <- function(line1, line2) {
  p <- c(line1[1],line1[2])
  r <- c(line1[3],line1[4])
  q <- c(line2[1],line2[2])
  s <- c(line2[3],line2[4])

  # if (s[1]*s[1]+s[2]*s[2] < 1e-5) {return (FALSE)} # if two points are too close together,
  if (crossprod2D(r, s) == 0) {
    # lines are parallel and do not intersect
    return(NULL)
  }
  t <- crossprod2D((q - p), s) / crossprod2D(r, s)
  u <- crossprod2D((q - p), r) / crossprod2D(r, s)
  return(p + t * r)
}

# Helper Function: finds cross product for two vectors
crossprod2D <- function(u, v){
  return(u[1]*v[2]-u[2]*v[1])
}

########################################################
save.image("B2.RData")
########################################################

# Extract front edge for corner lot
# Read in roadway network. Find two closest roadway points to parcel centroid
# Of the two ends of the front, choose the point shortest normal dist to roadway
# Select edges that are within a certain angle of the segment on this end
modParcel <- parcel
roadway <- vector("list", length(parcel))
closestRoad <- vector("list", length(parcel))
slopeDiffs <- vector("double", length(parcel))
orderRead <- vector("character", length(parcel))
for (i in 1:length(parcel)){
  print(i)
  par_cent <- as.vector(st_coordinates(st_centroid(parcels[i,])))
  
  roadname <- parcels$address[i]
  ind <- match(tolower(roadname), tolower(roads$name))
  if (is.na(ind)){
    print("Street not found")
    print(roadname)
    break
  }
  # Extract associated road, separate out invidual linestrings
  road <- st_geometry(roads[ind,]) %>% st_cast("LINESTRING")
  
  # Find closest point on roadway to parcel centroid
  minDist <- 1e99
  seg <- vector(length = 2)
  # Grab midpoints of each line segment
  for (j in 1:length(road)){
    linestring <- st_coordinates(road[j])[,1:2]
    for (k in 1:(nrow(linestring)-1)){
      midpt <- (linestring[k,] + linestring[k+1,])/2
      dist <- ((par_cent[1] - midpt[1])/100000)^2 + ((par_cent[2] - midpt[2])/100000)^2
      if (dist < minDist){
        minDist <- dist
        seg <- linestring[k:(k+1),]
      }
    }
  }
  closestRoad[[i]] <- seg
  # We now have the slope of the roadway
  road_slo <- (atan2(seg[2,2]- seg[1,2], seg[2,1]- seg[1,1]) + 2*pi) %% 2*pi
  
  if (!is.null(cornerStats[[i]])){  # Only modify if it is marked as a corner parcel

    # Compare first and last front point. ID which one has shortest perp dist to roadway.
    first <- parcelFront(i)[1,]
    last <- parcelFront(i)[nrow(parcelFront(i)),]
    distFirst <- perpDist(first[1], first[2], closestRoad[[i]][1,1], closestRoad[[i]][1,2], closestRoad[[i]][2,1], closestRoad[[i]][2,2])
    distLast <- perpDist(last[1], last[2], closestRoad[[i]][1,1], closestRoad[[i]][1,2], closestRoad[[i]][2,1], closestRoad[[i]][2,2])
    slopes <- cornerStats[[i]]$slopes
    # Case 1: First point is closer. Starting from first edge, grab all edges going up to the bisecting slope. If slope difference is less than 90 degrees, go up to 45 degrees
    if (distFirst < distLast){
      print("First < Last")
      orderRead[i] <-  "First < Last"
      slopeDiff <- atan2(sin(slopes[length(slopes)] - slopes[1]), cos(slopes[length(slopes)] - slopes[1]))
      slopeDiffs[i] <- slopeDiff
      if (slopeDiff < 0){
        print("slopeDiff < 0")
        # delta <- max(min(slopeDiff * 0.5, -pi/6), -70/180*pi)  # Add a bit more than half, just as a buffer. Range must be between 45-70 degrees
        delta <- slopeDiff/abs(slopeDiff)*20/180*pi            # Delta is 20 degrees
        slopeCut <- atan2(sin(slopes[1] + delta), cos(slopes[1] + delta))
        for (j in 1:length(slopes)){
          if (atan2(sin(slopes[j]-slopeCut), cos(slopes[j]-slopeCut)) < 0) {  # This edge goes past the threshold
            print(j)
            modParcel[[i]][,3] <- 0
            modParcel[[i]][1:j,3] <- 1
            break
          }
          # If no edge goes past the threshold, don't need to change anything
        }
      }
      else{
        print("slopeDiff >= 0")
        # delta <- min(max(slopeDiff * 0.5, pi/6), 70/180*pi)
        delta <- slopeDiff/abs(slopeDiff)*20/180*pi           # Delta is 20 degrees
        slopeCut <- atan2(sin(slopes[1] + delta), cos(slopes[1] + delta))
        for (j in 1:length(slopes)){
          if (atan2(sin(slopes[j]-slopeCut), cos(slopes[j]-slopeCut)) > 0) {   # This edge goes past the threshold
            print(j)
            modParcel[[i]][,3] <- 0
            modParcel[[i]][1:j,3] <- 1
            break
          }
          # If no edge goes past the threshold, don't need to change anything
        }
      }
    }
    # Case 2: Last point is closer. Starting from last edge, grad all edges going up to the bisecting slope. If slope difference is less than 90 degrees, go up to 45 degrees
    else{
      orderRead[i] <- "Last < First"
      slopeDiff <- atan2(sin(slopes[1] - slopes[length(slopes)]), cos(slopes[1] - slopes[length(slopes)]))
      slopeDiffs[i] <- slopeDiff
      if (slopeDiff < 0){
        print("slopeDiff < 0")
        # delta <- max(min(slopeDiff * 0.5, -pi/6), -70/180*pi)  # Add a bit more than half, just as a buffer
        delta <- slopeDiff/abs(slopeDiff)*20/180*pi            # Delta is 20 degrees
        slopeCut <- atan2(sin(slopes[length(slopes)] + delta), cos(slopes[length(slopes)] + delta))
        for (j in length(slopes):1){
          if (atan2(sin(slopes[j]-slopeCut), cos(slopes[j]-slopeCut)) < 0){
            print(j)
            modParcel[[i]][,3] <- 0
            modParcel[[i]][(j+1):(length(slopes)+1),3] <- 1
            break
          }
          # If no edge goes past the threshold, don't need to change anything
        }
      }
      else{
        print("slopeDiff >= 0")
        # delta <- min(max(slopeDiff * 0.5, pi/6), 70/180*pi)
        delta <- slopeDiff/abs(slopeDiff)*20/180*pi            # Delta is 20 degrees
        slopeCut <- atan2(sin(slopes[length(slopes)] + delta), cos(slopes[length(slopes)] + delta))
        for (j in length(slopes):1){
          if (atan2(sin(slopes[j]-slopeCut), cos(slopes[j]-slopeCut)) > 0){
            print(j)
            modParcel[[i]][,3] <- 0
            modParcel[[i]][(j+1):(length(slopes)+1),3] <- 1
            break
          }
          # If no edge goes past the threshold, don't need to change anything
        }
      }
    }
  }
}


# Flag corner lots where front points cover X% of total parcel area

flagCorner <- function(index){
  # Make sure this is marked as a potential corner lot
  # if(is.null(cornerStats[[index]])){
    # print("This is not marked as a potential corner lot")
    # return(NULL)}
  par <- modParcel[[index]][,1:2]
  front <- modParcel[[index]][(modParcel[[index]][,3] == 1),1:2]
  sf_par <- st_as_sf(SpatialPolygons(list(Polygons(list(Polygon(par)),1))))
  sf_front <- st_as_sf(SpatialPolygons(list(Polygons(list(Polygon(front)),1))))
  prop <- st_area(sf_front)/st_area(sf_par)
  return(prop)
}

propFront <- vector("double", length(parcel))

for(i in 1:length(parcel)){
  print(i)
  if (!misfits[i]){
    propFront[i] <- flagCorner(i)
  }
}

## Check out the distribution of proportions. Figure out a cutoff value for corner parcels
## that may not have front points extracted properly. Higher values = more concerning
tibble(val = propFront) %>%
  filter(val != 0) %>%    # Most values are 0%. Filter those out to look at relevant values
  ggplot(., aes(x = val)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.02) # +
  scale_x_continuous(breaks = seq(0, 0.5, 0.02)
    # , limits = c(90, 0)
  )
#
tibble(val = propFront) %>%
  filter(val != 0) %>%    # Most values are 0%. Filter those out to look at relevant values
  ggplot(., aes(x = val)) +
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..)), binwidth = 0.02) +
  scale_x_reverse(breaks = seq(0, 0.5, 0.02)
                     # , limits = c(90, 0)
  )
#
# sum(propFront > 0.1)
# sum(propFront > 0.25)

# Update misfits. Any parcels with proportion > 10% will be flagged for manual review
for (i in 1:length(parcel)){
  if(propFront[i] > 0.25){misfits[i] <- TRUE}
}

sum(misfits)
################################################################
save.image("C2.RData")
################################################################

# # Take a look at the roads
# troubleshootRoad <- function(index){
#   eqscplot(roadway[[index]], type='l')
#   points(closestRoad[[index]], pch = 19)
#   points(roadway[[index]], pch = 1)
#   lines(parcel[[index]])
# }
#
# # Check if the modified parcel fronts are good without edits
# for (i in 1:length(parcel)){
#   if (isCorner[i]){
#     print(i)
#     eqscplot(parcel[[i]],type='l', tol=0.9)
#     points(parcelFront(i), pch = 1)
#     text(parcelFront(i), labels = row(parcelFront(i)), pos = 4, offset = 1)
#     points(modParcel[[i]], pch = 16)
#     points(closestRoad[[i]])
#     lines(roadway[[i]])
#     # Sys.sleep(0.1)  # Pause and continues automatically
#     invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
#   }
# }
#
# # Manual check whether the modified parcel fronts are good, and adjust them manually
# modIndexFront <- vector("list", length(parcel))
# for (i in 1:length(parcel)){
#   if (isCorner[i]){
#
#     # Plot parcel and points
#     checkCorner(i)
#
#     # User will manually check points
#     correction <- readline(prompt="Are the green ones ID'd correctly? Enter index/indices of points that need to be switched (space in between each number):")
#     correction <- unlist(strsplit(correction, split=" "))
#
#
#     # Get the modified parcel indices for front from the function
#     modIndexFront[[i]] <- vector("logical", dim(parcel[[i]])[1]-1)
#     for (j in 1:(dim(parcel[[i]])[1]-1)){
#       print(j)
#       for (k in 1:dim(modParcel[[i]])[1]){
#         if (abs(parcel[[i]][j,1]-modParcel[[i]][k,1]) < 0.001 && abs(parcel[[i]][j,2]-modParcel[[i]][k,2]) < 0.0001){
#           modIndexFront[[i]][j] <- TRUE
#           break
#         }
#       }
#     }
#
#     if(length(correction) > 0){
#       for (j in 1:length(correction)){
#         index <- correction[j]
#         modIndexFront[[i]][j] <- !modIndexFront[[i]][j]
#       }
#     }
#
#     # invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
#   }
# }



# Read in building footprints
bldgs <- read_sf(dsn = "epabldg", layer = "epabldgs_byAPN_redo") %>% st_transform(., crs = 102643) %>% arrange(APN)
# Clean up buildings. For each parcel, clip the buildings to parcel (and if there are multiple buildings, take largest building)
bldg_all <- NULL
for (i in 1:nrow(parcels)){
  print(i)
  index <- match(parcels$APN[i], bldgs$APN)
  # No building available, add an empty polygon
  if (is.na(index)){
    # print(i)
    sf <- st_sf(APN = apn, geometry = st_sfc(st_polygon(list()), crs = 102643))
    misfits[i] <- TRUE
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

# # Function to grab largest building on each parcel
# largestBldg <- function(bldg){
#   bldg <- st_sf(bldg)
#   bldg %>%
#     st_cast("POLYGON") %>%
#     mutate(order = (order(st_area(.), decreasing = TRUE))) %>%
#     filter(order == 1) %>%
#     select(geometry)
# }

# 4 mins to complete
# bldg <- NULL
# system.time({
#   for (i in 1:nrow(bldgs)){
#     print(i)
#     bldgi <- bldgs[i,]
#     result <- suppressWarnings(largestBldg(bldgi))
#     bldg <- rbind(bldg, result)
#   }
# })
# bldg <- bldgs %>% st_set_geometry(NULL) %>% bind_cols(bldg)
# # Add in APNs with no buildings. Geometry is an empty list
# bldg_all <- bldg %>% right_join(tibble(parcels$properties.APN), by = c("APN" = "parcels$properties.APN")) %>% st_sf()
# sum(misfits)
# for (i in 1:length(parcel)){
#   if (is.na(st_dimension(bldg_all[i,]))){   # If there is no building, add to misfits
#     misfits[i] <- TRUE
#   }
# }
sum(misfits)

checkCorner <- function(i){
  # Convert matrices to tibbles
  tblPar <- as_tibble(parcel[[i]])
  tblFront <- as_tibble(parcelFront(i))
  tblModFront <- as_tibble(modParcel[[i]])
  tblClosestRoad <- as_tibble(closestRoad[[i]])
  tblRoadway <- as_tibble(roadway[[i]])
  tblBldg <- as_tibble(st_coordinates(bldg_all[i,])[,1:2])

  # Get XY limits of parcel
  parXMin <- min(tblPar[[1]])
  parXMax <- max(tblPar[[1]])
  parXCent <- mean(parXMin, parXMax)
  parYMin <- min(tblPar[[2]])
  parYMax <- max(tblPar[[2]])
  parYCent <- mean(parYMin, parYMax)
  zoom <- max(parXMax-parXMin, parYMax-parYMin)

  # Adjust the scope of the plot
  XMin <- parXCent - 1.5 * zoom
  XMax <- parXCent + 1.5 * zoom
  YMin <- parYCent - 1.5 * zoom
  YMax <- parYCent + 1.5 * zoom

  # Plot the parcel and points and roadway for user to check
  print(
    ggplot() +
      geom_polygon(data = tblPar, aes(V1, V2), color = 'gray') +
      geom_polygon(data = tblBldg, aes(X, Y), color = 'white') +
      geom_point(data = tblFront, aes(V1, V2), color = 'red', size = 3) +
      geom_point(data = tblModFront %>% filter(V3 == 1), aes(V1, V2), color = 'green', size = 3) +
      geom_line(data = tblRoadway, aes(V1, V2)) +
      geom_line(data = tblClosestRoad, aes(V1, V2), color = 'blue', size = 3) +
      geom_label_repel(data = tblFront, aes(V1, V2, label = rownames(tblFront))) +
      coord_cartesian(xlim = c(XMin, XMax), ylim = c(YMin, YMax))
  )
}


# Function
# ID parcel sides (front, side, back)
# Front edges are in between front points
# Back edge is furthest away (true distance) from front centroid
# If edges somehow tie, select edge that has smallest slope difference
# If 2 edges somehow tie, select randomly between edges. Or if more than two edges, furthest perpendicular distance?
# Inputs: parcel, indexFront
# Outputs: string vector that returns "front/side/rear" for each corresponding edge.
# Edge 1 = point1 to point2, edge 2 = point2 to point3, etc. Length of vector is length(parcel) - 1.
idEdges <- function(index){
  par <- modParcel[[index]]
  edges <- par[-nrow(par),3]
  # print(edges)
  # sides[(sides[])] <- "Front"
  # Find average of front
  if (sum(edges) == 0){
    return (NULL)
  }
  else if (sum(edges) == 1){
    centF <- parcelFront(index, "Mod")
  } else{
    centF <- colMeans(parcelFront(index, "Mod"))
  }
  maxDist <- 0
  maxInd <- NULL
  for (i in 1:(nrow(par)-1)){
    # Find furthest edge (using midpoint)
    if (par[i,3] == 0){       # Test only if not a front edge
      edge <- (par[i,] + par[i + 1, ]) / 2
      dist <- (edge[1]-centF[1])*(edge[1]-centF[1]) + (edge[2]-centF[2])*(edge[2]-centF[2])
      # Case 1: further distance, assign new furthest edge
      if (dist > maxDist){
        maxDist <- dist
        maxInd <- i
      }
      # Case 2: distance matches, keep all furthest edges ###### Pick the longest edge
      else if (dist == maxDist){
        print(paste("Tied! Index = ", i))
        maxInd <- c(maxInd, i)
      }
      # Case 3: distance is shorter. Keep moving.
    }
    ############### Identify front edge. Only front edge if the next point is also a front point
    if (par[i,3] == 1 && par[i+1,3] == 1){
      edges[i] <- "Front"
    }
  }
  # print(maxInd)
  edges[maxInd] <- "Rear"
  # Anything not yet marked is a side edge
  edges[(edges == 1)] <- "Side"
  edges[(edges == 0)] <- "Side"
  return(edges)
}

# Run idEdges on all parcels
parcelEdges <- vector("list", length(parcel))
for (i in 1:length(parcel)){
  print(i)
  if (misfits[i]){next}
  # print(i)
  parcelEdges[[i]] <- idEdges(i)
}

# # Index buildings by APN ascending. NULL for APNs with no buildings
# bldg <- data.frame(dim = c(length(parcel),2))
# for (i in 1:length(parcel)){
#   apn <- parcels$properties.APN[[i]]
#   bldg[i,] <- bldgs[match(apn, bldgs$APN), c("APN", "geometry")]
#   # if (!is.null(bldg[[i]])){      # If not, there is a building
#   #   if (length(bldg[[i]] == 1)){  # If there is one building, convert from MULTIPOLYGON to POLYGON
#   #     bldg[[i]] <- st_cast(bldg[[i]], "POLYGON")
#   #   }
#   #   else{
#   #
#   #   }
#   # }
# }
# test <- st_read(file_bldg)

# Cut out front yard
# Input: parcel, building_data, parcelFront
# Use first and last front edge points to draw a line
# Find point or edge on building that is closest (perpendicular distance)
# Output: st_polygon buffered to cover front yard
removeFront <- function(par, bldg, front){
  p_i <- front[1,]
  p_f <- front[dim(front)[1],]
  minIndex <- 0   # track index for closest point to line
  minDist <- 1e99 # track associated minimum distance
  for (i in 1:dim(bldg)[1]){
    dist <- abs(perpDist(bldg[i,1], bldg[i,2], p_i[1], p_i[2], p_f[1], p_f[2]))
    if (dist < minDist){
      minIndex <- i
      minDist <- dist
    }
  }
  front_seg <- st_sfc(st_linestring(rbind(p_i + 1000 * (p_f - p_i), p_i - 1000 * (p_f - p_i))), crs = 102643)
  front_yard <- st_buffer(front_seg,minDist,endCapStyle = "SQUARE")
  return(list(front_yard, minDist))
}

# Get histogram of distances between front of building and parcel edge
frontDist <- vector("double", length(parcel))
for (i in 1:length(parcel)){
  print(i)
  if (st_is_empty(bldg_all[i,])){
    misfits[i] <- TRUE
    next
  }
  if (!misfits[i]){
    par <- parcel[[i]][,1:2]
    bldg <- st_coordinates(bldg_all[i,])[,1:2]
    front <- parcelFront(i, "Mod")
    frontDist[i] <- removeFront(par, bldg, front)[[2]]
  }
}

# tibble(val = frontDist) %>%
#   filter(val != 0) %>%
#   ggplot(., aes(x = val)) +
#   # geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 18)
#   geom_histogram(aes(), binwidth = 18)
# # Roughly 16% of parcels (600) have a front dist < 18 ft

###############################################
save.image("D2.RData")
###############################################


# Helper Function: buffer for one edge
# Input: parcel
# Param: edge_index, buffer distance
# Returns a buffer around the edge with distance dist
# Returns as a st_polygon
buffer <- function(par, ind, dist){
  edge <- par[ind:(ind+1),]      # index and next point
  # Convert coordinates into st_linestring
  line <- st_sfc(st_linestring(edge), crs = 102643)
  
  # Buffer line seg by dist
  buffered <- st_buffer(line, dist, nQuadSegs = 2, endCapStyle = "SQUARE") %>% st_make_valid()
  return(buffered)
}


# Side and rear buffers
# Input: parcel, edgeID (front, side, rear), building footprint, front points
# Param: side buffer dist, rear buffer dist
# Draw parallel lines that are [buffer dist] away from each side
# Select parallel lines that are closer to parcel centroid, so lines are going inward, not outward
# Find intersection between adjacent lines to reconstruct buffered parcel
# Check whether buffer overlaps with building. If it does, adjust so that there is no intersection in result
# Output: new parcel coordinates, array (n by 2)
allBuffers <- function(par, edges, bldg, front, side_dist, rear_dist, bldg_dist = 0){
  # Create sf objects for par and bldg
  st_par <- st_sfc(st_polygon(list(par)), crs = 102643)
  st_par <- st_snap(st_par, st_par, tolerance = 1e-1) %>% st_set_precision(1e9)
  
  # Bldg already in st_polygon form
  st_bldg <- bldg %>% st_set_precision(1e9)
  st_bldg <- st_snap(st_bldg, st_bldg, tolerance = 1e-1) %>% st_set_precision(1e9)
  bldg <- st_coordinates(st_set_precision(st_bldg,1e9))[,1:2]
  
  # Remove building footprint from parcel
  bldg_dist <- 0
  result <- st_difference(st_buffer(st_par,0) %>% st_set_precision(1e9), st_buffer(st_bldg, bldg_dist,nQuadSegs = 2, endCapStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9)) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_make_valid() %>% st_set_precision(1e9)
  # result <- st_intersection(result, result)
  
  # Remove front yard
  front_yard <- removeFront(par, bldg, front)[[1]] %>% st_set_precision(1e9)
  result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), front_yard) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
  result <- st_snap(result, result, tolerance = 1e-3) %>% st_set_precision(1e9)
  for (i in 1:length(edges)){
    # i <- 4
    # print(i)
    if (edges[i] == "Front"){next}
    else if (edges[i] == "Side"){
      buff <- buffer(par, i, side_dist) %>% st_set_precision(1e9)
      # result <- st_intersection(result, result)
      result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), buff) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
    }
    else if (edges[i] == "Rear"){
      buff <- buffer(par, i, rear_dist) %>% st_set_precision(1e9)
      # result <- st_intersection(result, result)
      result <- st_difference(st_buffer(result, 0) %>% st_set_precision(1e9), buff) %>% st_buffer(-1e-9, joinStyle = "MITRE", mitreLimit = 5) %>% st_set_precision(1e9) %>% st_make_valid() %>% st_set_precision(1e9)
    }
  }
  
  return (result)
  
}


# Find the available area after buffers. No building set back. Side = 5 ft, Rear = 10 ft
result_Bldg0 <- NULL
# need to fix 905, 3458, 3539, 3869
# misfits[905] <- TRUE
# misfits[2678] <- TRUE
# misfits[3458] <- TRUE
# misfits[3480] <- TRUE
# misfits[3539] <- TRUE
# misfits[3612] <- TRUE
# misfits[3869] <- TRUE

# 3339
for (i in 1:length(parcel)){
  if (!misfits[i]){
    # i <- 1
    print(i)
    par <- parcel[[i]][,1:2]
    edges <- parcelEdges[[i]]
    bldg <- st_geometry(bldg_all[i,]) %>% st_buffer(0)
    # bldg <- st_coordinates(bldg_all[i,])[,1:2]
    front <- parcelFront(i, "Mod")
    side_dist <- 5
    rear_dist <- 10
    geom <- allBuffers(par, edges, bldg, front, side_dist, rear_dist, bldg_dist = 0)
    # if (is.character(geom) || nrow(geom) == 0){
    #   sf <- st_sf(APN = parcels$properties.APN[[i]], valid = FALSE, geometry = st_sfc(st_polygon()))
    # }
    if (length(geom) == 0){
      sf <- st_sf(APN = parcels$APN[i], geometry = st_sfc(st_polygon(), crs = 102643))
    }
    else{
      sf <- st_sf(APN = parcels$APN[i], geometry = geom)
    }
    result_Bldg0 <- rbind(result_Bldg0, sf)
  }
  else{
    sf <- st_sf(APN = parcels$APN[i], geometry = st_sfc(st_polygon(), crs = 102643))
    result_Bldg0 <- rbind(result_Bldg0, sf)
  }
}

# for (i in 1:length(parcel)){
#   if (!misfits[i]){
#     eqscplot(st_coordinates(result_Bldg0[i,]), type='l')
#     invisible(readline(prompt="Press [enter] to continue"))
#   }
# }

############################################
save.image("E2.RData")
############################################

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
# # misfits identified on result_Bldg0
# for (i in 1:nrow(result_Bldg0)){
#   result_Bldg0$misfit[i] <- misfits[which(result_Bldg0$APN[i] == misfits, TRUE)[1],1]
# }

result_Bldg0$valid <- st_is_valid(result_Bldg0)
result_Bldg0$xmin <- 0
result_Bldg0$ymin <- 0

# Get XMin and YMin and shift coordinates (avoids numerical errors)
buildable <- result_Bldg0
buildable$xmin <- 0
buildable$ymin <- 0
for (i in 1:nrow(buildable)){
  if (st_geometry_type(result_Bldg0[i,]) == "GEOMETRYCOLLECTION"){
    buildable$valid[i] <- FALSE
    next
  }
  if (st_is_empty(result_Bldg0[i,])){
    buildable$valid[i] <- FALSE
    next
  }
  print(i)
  coord <- st_coordinates(buildable[i,])[,1:2]
  buildable$xmin[i] <- min(coord[,1])
  buildable$ymin[i] <- min(coord[,2])
  st_geometry(buildable[i,]) <- st_geometry(buildable[i,]) - c(buildable$xmin[i], buildable$ymin[i])
}
beep(3)
############################################
save.image("F2.RData")
############################################

# Apply minimum 8 x 20 area to get rid of unviable spaces
# Merge the possible rectangles together
buildable_adu <- NULL

# 604
for (i in 1:nrow(buildable)){
  if (i == 627 || i == 790 || i == 1104 || i == 1120 || i == 2350 || i == 2463 || i == 2496 || i == 2524 || i == 2577 || i == 2696){
    sf <- st_sf(APN = buildable$APN[i], message = "Invalid geometry", geometry = st_sfc(st_polygon(), crs = 102643))
    buildable_adu <- rbind(buildable_adu, sf)
    next
  }
  if (buildable$valid[i] == FALSE){
    sf <- st_sf(APN = buildable$APN[i], message = "Invalid geometry", geometry = st_sfc(st_polygon(), crs = 102643))
    buildable_adu <- rbind(buildable_adu, sf)
    next
  }
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
    sf <- st_sf(APN = buildable$APN[i], message = msg, geometry = st_sfc(st_polygon(), crs = 102643))
  }
  else{
    sf <- st_sf(APN = buildable$APN[i], message = "Success", geometry = result)
  }
  buildable_adu <- rbind(buildable_adu, sf)
}


##### Work in Progress
# test <- largestRect(result_Bldg0[3551,], print = TRUE)
# 
# for(i in 1:(length(test)-1)){
#   if(test[i] == test[i+1]){
#     print(test[i])
#   }
# }


buildable_adu_no_offset <- buildable_adu
# Return geometries to original coordinates, no offsets
for(i in 1:nrow(buildable_adu_no_offset)){
  print(i)
  offset <- c(buildable$xmin[i], buildable$ymin[i])
  st_geometry(buildable_adu_no_offset[i,]) <- st_geometry(buildable_adu_no_offset[i,]) + offset
  buildable_adu_no_offset[i,] <- fill_holes(buildable_adu_no_offset[i,], 1e5)
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

Parcels <- parcels
ExistingBuilding <- bldg_all
AttachedADU <- buildable_adu_no_offset %>% filter(message == "Success")

map <- mapview(Parcels, alpha.regions = 0, legend = FALSE) + mapview(ExistingBuilding, col.regions = "grey", legend = FALSE) + mapview(AttachedADU, col.regions = "green", legend = TRUE)
map
mapshot(map, "attached_adu.html")




# https://github.com/r-spatial/sf/issues/290

result_Bldg0_combined <- result_Bldg0 %>% filter(inBox == TRUE)
result_Bldg0_combined <- st_sf(APN = result_Bldg0_combined$APN %>% unique(), geometry = result_Bldg0_combined %>% split(.$APN) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())
bldg0_buildable_combine <- st_sf(APN = bldg0_buildable$APN %>% unique(), geometry = bldg0_buildable %>% split(.$APN) %>% lapply(st_union) %>% do.call(c, .) %>% st_cast())

EPAbbox <- st_bbox(st_combine(bldg_all)) + c(-1000, -1000, 1000, 1000)

# Check which result_Bldg0 results need to be fixed. Filter out the ones that show up outside of the EPAbbox
test <- result_Bldg0
result_Bldg0$inBox <- FALSE
for (i in 1:nrow(result_Bldg0)){
  print(i)
  bbox <- st_bbox(result_Bldg0[i,])
  if (is.na(bbox[1])){next}
  if (bbox[1] >= EPAbbox[1] && bbox[2] >= EPAbbox[2] && bbox[3] <= EPAbbox[3] && bbox[4] <= EPAbbox[4]){
    result_Bldg0$inBox[i] = TRUE
  }
}

mapview(result_Bldg0_combined)

for i in 1:nrow(bldg0_buildable_combine){
  offset
}

# The function will plot the total buildable area in green, and the result in a transparent red
# The result will look like a brown-green

#########EXPLORE HERE#################################


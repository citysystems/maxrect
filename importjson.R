### Only run this once to install maxrectangle
# devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)
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

### Note: In order for the find_lr function to work as we intend (pulling all possible buildable area), we need to change the largestRect.coffee in the R library "3.5/maxrectangle/js" folder

# To save current environment
# save.image(".RData")

### parcels
file_parcel <- "epaparcels_clean_NAD83.geojson"
parcels <- fromJSON(txt=file_parcel, flatten = TRUE, simplifyDataFrame = TRUE)
parcels <- parcels$features
# Filter down to 3920 SFH parcels
file_apn <- "apn.csv"
apn <- read_csv(file_apn)
apn <- paste( "0", apn$apn, sep = "")
parcels <- dplyr::filter(parcels, properties.APN %in% apn)
### Join parcels to their address streets
file_street <- "apn_street.csv"
street <- read_csv(file_street)
street$apn <- paste0( "0", street$apn)
parcels <- parcels %>% inner_join(street, by = c("properties.APN" = "apn"))
ord <- sort.list(parcels$properties.APN)
parcels <- parcels[ord,]
numPar <- nrow(parcels)
rownames(parcels) <- 1:numPar
parcel <- parcels$geometry.coordinates

### roads
file_roads <- "epa_roads_adj_NAD83.geojson"
roads <- fromJSON(txt=file_roads, flatten = TRUE, simplifyDataFrame = TRUE)
roads <- roads$features


# Initialize column for storing road segments
parcels[,"list_roads"] <- NA
for (i in 1:numPar){
  parcels$list_roads[i] <- list(roads %>% filter(toupper(properties.FULLNAME) == toupper(parcels$address[[i]])))
}


### coordinates of front
file_blocks <- "front_vertices_cleaned_NAD83.geojson"
blocks <- fromJSON(txt=file_blocks, flatten = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
blocks <- blocks$features$geometry.coordinates
blockPts <- array(data = 0, dim = c(length(blocks), 2))
for(i in 1:length(blocks)){
  if(is.null(blocks[[i]]) || typeof(blocks[[i]]) != "double"){next}
  blockPts[i,1] <- blocks[[i]][1]
  blockPts[i,2] <- blocks[[i]][2]
}
blockPts <- blockPts[blockPts[,1] != 0,]
blockPts <- blockPts[order( blockPts[,1], blockPts[,2]),]
blockPts <- unique(blockPts)

######## Rdata of all the raw data
##########################################
save.image("A.RData")
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
  index <- findInterval(point[1], blockPts[,1])  # This index may be +/- 1 from the actual value
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

# Remove collinear points from parcels
for (i in 1:numPar){
  arr <- drop(parcel[[i]])
  parcel[[i]] <- removeCollinear(arr)
}

# Find number of landlocked parcels (all points are touching another parcel)
# First, pull all the coordinates for the first polygon for each parcel
checkFront <- vector("logical", numPar)   # Keep track of whether a parcel touches the front
numFront <- vector("double", numPar)      # Keep track of number of points that touch the front
# indexFront <- vector("list", numPar)      # Keep track of index of points that touch the front
for (i in 1:numPar){
  print(i)
  if (typeof(parcel[[i]]) == "list"){
    poly <- unique(drop(parcel[[i]][[1]]))
  }
  else if (typeof(parcel[[i]]) == "double"){
    # if (length(dim(parcel[[i]])) > 3){
      # poly <- drop(parcel[[i]])[1,,]
      poly <- unique(drop(parcel[[i]]))
    # }
    # else{poly <- unique(parcel[[i]][1,,])}
  }
  else{
    break
  }
  # poly <- rbind(poly, poly[1,])
  parcel[[i]] <- cbind(poly, vector(length = dim(poly)[1]))
  for (j in 1:dim(poly)[1]){
    if (isFront(poly[j,])){
      checkFront[i] <- TRUE
      parcel[[i]][j,3] <- TRUE
      numFront[i] <- numFront[i] + 1
    }
  }
}

# sum(checkFront)
# table(numFront)

# Front indices may be out of order if the polygon starting point is in the middle of the front
chunks <- vector("double", numPar)              # chunks of points marked as front
false_chunks <- vector("double", numPar)        # chunks of points not marked as front (used to ID problems)
# offsetFront <- vector("double", numPar)         # what number of points to move to the back
# deleteFront <- vector("double", numPar)         # what index point to remove
chunks2 <- vector("double", numPar)              # chunks of points marked as front
false_chunks2 <- vector("double", numPar)        # chunks of points not marked as front (used to ID problems)
chunks3 <- vector("double", numPar)              # chunks of points marked as front
false_chunks3 <- vector("double", numPar)        # chunks of points not marked as front (used to ID problems)
for (i in 1:numPar){
  print(i)
  preVal <- FALSE
  arr <- parcel[[i]]
  # print(parcel[[i]])
  # print(arr)
  for (j in 1:dim(arr)[1]){
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
  parcel[[i]][dim(parcel[[i]])[1],3] <- 0
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
misfits <- vector("logical", numPar)

# Check if front edge is properly extracted
for (i in 1:numPar){
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
cornerStats <- vector(mode = "list", numPar)
for (i in 1:numPar){
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
segDiff <- array(dim = c(numPar,2))
for (i in 1:numPar){
  if (is.null(cornerStats[[i]])) {
    segDiff[i,1] <- NA
  }
  else {
    segDiff[i,1] <- abs(cornerStats[[i]]$segdiff)/pi*180
  }
}
segDiff[,2] <- 1:numPar
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
totDiff <- array(dim = c(numPar,2))
for (i in 1:numPar){
  if (is.null(cornerStats[[i]])) {
    totDiff[i,1] <- NA
  }
  else {
    totDiff[i,1] <- abs(min(cornerStats[[i]]$totdiff1, cornerStats[[i]]$totdiff2))/pi*180
  }
}
totDiff[,2] <- 1:numPar
colnames(totDiff) <- c("totdiff", "index")
totDiff <- na.omit(totDiff)

tibble(val = totDiff[,1]) %>%
  ggplot(., aes(x = val)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 5) +
  scale_y_continuous(labels = scales::percent) #+
  # scale_x_continuous(limits = c(0, 90))


isCorner <- vector("logical", numPar)
for (i in 1:numPar){
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
save.image("B.RData")
########################################################

# Extract front edge for corner lot
# Read in roadway network. Find two closest roadway points to parcel centroid
# Of the two ends of the front, choose the point shortest normal dist to roadway
# Select edges that are within a certain angle of the segment on this end
modParcel <- parcel
roadway <- vector("list", numPar)
closestRoad <- vector("list", numPar)
slopeDiffs <- vector("double", numPar)
orderRead <- vector("character", numPar)
for (i in 1:numPar){
  print(i)
  par_cent <- colMeans(drop(parcel[[i]]))      # Get the parcel centroid
  # Extract associated road
  roads <- parcels$list_roads[[i]]$geometry.coordinates     # Get the list
  # Find two closest points on roadway network to parcel centroid
  minDist1 <- 1e99
  minDist2 <- 1e99
  point1 <- vector(length = 2)
  point2 <- vector(length = 2)
  road_arr <- array(dim = c(0,2))
  for (j in 1:length(roads)){
    if (typeof(roads[[j]]) == "list"){
      for (k in 1:length(roads[[j]])){
        road_arr <- rbind(road_arr, drop(roads[[j]][[k]]))
      }
    } else{
      road_arr <- rbind(road_arr, drop(roads[[j]]))
    }
  }
  road_arr <- unique(road_arr)
  roadway[[i]] <- road_arr
  dist <- vector(length = dim(road_arr)[1])
  for (j in 1:dim(road_arr)[1]){
    dist[j] <- ((par_cent[1]-road_arr[j,1])/100000)^2 + ((par_cent[2]-road_arr[j,2])/100000)^2
  }
  # print(dist)
  ord <- order(dist)
  # print(ord)
  road_arr <- road_arr[ord,]
  point1 <- road_arr[1,]
  point2 <- road_arr[2,]

  closestRoad[[i]] <- rbind(point1,point2)
  # We now have the slope of the roadway
  road_slo <- (atan2(point2[2]-point1[2],point2[1]-point1[1]) + 2*pi) %% 2*pi
  # print(road_slo)

  if (!is.null(cornerStats[[i]])){  # Only modify if it is marked as a corner parcel

    # Compare first and last front point. ID which one has shortest perp dist to roadway.
    first <- parcelFront(i)[1,]
    last <- parcelFront(i)[dim(parcelFront(i))[1],]
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
        delta <- max(min(slopeDiff * 0.5, -pi/6), -70/180*pi)  # Add a bit more than half, just as a buffer. Range must be between 45-70 degrees
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
        delta <- min(max(slopeDiff * 0.5, pi/6), 70/180*pi)
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
        delta <- max(min(slopeDiff * 0.5, -pi/6), -70/180*pi)  # Add a bit more than half, just as a buffer
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
        delta <- min(max(slopeDiff * 0.5, pi/6), 70/180*pi)
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

propFront <- vector("double", numPar)

for(i in 1:numPar){
  print(i)
  if (!misfits[i]){
    propFront[i] <- flagCorner(i)
  }
}

## Check out the distribution of proportions. Figure out a cutoff value for corner parcels
## that may not have front points extracted properly. Higher values = more concerning
# tibble(val = propFront) %>%
#   filter(val != 0) %>%    # Most values are 0%. Filter those out to look at relevant values
#   ggplot(., aes(x = val)) +
#   geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 0.02) # +
#   scale_x_continuous(breaks = seq(0, 0.5, 0.02)
#     # , limits = c(90, 0)
#   )
#
# tibble(val = propFront) %>%
#   filter(val != 0) %>%    # Most values are 0%. Filter those out to look at relevant values
#   ggplot(., aes(x = val)) +
#   geom_histogram(aes(y = cumsum(..count..)/sum(..count..)), binwidth = 0.02) +
#   scale_x_reverse(breaks = seq(0, 0.5, 0.02)
#                      # , limits = c(90, 0)
#   )
#
# sum(propFront > 0.1)
# sum(propFront > 0.25)

# Update misfits. Any parcels with proportion > 10% will be flagged for manual review
for (i in 1:numPar){
  if(propFront[i] > 0.25){misfits[i] <- TRUE}
}

sum(misfits)
################################################################
save.image("C.RData")
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
# for (i in 1:numPar){
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
# modIndexFront <- vector("list", numPar)
# for (i in 1:numPar){
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
file_bldg <- "epabldgsSFH_byAPN_PF_NAD83.geojson"
bldgs <- read_sf(file_bldg) %>% select(APN, geometry)
bldgs <- bldgs %>% st_set_crs(102643)%>% arrange(APN)

# Function to grab largest building on each parcel
largestBldg <- function(bldg){
  bldg <- st_sf(bldg)
  bldg %>%
    st_cast("POLYGON") %>%
    mutate(order = (order(st_area(.), decreasing = TRUE))) %>%
    filter(order == 1) %>%
    select(geometry)
}

# 4 mins to complete
bldg <- NULL
system.time({
  for (i in 1:nrow(bldgs)){
    print(i)
    bldgi <- bldgs[i,]
    result <- suppressWarnings(largestBldg(bldgi))
    bldg <- rbind(bldg, result)
  }
})
bldg <- bldgs %>% st_set_geometry(NULL) %>% bind_cols(bldg)
# Add in APNs with no buildings. Geometry is an empty list
bldg_all <- bldg %>% right_join(tibble(parcels$properties.APN), by = c("APN" = "parcels$properties.APN")) %>% st_sf()
sum(misfits)
for (i in 1:numPar){
  if (is.na(st_dimension(bldg_all[i,]))){   # If there is no building, add to misfits
    misfits[i] <- TRUE
  }
}
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
  edges <- par[-dim(par)[1],3]
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
  for (i in 1:(dim(par)[1]-1)){
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
    if (edges[i] == 1 && edges[i+1] == 1){
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
parcelEdges <- vector("list", numPar)
for (i in 1:numPar){
  if (misfits[i]){next}
  # print(i)
  parcelEdges[[i]] <- idEdges(i)
}

# # Index buildings by APN ascending. NULL for APNs with no buildings
# bldg <- data.frame(dim = c(numPar,2))
# for (i in 1:numPar){
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
# Find line equation for front edge of building, XXXX then find intersection points on parcel
# Output: point and vector for front cut XXX new parcel coordinates, array (n by 2)
################### Need to incorporate merge_rings to cut out bldg footprint
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
  # print(minIndex)
  # print(minDist)
  # Find parallel line
  # slope <- atan2((p_f-p_i)[2],(p_f-p_i)[1])
  pt <- bldg[minIndex,]
  r_slope <- p_f-p_i
  return (c(pt, r_slope, minDist))
}


# Get histogram of distances between front of building and parcel edge
frontDist <- vector("double", numPar)
for (i in 1:numPar){
  if (!misfits[i]){
    par <- parcel[[i]][,1:2]
    bldg <- st_coordinates(bldg_all[i,])[,1:2]
    front <- parcelFront(i, "Mod")
    frontDist[i] <- removeFront(par, bldg, front)[5]
  }
}

# tibble(val = frontDist) %>%
#   filter(val != 0) %>%
#   ggplot(., aes(x = val)) +
#   # geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 18)
#   geom_histogram(aes(), binwidth = 18)
# # Roughly 16% of parcels (600) have a front dist < 18 ft

###############################################
save.image("D.RData")
###############################################


# Helper Function: buffer for one edge
# Input: parcel
# Param: edge_index, buffer distance
# Draw two points [buffer dist] perpendicular from edge midpoint, choose the point that is inside polygon
# Draws parallel line that [buffer dist] away from the edge
# Returns a line for the edge (u + param * slope vector r) as a vector length 4 (ux, uy, rx, ry)
buffer <- function(par, ind, dist){
  edge <- par[ind:(ind+1),]      # index and next point
  # print(edge)
  edge_mp <- colMeans(edge)      # Edge midpoint
  # print(edge_mp)
  edge_slope <- atan2(par[ind+1,2]-par[ind,2], par[ind+1,1]-par[ind,1])
  # print(edge_slope)
  offset <- dist * c(cos(edge_slope + pi/2), sin(edge_slope + pi/2))
  # print(offset)
  opt1 <- edge_mp + offset
  # print(opt1)
  opt2 <- edge_mp - offset
  # print(opt2)
  if (point.in.polygon(opt1[1],opt1[2],par[,1],par[,2]) == 1){ # If opt1 is inside polygon
    vec <- c(cos(edge_slope), sin(edge_slope))
    return (c(opt1, vec))
  } else if (point.in.polygon(opt2[1],opt2[2],par[,1],par[,2]) == 1){ # If opt2 is inside polygon
    vec <- c(cos(edge_slope), sin(edge_slope))
    return (c(opt2, vec))
  } else{
    print(paste("Buffer does not work for this polygon, index = ", ind))
  }
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
  buffers <- array(dim = c(0, 4))
  skipFront <- FALSE               # Keep track of whether front cut was already added. If so, skip other front edges
  # Find the lines for all the buffers
  for (i in 1:length(edges)){
    if (edges[i] == "Front"){
      if (skipFront){
        next
      }
      else{
        buffers <- rbind(buffers, removeFront(par, bldg, front)[1:4])
        skipFront <- TRUE
      }
    }
    else if (edges[i] == "Side"){
      buffers <- rbind(buffers, buffer(par, i, side_dist))
    }
    else if (edges[i] == "Rear"){
      buffers <- rbind(buffers, buffer(par, i, rear_dist))
    }
  }
  buffers <- rbind(buffers, buffers[1,])      # Wrap around for easier looping later
  # print(buffers)
  newPar <- array(dim = c(nrow(buffers), 2))
  for (i in 1:(nrow(buffers)-1)){
    # print(paste("Buffer", i))
    newPar[i,] <- intersectLines(buffers[i,],buffers[i+1,])
  }
  newPar[nrow(buffers),] <- newPar[1,]
  # print(newPar)
  # Check if new parcel has any intersections due to buffers overtaking edge(s)
  # Use st_is_valid() to check
  sf_newPar <- st_as_sf(SpatialPolygons(list(Polygons(list(Polygon(newPar)),1))))
  # print("test2")
  # Remove building parcel from new parcel
  sf_bldg <- st_as_sf(SpatialPolygons(list(Polygons(list(Polygon(bldg)),1))))
  sf_bldg <- st_buffer(sf_bldg, bldg_dist)
  # print("test3")

  # If there are intersections, this will split into multiple polygons. If not, nothing changes
  split <- st_cast(st_make_valid(sf_newPar), "POLYGON")
  split <- st_difference(split, sf_bldg)
  split <- st_cast(split, "POLYGON")
  # return(split)
  if (nrow(split) == 0){
    return ("No suitable polygons")
  }


  # Get the order of polygons by largest to smallest
  # ord <- order(st_area(split), decreasing = TRUE)
  split <- cbind(valid = vector("logical", length = nrow(split)), split)


  for (i in 1:nrow(split)){
    # inverted <- FALSE     # Track
    # poly <- split[ord[i],]

    # If polygon is not inverted, mark as valid
    # Check shortest distance between polygon centroid and each edge.
    cent <- st_coordinates(st_centroid(split[i,]))
    split$valid[i] <- TRUE
    for (j in 1:length(edges)){
      if (edges[j] == "Front") {next} # Skip front facing, since we don't know the distance
      else if (edges[j] == "Side") {dist <- side_dist}
      else {dist <- rear_dist}

      # distPtLineSeg <- function(x, y, x1, y1, x2, y2){
      # If the distance > respective buffer, not inverted. Skip to next polygon
      if (dist > distPtLineSeg(cent[1], cent[2], par[j,1], par[j,2], par[j+1,1], par[j+1,2])){
        split$valid[i] <- FALSE
        break
      }
    }
    # # If after checking all the sides, not marked as inverted, add this polygon
    # if (!inverted){
    #   # polys <- rbind(polys, poly)
    #   return(poly)
    # }
  }
  if (sum(split$valid) == 0){
    # print(split)
    return ("No suitable polygons")
  }
  else{
    # print(split)
    return(split)
  }
  #
  # if (length(polys) > 0){
  #
  # }
  # else{
    # return ("No suitable polygons")
  # }
}


# Find the available area after buffers. No building set back. Side = 5 ft, Rear = 10 ft
result_Bldg0 <- NULL
# need to fix 905, 3458, 3539, 3869
misfits[905] <- TRUE
misfits[2678] <- TRUE
misfits[3458] <- TRUE
misfits[3480] <- TRUE
misfits[3539] <- TRUE
misfits[3612] <- TRUE
misfits[3869] <- TRUE

for (i in 1:numPar){
  if (!misfits[i]){
    print(i)
    par <- parcel[[i]][,1:2]
    edges <- parcelEdges[[i]]
    bldg <- st_coordinates(bldg_all[i,])[,1:2]
    front <- parcelFront(i, "Mod")
    side_dist <- 5
    rear_dist <- 10
    geom <- allBuffers(par, edges, bldg, front, side_dist, rear_dist, bldg_dist = 0)
    if (is.character(geom) || nrow(geom) == 0){
      sf <- st_sf(APN = parcels$properties.APN[[i]], valid = FALSE, geometry = st_sfc(st_polygon()))
    }
    else{
      sf <- st_sf(data.frame(tibble(APN = parcels$properties.APN[[i]]),geom))
    }

    result_Bldg0 <- rbind(result_Bldg0, sf)
  }
}

for (i in 1:numPar){
  if (!misfits[i]){
    eqscplot(st_coordinates(result_Bldg0[i,]), type='l')
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

############################################
save.image("E.RData")
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

result_Bldg0$slopes[[1]] <- list()
result_Bldg0$valid <- TRUE
result_Bldg0$xmin <- 0
result_Bldg0$ymin <- 0

# Get the angle/slopes present in each bldg parcel
for (i in 1:nrow(result_Bldg0)){
  # Skip this one if the area is smaller than 160 SF (minimum area for ADU) or if there is no geometry
  if (st_area(result_Bldg0[i,]) < 160 || st_is_empty(result_Bldg0[i,])){
    result_Bldg0$valid[i] <- FALSE
    next
  }
  print(i)
  # Extract coordinates
  coord <- as_tibble(st_coordinates(result_Bldg0[i,]))
  # Separate out if there are holes
  poly1 <- (coord %>% filter(L1 == 1) %>% select(X, Y)) %>% unlist() %>% unname() %>% matrix(ncol = 2)
  poly2 <- NULL
  if (mean(coord$L1) > 1){
    poly2 <- (coord %>% filter(L1 == 2) %>% select(X, Y)) %>% unlist() %>% unname() %>% matrix(ncol = 2)
  }
  # Get the slopes for each edge
  slopes <- vector(length = nrow(poly1)-1)
  for (j in 1:(nrow(poly1)-1)){
    slopes[j] <- atan2((poly1[j+1,2]-poly1[j,2]),(poly1[j+1,1]-poly1[j,1]))
  }
  if (!is.null(poly2)){
    slopes2 <- vector(length = nrow(poly1)-1)
    for (j in 1:(nrow(poly2)-1)){
      slopes2[j] <- atan2((poly2[j+1,2]-poly2[j,2]),(poly2[j+1,1]-poly2[j,1]))
    }
    # Put slopes in one list
    slopes <- c(slopes, slopes2)
  }
  
  # Remove duplicates
  slopes <- slopes %>% sort() %>% unique() %>% list()
  
  
  # Add to result_Bldg0
  result_Bldg0$slopes[[i]] <- slopes
  
  # Store Xmin and Ymin and adjust geometry to relative coordinates
  result_Bldg0$xmin[i] <- st_bbox(result_Bldg0[i,])[1]
  result_Bldg0$ymin[i] <- st_bbox(result_Bldg0[i,])[2]
  st_geometry(result_Bldg0[i,]) <- st_geometry(result_Bldg0[i,]) - c(result_Bldg0$xmin[i], result_Bldg0$ymin[i])
  
}

############################################
save.image("F.RData")
############################################

# Apply minimum 8 x 20 area to get rid of unviable spaces
# Merge the possible rectangles together
bldg0_buildable <- NULL

# 3516 self intersection error
# 3551 self intersection error
# 3692 self intersection error
# 4020 self intersection error
# 4562, 4896, 5052, 5295, 5726, 5733


for (i in 5734:nrow(result_Bldg0)){
  print(i)
  geom <- largestRect(result_Bldg0[i,], print = TRUE)
  # print(i)
  if (is.character(geom)){
    sf <- st_sf(APN = result_Bldg0$APN[i], message = geom, geometry = st_sfc(st_polygon()))
  }
  else{
    sf <- st_sf(APN = result_Bldg0$APN[i], message = "Success", geometry = geom)
  }
  # print(i)
  bldg0_buildable <- rbind(bldg0_buildable, sf)
}

##### Work in Progress
# test <- largestRect(result_Bldg0[3551,], print = TRUE)
# 
# for(i in 1:(length(test)-1)){
#   if(test[i] == test[i+1]){
#     print(test[i])
#   }
# }

##############################################
save.image("G.RData")
#############################################
#########EXPLORE HERE#################################

for (i in 1:nrow(result_Bldg0)){
  print(i)
  largestRect(result_Bldg0[i,], print = TRUE)
}

# To show the result for an index:
# Choose a value for i
i <- 1
largestRect(result_Bldg0[i,], print = TRUE)

test <- bldg0_buildable[7,]
# Set CRS as 102643
test <- test %>% st_set_crs(102643)
test2 <- fill_holes(test, 1000)
plot(test)
plot(test2)

result_Bldg0 <- result_Bldg0 %>% st_set_crs(102643)
bldg0_buildable <- bldg0_buildable %>% st_set_crs(102643)

result_Bldg0_offset <- result_Bldg0

# Return geometries to original coordinates, no offsets
for(i in 1:nrow(result_Bldg0)){
  offset <- c(result_Bldg0$xmin[i], result_Bldg0$ymin[i])
  result_Bldg0$geometry[i] <- result_Bldg0$geometry[i] + offset
  result_Bldg0$xmin[i] <- 0
  result_Bldg0$ymin[i] <- 0
}


rownames(test) <- c()
saveAPN <- test$APN[1]
for(i in 1:nrow(test)){
  offset <- c(test$xmin[i], test$ymin[i])
  test$geometry[i] <- test$geometry[i] + offset
  test$xmin[i] <- 0
  test$ymin[i] <- 0
}


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


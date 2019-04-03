# devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)
library(jsonlite)
library(readr)
library(MASS)
library(sp)
library(dplyr)


### parcels minus building footprint
json_file <- "epaparcelsnobldg.json"
full_json_data <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
# Filter down to 3920 SFH parcels
apn_file <- "apn.csv"
apn <- read_csv(apn_file)
apn <- paste( "0", apn$apn, sep = "")
full_json_data <- dplyr::filter(full_json_data, APN %in% apn)

### parcels and their streets
street_file <- "apn_street.csv"
street <- read_csv(street_file)
street$apn <- paste0( "0", street$apn)
full_json_data <- full_json_data %>% inner_join(street, by = c("APN" = "apn"))


# Pull polygons
json_data <- full_json_data$json_geometry.coordinates
len_json <- length(json_data)

# Find number of polygons for each parcel
numPoly <- vector(mode = "double", length = length(json_data))
for (i in 1:len_json){
  if (typeof(json_data[[i]]) == "list"){
    numPoly[i] <- length(json_data[[i]])
  } else if (typeof(json_data[[i]]) == "double"){
    numPoly[i] <- dim(json_data[[i]])[1]
  } else {
    numPoly[i] <- 0
  }
}
table(numPoly)


### parcels without building footprint
parcel_file <- "epaparcels_clean.geojson"
parcels <- fromJSON(txt=parcel_file, flatten = TRUE, simplifyDataFrame = TRUE)
parcels <- parcels$features
parcels <- dplyr::filter(parcels, properties.APN %in% full_json_data$APN)
parcel_data <- parcels$geometry.coordinates


### roads
roads_file <- "epa_roads_adj.geojson"
roads <- fromJSON(txt=roads_file, flatten = TRUE, simplifyDataFram = TRUE)
roads <- roads$features

match_street <- toupper(match_street) %in% toupper(roads$properties.FULLNAME)



# Initialize column for storing road segments
full_json_data[,"list_roads"] <- NA
for (i in 1:dim(full_json_data)[1]){
  full_json_data$list_roads[i] <- list(roads %>% filter(toupper(properties.FULLNAME) == toupper(full_json_data$address[[i]])))
}


### coordinates of front
json_block <- "front_vertices_cleaned.geojson"
json_block_data <- fromJSON(txt=json_block, flatten = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
json_block_data <- json_block_data$features$geometry.coordinates
json_block_length <- length(json_block_data)
json_block_points <- array(data = 0, dim = c(json_block_length, 2))
for(i in 1:json_block_length){
  print(i)
  v <- json_block_data[[i]]
  if(is.null(json_block_data[[i]])){next}
  x <- v[1]
  y <- v[2]
  json_block_points[i,1] <- x
  json_block_points[i,2] <- y
}
json_block_points <- json_block_points[json_block_points[,1] != 0,]
json_block_points <- json_block_points[order( json_block_points[,1], json_block_points[,2]),]
json_block_points <- unique(json_block_points)
json_block_length <- dim(json_block_points)[1]

# json_block <- "epaparcels_dissolved/epaparcels_dissolved_adj.geojson"
# json_block_data <- fromJSON(txt=json_block, flatten = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
# json_block_data <- json_block_data$features$geometry.coordinates[[1]]
# 
# json_block_length <- 0
# for(i in 1:length(json_block_data)){
#   json_block_data[[i]] <- drop(json_block_data[[i]])
#   json_block_length <- json_block_length + dim(json_block_data[[i]])[1]
# }
# jsonIndex <- 1
# json_block_points <- array(data = 0, dim = c(json_block_length, 2))
# for(i in 1:length(json_block_data)){
#   for(j in 1:dim(json_block_data[[i]])[1]){
#     json_block_points[jsonIndex,] <- json_block_data[[i]][j,]
#     jsonIndex <- jsonIndex + 1
#   }
# }
# json_block_points <- json_block_points[order( json_block_points[,1], json_block_points[,2]),]

# Check if point is at front
# Inputs one point (x, y coordinate 1 x 2 vector)
# Outputs a boolean
isFront1 <- function(point){
  # Check if point matches json_block_data
  index <- findInterval(point[1], json_block_points[,1])  # This index may be +/- 1 from the actual value
  if (index > 0) {index <- index - 1}                   # Move back one
  for (i in 1:3){                      # Make sure a minimum of three iterations done before stopping
    while((index > 0) && (index <= dim(json_block_points)[1]) && (abs(point[1] - json_block_points[index,1]) <= 0.01)){         # search all coordinates w/ same X coord
      if(abs(point[2] - json_block_points[index,2]) <= 0.01){return(TRUE)}
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
  angle <- vector(mode = "double", length = dim(arr)[1]-1)
  for (j in 1:length(angle)){
    angle[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
  }
  i <- 2
  while (i <= length(angle)){
    if (abs(angle[i] - angle[i-1])/pi*180 < 0.5) {
      arr <- arr[-i,]
      angle[i-1] <- (angle[i]+angle[i-1])/2
      angle <- angle[-i]
      i <- i - 1            # adjust index, since we removed a point
    }
    i <- i + 1              # increment index
  }
  return(arr)
}


for (i in 1:len_json){
  arr <- drop(parcel_data[[i]])
  parcel_data[[i]] <- removeCollinear(arr)
}


json_data <-  parcel_data        #### DOUBLE CHECK THIS VARIABLE EVERY TIME
# Find number of landlocked parcels (all points are touching another parcel)
# First, pull all the coordinates for the first polygon for each parcel
checkFront <- vector("logical", len_json)   # Keep track of whether a parcel touches the front
numFront <- vector("double", len_json)      # Keep track of number of points that touch the front
indexFront <- vector("list", len_json)      # Keep track of index of points that touch the front
for (i in 1:len_json){
  print(i)
  if (typeof(json_data[[i]]) == "list"){
    poly <- unique(drop(json_data[[i]][[1]]))
  }
  else if (typeof(json_data[[i]]) == "double"){
    # if (length(dim(json_data[[i]])) > 3){
      # poly <- drop(json_data[[i]])[1,,]
      poly <- unique(drop(json_data[[i]]))
    # }
    # else{poly <- unique(json_data[[i]][1,,])}
  }
  else{
    break
  }
  indexFront[[i]] <- vector("logical", dim(poly)[1])
  for (j in 1:dim(poly)[1]){
    if (isFront1(poly[j,])){
      checkFront[i] <- TRUE
      indexFront[[i]][j] <- TRUE
      numFront[i] <- numFront[i] + 1
    }
  }
}

sum(checkFront)
length(numFront[numFront == 0])
length(numFront[numFront == 1])
length(numFront[numFront == 2])
length(numFront[numFront > 2])




# Front indices may be out of order if the polygon starting point is in the middle of the front
chunks <- vector("double", len_json)              # chunks of points marked as front
false_chunks <- vector("double", len_json)        # chunks of points not marked as front (used to ID problems)
offsetFront <- vector("double", len_json)         # what number of points to move to the back
deleteFront <- vector("double", len_json)         # what index point to remove
chunks2 <- vector("double", len_json)              # chunks of points marked as front
false_chunks2 <- vector("double", len_json)        # chunks of points not marked as front (used to ID problems)
for (i in 1:len_json){
  preVal <- FALSE
  arr <- indexFront[[i]]
  for (j in 1:length(arr)){
    if (j == 1 && arr[j]){chunks[i] <- chunks[i] + 1}
    else if (j == 1 && arr[j] == FALSE){false_chunks[i] <- false_chunks[i] + 1}
    else if (preVal == FALSE && arr[j]){chunks[i] <- chunks[i] + 1}
    else if (preVal && arr[j] == FALSE){false_chunks[i] <- false_chunks[i] + 1}
    preVal <- arr[j]
  }
  # Track which parcel's front vertices need reordering
  # If false_chunks < chunks, reorder by length of first true chunk
  if (false_chunks[i] < chunks[i]){
    for (j in 1:length(arr)){
      if (!arr[j]){
        break
      }
      offsetFront[i] <- offsetFront[i] + 1
    }
    indexFront[[i]] <- c(indexFront[[i]][-1:-offsetFront[i]], indexFront[[i]][1:offsetFront[i]])
  }
  preVal <- FALSE
  for (j in 1:length(arr)){
    if (j == 1 && indexFront[[i]][j]){chunks2[i] <- chunks2[i] + 1}
    else if (j == 1 && indexFront[[i]][j] == FALSE){false_chunks2[i] <- false_chunks2[i] + 1}
    else if (preVal == FALSE && indexFront[[i]][j]){chunks2[i] <- chunks2[i] + 1}
    else if (preVal && indexFront[[i]][j] == FALSE){false_chunks2[i] <- false_chunks2[i] + 1}
    preVal <- indexFront[[i]][j]
  }
  if (chunks2[i] == 2){
    for (j in 2:(length(arr)-1)){
      if (isTRUE(indexFront[[i]][j] && (!indexFront[[i]][j-1] && !indexFront[[i]][j+1]))){
        deleteFront[i] <- j
      }
    }
  }
}

table(chunks)
table(false_chunks)
table(chunks2)
table(false_chunks2)
table(deleteFront)

View(cbind(chunks, false_chunks))
View(cbind(chunks2, false_chunks2))

# Extract parcel front points
parcelFront <- vector(mode = "list", len_json)     # Keep track of the points that touch the front
# for (i in 1:len_json){
for (i in 1:len_json){
  if (checkFront[i]){   # Check whether we need to save any points
    parcelFront[[i]] <- array(dim = c(numFront[i],2))
    index <- 1
    if (typeof(json_data[[i]]) == "list"){
      poly <- unique(drop(json_data[[i]][[1]]))
    }
    else if (typeof(json_data[[i]]) == "double"){
      # if (length(dim(json_data[[i]])) > 3){
        # poly <- drop(json_data[[i]])[1,,]
        poly <- unique(drop(json_data[[i]]))
      # }
      # else{poly <- unique(json_data[[i]][1,,])}
    }
    for (j in 1:(dim(poly)[1])){
      if (isFront1(poly[j,])){
        parcelFront[[i]][index,] <- poly[j,]
        index <- index + 1
      }
    }
  }
  if (deleteFront[i] > 0){
    parcelFront[[i]] <- parcelFront[[i]][-deleteFront[i],]
  }
  # Check if indices need rearranging
  if (offsetFront[i] > 0){
    parcelFront[[i]] <- rbind(parcelFront[[i]][-1:-offsetFront[i], ], parcelFront[[i]][1:offsetFront[i], ])
  }
}


# Check if front edge is properly extracted
for (i in 1:len_json){
  print(i)
  if (is.null(parcelFront[[i]])) {next}
  if (dim(parcelFront[[i]])[1] == 1) {next}
  else {eqscplot(parcelFront[[i]], type="l")}
  Sys.sleep(0.05)  # Pause and continues automatically
  # invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
}

# Identify corner lots
cornerStats <- vector(mode = "list", len_json)
for (i in 1:len_json){
  if(is.null(parcelFront[[i]])) {next}              # Skip landlocked
  else if (dim(parcelFront[[i]])[1] < 3) {next}     # Skip parcels with 1 or 2 front points (most likly not a corner)
  arr <- parcelFront[[i]]                           # Get the front points
  
  # Find angles
  angle <- vector(mode = "double", length = dim(arr)[1]-1)
  for (j in 1:length(angle)){
    angle[j] <- atan2(arr[j+1,2]-arr[j,2], arr[j+1,1]-arr[j,1])
  }
  totang1 <- atan2(arr[dim(arr)[1],2]-arr[1,2], arr[dim(arr)[1],1]-arr[1,1])
  totang2 <- totang1 - pi
  # Save angles
  cornerStats[[i]]$angles <- angle
  # Average angle
  cornerStats[[i]]$avg <- mean(angle)
  # Difference in angle between first segment and hypotenuse (first to last point)
  cornerStats[[i]]$totdiff1 <- atan2(sin(totang1-angle[1]),cos(totang1-angle[1]))
  # Difference in angle between last segment and hypotenuse (first to last point)
  cornerStats[[i]]$totdiff2 <- atan2(sin(totang2-angle[length(angle)]+pi),cos(totang2-angle[length(angle)]+pi))
  # Difference in angle between first segment and last segment
  cornerStats[[i]]$segdiff <- atan2(sin(angle[length(angle)]-angle[1]), cos(angle[length(angle)]-angle[1]))
}

isCorner <- vector("logical", len_json)
for (i in 1:len_json){
  if (is.null(cornerStats[[i]])) {next}
  # if mostly straight, skip
  if (abs(cornerStats[[i]]$totdiff1) < (20/180*pi) || abs(cornerStats[[i]]$totdiff2) < (20/180*pi)) {next}
  # if first and last segment have more than 60 degrees diff, mark as corner
  if (abs(cornerStats[[i]]$segdiff) >= (60/180*pi)) {isCorner[i] <- TRUE}
}

# Extract front edge for corner lot
# Read in roadway network. Find two closest roadway points to parcel
# Find edges within X degrees of parallel to roadway
# For cases where there are more than 2 "chunks" near parallel, find chunk that is closest to roadway
chunks <- vector("double", len_json)              # chunks of edges near parallel to road
false_chunks <- vector("double", len_json)        # chunks of edges not near parallel to road
modParcelFront <- parcelFront
for (i in 1:len_json){
  if (isCorner[i]){  # Only modify if it is marked as a corner parcel
    print(i)
    par_cent <- colMeans(drop(parcel_data[[i]]))      # Get the parcel centroid
    # Extract associated road
    roads <- full_json_data$list_roads[[i]]$geometry.coordinates     # Get the list
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
      
    for (j in 1:dim(road_arr)[1]){
      dist <- sqrt((par_cent[1]-road_arr[j,1])^2 + (par_cent[2]-road_arr[j,1])^2)
      if (dist < minDist1){
        point2 <- point1
        minDist2 <- minDist1
        point1 <- road_arr[j,]
        minDist1 <- dist
      }
    }
    # We now have the angle of the roadway
    road_ang <- atan2(point2[2]-point1[2],point2[1]-point1[1])
    print(road_ang)
    
    # Find edges within X degrees of parallel to roadway
    angles <- cornerStats[[i]]$angles
    nearParallel <- (abs(sin(angles - road_ang)) < sqrt(2)/2)
    print(nearParallel)
    
    # Check if there are more than one "chunk" (of consecutive edges) that is near parallel
    preVal <- FALSE
    chunkInd <- vector()
    for (j in 1:length(nearParallel)){
      if (j == 1 && nearParallel[j]){
        chunks[i] <- chunks[i] + 1
        chunkInd <- c(chunkInd, j)
        }
      else if (j == 1 && nearParallel[j] == FALSE){false_chunks[i] <- false_chunks[i] + 1}
      else if (preVal == FALSE && nearParallel[j]){
        chunks[i] <- chunks[i] + 1
        chunkInd <- c(chunkInd, j)
        }
      else if (preVal && nearParallel[j] == FALSE){
        false_chunks[i] <- false_chunks[i] + 1
        chunkInd <- c(chunkInd, j - 1)
        }
      preVal <- nearParallel[j]
    }
    if (preVal == TRUE){
      chunkInd <- c(chunkInd, length(nearParallel))
    }
    
    # Case 1: more than one chunk
    if (chunks[i] > 1){
      chunkCent <- array(dim = c(chunks[i],2)) # Find centroid of each chunk
      perpDistCent <- vector(length = chunks[i])
      for (j in 1:chunks[i]){
        start <- chunkInd[2*j-1]
        finish <- chunkInd[2*j]
        if (start == finish){
          chunkCent[j,] <- parcelFront[[i]][start,]
        } else{
          chunkCent[j,] <- colMeans(parcelFront[[i]][start:finish,])
        }
        perpDistCent[j] <- perpDist(chunkCent[j,1],chunkCent[j,2],point1[1],point1[2],point2[1],point2[2])
      }
      # Find which chunk is the shortest perpendicular dist from roadway
      chunkNum <- which(perpDistCent == min(perpDistCent))[1]
      start <- chunkInd[2*chunkNum-1]
      finish <- chunkInd[2*chunkNum]
      print(paste("Start = ", start, "Finish = ", finish))
      modParcelFront[[i]] <- parcelFront[[i]][start:(finish+1),]
    }
    # Case 2: one chunk. Just return that
    else{
      start <- chunkInd[1]
      finish <- chunkInd[2]
      print(paste("Start = ", start, "Finish = ", finish))
      modParcelFront[[i]] <- parcelFront[[i]][start:(finish+1),]
    }
  }
}

# Check if the modified parcel fronts are good
for (i in 1:len_json){
  if (isCorner[i]){
    eqscplot(json_data[[i]],type='l')
    points(modParcelFront[[i]])
    Sys.sleep(0.1)  # Pause and continues automatically
    # invisible(readline(prompt="Press [enter] to continue"))  # Manually press enter to continue
  }
}


# Function
# ID parcel sides (front, side, back)
# Front edges are in between front points
# Back edge is furthest away (true distance) from front centroid
# If edges somehow tie, select edge that has smallest angle difference
# If 2 edges somehow tie, select randomly between edges. Or if more than two edges, furthest perpendicular distance?
# Inputs: parcel_data, parcelFront, indexFront
# Outputs: string vector that returns "front/side/rear" for each corresponding edge. 
# Edge 1 = point1 to point2, edge 2 = point2 to point3, etc. Length of vector is length(parcel_data) - 1.
idEdges <- function(par, indF){
  edges <- indF
  # print(edges)
  # sides[(sides[])] <- "Front"
  # Find average of front
  centF <- colMeans(par[indF[],])
  # Find furthest edge (using midpoint)
  maxDist <- 0
  maxInd <- NULL
  for (i in 1:(dim(par)[1]-1)){
    if (!indF[i]){       # Test only if not a front edge
      edge <- (par[i,] + par[i + 1, ]) / 2
      dist2 <- (edge[1]-centF[1])*(edge[1]-centF[1]) + (edge[2]-centF[2])*(edge[2]-centF[2])
      # Case 1: further distance, assign new furthest edge
      if (dist2 > maxDist){
        maxDist <- dist2
        maxInd <- i
      }
      # Case 2: distance matches, keep all furthest edges
      else if (dist2 == maxDist){
        maxInd <- c(maxInd, i)
      }
      # Case 3: distance is shorter. Keep moving.
    }
  }
  # print(maxInd)
  edges[(edges[])] <- "Front"
  edges[maxInd] <- "Rear"
  # Anything not yet marked is a side edge
  edges[(edges[] == "FALSE")] <- "Side"
  return(edges)
}


# Cut out front yard
# Input: parcel_data, building_data, parcelFront
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
    dist <- perpDist(bldg[i,1], bldg[i,2], p_i[1], p_i[2], p_f[1], p_f[2])
    if (dist < minDist){
      minIndex <- i
      minDist <- dist
    }
  }
  # print(minIndex)
  # print(minDist)
  # Find parallel line
  ang <- atan2((p_f-p_i)[2],(p_f-p_i)[1])
  pt <- bldg[minIndex,]
  r_ang <- c(cos(ang),sin(ang))
  return (c(pt, r_ang))
  
  #### NOT USED ANYMORE
  # # For each line of the parcel, check if intersects with front cut line
  # # Store (1) index of parcel side and (2) intersection point on parcel
  # # Usually will be two intersections, but may be more if unusual shape
  # parcelIndex <- vector("double")
  # parcelPoints <- array(dim = c(0,2))
  # for (i in 1:(dim(par)[1]-1)){
  #   p1 <- par[i,]
  #   p2 <- par[i+1,]
  #   if (!is.null(checkIntersect(p1,p2,pt,ang))){
  #     print(i)
  #     parcelIndex <- c(parcelIndex,i)
  #     parcelPoints <- rbind(parcelPoints,checkIntersect(p1,p2,pt,ang))
  #   }
  # }
  # print(parcelIndex)
  # # Normal case: two intersection points
  # if (length(parcelIndex) == 2){
  #   opt1 <- array(dim=c(20,2))
  #   opt2 <- array(dim=c(20,2))
  #   A <- parcelIndex[1]
  #   B <- parcelIndex[2]
  #   # Option 1: loop from lower index (A) to higher index (B)
  #   opt1[1,] <- parcelPoints[1,]    # point A
  #   index <- 2
  #   for (i in (A+1):B){
  #     opt1[index,] <- par[i,]
  #     index <- index + 1
  #   }
  #   opt1[index,] <- parcelPoints[2,] # point B
  #   index <- index + 1
  #   opt1[index,] <- parcelPoints[1,]  # back to point A
  #   opt1 <- opt1[which(rowSums(opt1)>0),]
  #   opt1 <- unique(opt1)
  #   opt1 <- rbind(opt1, opt1[1,])
  #   # Option 2: loop from higher index (B) back to lower index (A)
  #   opt2[1,] <- parcelPoints[2,] # point B
  #   index <- 2
  #   i <- B + 1
  #   if (i > dim(par)[1]){   # If B is the last index, adjust i to 1
  #     i <- 1
  #   }
  #   while (i != A){
  #     opt2[index,] <- par[i,]
  #     i <- i + 1
  #     if (i > dim(par)[1]){
  #       i <- 1
  #     }
  #   }
  #   opt2[index,] <- parcelPoints[1,] # point A
  #   index <- index + 1
  #   opt2[index,] <- parcelPoints[2,] # point B
  #   opt2 <- opt2[which(rowSums(opt2)>0),]
  #   opt2 <- unique(opt2)
  #   opt2 <- rbind(opt2, opt2[1,])
  #   # Check if option 1 goes through front
  #   for (i in 1:dim(opt1)[1]){
  #     # if (isFront1(opt1[i,])){   
  #     #   return (opt2)
  #     # }
  #     for (j in 1:dim(front)[1]){
  #       if (isTRUE(all.equal(opt1[i,],front[j,]))){  # If any point is on the front, return option 2
  #         return (opt2)
  #       }
  #     }
  #   }
  #   return (opt1)
  # }
  # # Other cases: deal with later
  # else {print("Abnormal number of intersections")}
}

# Helper function: shortest distance between a point and a line segment
# (x,y) is the point, (x1,y2) and (x2,y2) make the line segment
# https://stackoverflow.com/a/6853926
perpDist <- function(x, y, x1, y1, x2, y2){
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

# Check if 2 line segments intersect (line 1 made by point and angle) (line 2 made by two points)
# Input: point 1 of line segment, point 2 of line segment, point of line, angle of line
# Input: p1, p2, pt are vectors (length 2), ang is in degrees
# Output: returns true/false
# https://stackoverflow.com/a/565282/68063
checkIntersect <- function(p1, p2, pt, ang) {
  q <- p1
  s <- p2 - p1
  # if (s[1]*s[1]+s[2]*s[2] < 1e-5) {return (FALSE)} # if two points are too close together,
  p <- pt
  r <- c(cos(ang / 180 * pi), sin(ang / 180 * pi))
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
# Output: returns true/false
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


# Side and rear buffers
# Input: parcel_data, edgeID (front, side, rear), building footprint, front points
# Param: side buffer dist, rear buffer dist
# Draw parallel lines that are [buffer dist] away from each side
# Select parallel lines that are closer to parcel centroid, so lines are going inward, not outward
# Find intersection between adjacent lines to reconstruct buffered parcel
# Check whether buffer overlaps with building. If it does, adjust so that there is no intersection in result
# Output: new parcel coordinates, array (n by 2)
allBuffers <- function(par, edges, bldg, front, side_dist, rear_dist){
  buffers <- array(dim = c(0, 4))
  skipFront <- FALSE               # Keep track of whether front cut was already added. If so, skip other front edges
  # Find the lines for all the buffers
  for (i in 1:length(edges)){
    if (edges[i] == "Front"){
      if (skipFront){
        next
      }
      else{
        buffers <- rbind(buffers, removeFront(par, bldg, front))
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
  newPar <- array(dim = c((length(edges)+1), 2))
  for (i in 1:length(edges)){
    newPar[i,] <- intersectLines(buffers[i,],buffers[i+1,])
  }
  newPar[length(edges)+1,] <- newPar[1,]
  return (newPar)
}



# Helper Function: buffer for one edge
# Input: parcel_data
# Param: edge_index, buffer distance
# Draw two points [buffer dist] perpendicular from edge midpoint, choose the point that is inside polygon
# Draws parallel line that [buffer dist] away from the edge
# Returns a line for the edge (u + param * angle vector r) as a vector length 4 (ux, uy, rx, ry)
buffer <- function(par, ind, dist){
  edge <- par[ind:(ind+1),]      # index and next point
  # print(edge)
  edge_mp <- colMeans(edge)      # Edge midpoint
  # print(edge_mp)
  edge_ang <- atan2(par[ind+1,2]-par[ind,2], par[ind+1,1]-par[ind,1])
  # print(edge_ang)
  offset <- dist * c(cos(edge_ang + pi/2), sin(edge_ang + pi/2))
  # print(offset)
  opt1 <- edge_mp + offset
  # print(opt1)
  opt2 <- edge_mp - offset
  # print(opt2)
  if (point.in.polygon(opt1[1],opt1[2],par[,1],par[,2]) == 1){ # If opt1 is inside polygon
    vec <- c(cos(edge_ang), sin(edge_ang))
    return (c(opt1, vec))    
  } else if (point.in.polygon(opt2[1],opt2[2],par[,1],par[,2]) == 1){ # If opt2 is inside polygon
    vec <- c(cos(edge_ang), sin(edge_ang))
    return (c(opt2, vec))
  } else{
    print("Buffer does not work for this polygon")
  }
}




# Find which parcels are landlocked. These will need front edges manually identified
landlocked <- vector("logical", len_json - sum(checkFront))
index <- 1
for (i in 1:len_json){
  if (!checkFront[i]){
    landlocked[index] <- i
    index <- index + 1
  }
}
landlockedAPNs <- vector("logical", len_json - sum(checkFront))
for (i in 1:(len_json - sum(checkFront))){
  print(i)
  landlockedAPNs[i] <- full_json_data$APN[[landlocked[i]]]
}
write_csv(as.data.frame(landlockedAPNs), "landlockedAPNs.csv")

# Check if merge ring cuts at the front
# Inputs two points (x, y coordinate 1 x 2 vector)
# Outputs a boolean
isFront <- function(point1, point2){
  # Check if point1 matches json_block_data
  index <- findInterval(point1[1], json_block_points[,1])
  while(isTRUE(all.equal(point1[1], json_block_points[index,1]))){         # search all coordinates w/ same X coord
    if(isTRUE(all.equal(point1[2], json_block_points[index,2]))){return(TRUE)}
    index <- index + 1
  }
  # Check if point2 matches json_block_data
  index <- findInterval(point2[1], json_block_points[,1])
  while(isTRUE(all.equal(point2[1], json_block_points[index,1]))){         # search all coordinates w/ same X coord
    if(isTRUE(all.equal(point2[2], json_block_points[index,2]))){return(TRUE)}
    index <- index + 1
  }
  # for (i in 1:json_block_length){
  #   if(isTRUE(all.equal(point1, json_block_points[i,])) || isTRUE(all.equal(point2, json_block_points[i,]))){
  #     return(TRUE)
  #   }
  # }
  # If nothing returns true, return false
  return(FALSE)
}

# ring1 and ring2 are arrays of dim [x, 2] AND/OR lists of already merged rings
# Function is recursively to merge rings if list(s) is passed as an input
# returns list of merged rings
merge_rings <- function(ringA, ringB, frontCut = FALSE){ 
  result <- list()
  if (typeof(ringA) == "list"){
    indexA <- length(ringA)
  } else{indexA <- 1}
  if (typeof(ringB) == "list"){
    indexB <- length(ringB)
  } else{indexB <- 1}
  
  # Case 1: ringA is a list, ringB is an array
  if (indexA > 1 && indexB == 1){   #check if there are any lists
    for(j in 1:indexA){
      result <- append(result, merge_rings(ringA[[j]], ringB, FALSE))
    }
  } 
  
  # Case 2: ringB is a list, ringA is an array
  else if (indexA == 1 && indexB > 1){
    for(k in 1:indexB){
      result <- append(result, merge_rings(ringA, ringB[[k]], FALSE))
    }
  }
  
  # Case 3: ringA and ringB are both lists
  else if (indexA > 1 && indexB > 1){
    for(j in 1:indexA){
      for(k in 1:indexB){
        result <- append(result, merge_rings(ringA[[j]], ringB[[k]], FALSE))
      }
    }
  }
  # Base Case: ringA and ringB are arrays
  # ring2 is the polygon with fewer points
  # ring1 is the polygon with more points
  else{
    if (length(ringA[,1]) <= length(ringB[,1])){
      ring2 <- ringA
      ring1 <- ringB
    } else {
      ring1 <- ringA
      ring2 <- ringB
    }
    len_r1 <- length(ring1[,1])-1  #don't need to consider last point, which is the same as point 1
    len_r2 <- length(ring2[,1])-1  #don't need to consider last point, which is the same as point 1
    
    # find all pairs of closest points
    ring2_closest <- vector("double", len_r2)
    # cycle through each ring1 point
    for(m in 1:len_r2){
      #returns index of closest ring2 point to current ring1 point
      min_dist <- 1e99 #start with a very high number
      min_index <- 0   #start with index 0 (not in the index)
      for (n in 1:len_r1) {
        dx <- abs(ring2[m,1]-ring1[n,1])
        dy <- abs(ring2[m,2]-ring1[n,2])
        dist <- sqrt(dx*dx+dy*dy)
        if (dist < min_dist) {
          min_dist <- dist
          min_index <- n
        }
      }
      # saves index of closest ring2 point
      ring2_closest[m] <- min_index
    }
    cutIndex <- vector(mode = "logical", len_r2)
    cutIndex[] <- TRUE
    if (frontCut){
      # ID the indices for points on ring1 and ring2 for the cuts after confirming that it goes through the front
      cutIndex[] <- FALSE
      # Filter index to get cuts that go through the front
      for(m in 1:len_r2){
        # index1 = m
        point1 <- ring2[m,]
        index2 <- ring2_closest[m]
        point2 <- ring1[ring2_closest[m],]
        if (isFront(point1, point2)) {
          cutIndex[m] <- TRUE
        }
      }
      # Remove empty rows
      if(sum(cutIndex) == 0){                     # If no matches for front cut, switch to no front cut
        frontCut <- FALSE
        cutIndex[] <- TRUE                        # 
      }
    }
    
    result <- vector("list", len_r2)
    for(i in 1:len_r2){
      if(cutIndex[i] == FALSE){next}
      # merge rings for 1 iteration
      ring_merge <- cbind(vector("double", len_r1+len_r2+3),vector("double", len_r1+len_r2+3))
      index_merge <- 1   # keep track of index in ring_merge
      index2 <- i        # keep track of index in ring2
      # add ring1 to ring_merge
      for(m in 1:len_r2){
        ring_merge[index_merge,1] <- ring2[index2,1]  # transfer value
        ring_merge[index_merge,2] <- ring2[index2,2]  # transfer value
        index2 <- index2 %% len_r2 + 1 #increment, if at end of index, loops back to 1
        index_merge <- index_merge + 1 #increment, no need to loop back
      }
      #add last point to close ring2
      ring_merge[index_merge,1] <- ring2[index2,1]
      ring_merge[index_merge,2] <- ring2[index2,2]
      index_merge <- index_merge + 1
      #add ring2 to ring_merge
      index1 <- ring2_closest[index2]
      for(n in 1:len_r1){
        ring_merge[index_merge,1] <- ring1[index1,1]  # transfer value
        ring_merge[index_merge,2] <- ring1[index1,2]  # transfer value
        index1 <- index1 - 1 #increment, if at end of index, loops back to 1
        if (index1 == 0) {index1 <- len_r1}
        index_merge <- index_merge + 1 #increment, no need to loop back
      }
      ring_merge[index_merge,1] <- ring1[index1,1]
      ring_merge[index_merge,2] <- ring1[index1,2]
      index_merge <- index_merge + 1
      ring_merge[index_merge,1] <- ring2[index2,1]
      ring_merge[index_merge,2] <- ring2[index2,2]
      result[[i]] <- ring_merge
    }
    if(frontCut){                   # remove empty indices
      result <- result[cutIndex]
    }
    if (length(result) == 1){return(result[[1]])} else{return(result)}
  }
  if (length(result) == 1){return(result[[1]])} else{return(result)}
}

# Create recursive function to unpack array with more than 2 polygons
# Takes an array
# Returns a list of merged arrays
# Account for errors
unpackArray <-local({
  function(arr) {
    dim <- dim(arr)    # get length of first dimension of array
    if(length(dim) > 3){
      print("Error. Nonconforming array")
      return()
    }
    if(dim[3] == 2){       # add first poly
      result <- arr[1,,]
    } else{
      print("Error")
      return()
    }
    for(i in 2:dim[1]){       # go through list
      # check if element is a list or array
      result <- merge_rings(result, arr[i,,], TRUE)
    }
    return(result)
  }
})



# Create recursive function to unpack list elements
# Takes a list (maybe of more lists)
# Returns a list of merged arrays
unpackList <-local({
  function(lst) {
    len <- length(lst)    # get length of list
    if(typeof(lst[[1]]) == "list"){
      result <- unpackList(lst[[1]])  # initialize result, recursively call function
    } else if (typeof(lst[[1]]) == "double"){
      arr <- drop(lst[[1]])
      if (length(dim(arr)) == 2 && dim(arr)[2] == 2){   # check that array has R x 2 dimensions
        result <- arr
      }
    } else{
      print("Error")
      return()
    }
    for(i in 2:len){       # go through list
      # check if element is a list or array
      if (typeof(lst[[i]]) == "list"){
        result <- merge_rings(result, unpackList(lst[[i]]), TRUE) # merge onto existing result, recursively call function
      } else if (typeof(lst[[i]]) == "double"){
        arr <- drop(lst[[i]])
        if (length(dim(arr)) == 2 && dim(arr)[2] == 2){   # check that array has R x 2 dimensions
          result <- merge_rings(result, arr, TRUE)                # merge onto existing result
        } 
      } else{
        print("Error")
        return()
      }
    }
    return(result)
  }
})


# convert JSON data into merged polygons
all_merged_rings <- vector("list", len_json)
manualJSON <- array(dim = c(len_json, 2))
jsonIndex <- 1
for(a in 1:len_json) {
  all_merged_rings[[a]] <- NA
  print(a)
  # Check the type of data: double (aka array) or list
  arr <- drop(json_data[[a]]) # gets rid of dimensions of 1 length
  if (typeof(json_data[[a]]) == "double") { # is an array [a x b x c]
    if(length(dim(arr)) == 3 && dim(arr)[1] == 2 && dim(arr)[3] == 2) { # if array is [2 x b x 2]
      ring1 <- arr[1,,]
      ring2 <- arr[2,,]
      all_merged_rings[[a]] <- merge_rings(ring1,ring2,TRUE)
      # all_merged_rings[[a]] <- 1
    }
    else if (length(dim(arr)) == 2 && dim(arr)[2] == 2){    # if is just one polygon, return that polygon
      all_merged_rings[[a]] <- arr
    }
    else if (length(dim(arr)) == 3 && dim(arr)[1] > 2 && dim(arr)[3] == 2){ # if array is [a x b x 2] where a > 2
      all_merged_rings[[a]] <- unpackArray(arr)
    }
    else {
      manualJSON[jsonIndex, 1] <- a
      manualJSON[jsonIndex, 2] <- "nonconforming array"
      jsonIndex <- jsonIndex + 1
    }                                  
  } 
  else if (typeof(json_data[[a]]) == "list") {
    if (length(json_data[[a]]) > 5){
      manualJSON[jsonIndex, 1] <- a
      manualJSON[jsonIndex, 2] <- "long list"
      jsonIndex <- jsonIndex + 1
    } else{all_merged_rings[[a]] <- unpackList(json_data[[a]])}
  }
  else{
    manualJSON[jsonIndex, 1] <- a
    manualJSON[jsonIndex, 2] <- "other error"
    jsonIndex <- jsonIndex + 1
  }
}


plotrect <- function(lr1){
  ## convert rectangle output to coordinates
  
  ## zero-centred
  pts = cbind(
    c(lr1$width,lr1$width,-lr1$width,-lr1$width)/2,
    c(lr1$height,-lr1$height,-lr1$height,lr1$height)/2
  )
  
  ## rotate
  r = rmat(lr1$ang)
  xy = pts %*% r
  
  ## add centre offsets
  xy[,1] = xy[,1] + lr1$cx
  xy[,2] = xy[,2] + lr1$cy
  
  ## repeat last point so output is a closed polygon
  rbind(xy,xy[1,])
}

find_lr <- function(ct, xy, options){
  ## xy MUST be a two-column matrix. This transfers it as
  ## an Array of Arrays:
  ct$assign("xy",xy)
  ## call the largest rectangle routine and return the result
  ## TODO: pass options here
  # if(!missing(options)){ct$assign("options", options)}
  tryCatch(
    {
      lrect = ct$get("window.largestRect(xy)")
      lrect
    }, error = function(e){
      find_lr(ct, xy, options)
    }
  )
}

prepPoly <- function(poly){
  poly <- apply(poly, c(1,2), round) # round the coordinates
  minX <- min(poly[,1])              # get the smallest x coord
  minY <- min(poly[,2])              # get the smallest y coord
  poly[,1] <- poly[,1]- minX         # adjust for relative coordinates
  poly[,2] <- poly[,2]- minY         # adjust for relative coordinates
  return(list(poly,minX,minY))
}

# See # of combinations of cuts 
test <- vector(mode = "double", length = 5051)
for(i in 1:5051){
  print(i)
  if(typeof(all_merged_rings[[i]]) == "list"){
    test[i] <-  length(all_merged_rings[[i]])
  }
  else if (typeof(all_merged_rings[[i]]) == "double"){
    test[i] <- dim(all_merged_rings[[i]])[1]
  } else if (is.na(all_merged_rings[[i]])){
    next
  }
}
View(test)
# Run to find maxrect for all polygon combinations
count_success <- 0
count_error <- 0
error_poly <- array(0, dim=c(1000,2))
index_poly <- 1
listMaxRect <- vector("list", len_json)
ctx = initjs()
for(a in 1:5051) {
  ## process list types
  if(typeof(all_merged_rings[[a]]) == "list"){
    len = length(all_merged_rings[[a]])
    for(b in 1:min(len,22)){                    # if number of combinations tested capped at 64 (90th %ile)
      print(paste(a, ",", b))
      ctx = initjs()
      result <- prepPoly(all_merged_rings[[a]][[b]])
      poly <- result[[1]]
      minX <- result[[2]]
      minY <- result[[3]]
      tryCatch({lr = find_lr(ctx, poly)
      count_success <<- count_success + 1
      poly[,1] <- poly[,1] + minX
      poly[,2] <- poly[,2] + minY
      lr[[1]]$cx <- lr[[1]]$cx + minX
      lr[[1]]$cy <- lr[[1]]$cy + minY
      listMaxRect[[a]][[b]] <- lr
      eqscplot(poly, type= "l")
      pp = plotrect(lr[[1]])
      lines(pp)
      print("success")
      },
      error = function(e){
        print(paste("Error, skipping ", a, ", ", b))
        error_poly[index_poly,] <<- c(a,b)
        index_poly <<- index_poly + 1
      }
      )
    }
  } else if(typeof(all_merged_rings[[a]]) == "double"){
    print(paste(a, ", 1"))
    ctx = initjs()
    result <- prepPoly(all_merged_rings[[a]])
    poly <- result[[1]]
    minX <- result[[2]]
    minY <- result[[3]]
    tryCatch({lr = find_lr(ctx, poly)
    count_success <<- count_success + 1
    poly[,1] <- poly[,1] + minX
    poly[,2] <- poly[,2] + minY
    lr[[1]]$cx <- lr[[1]]$cx + minX
    lr[[1]]$cy <- lr[[1]]$cy + minY
    listMaxRect[[a]][[1]] <- lr
    eqscplot(poly, type= "l")
    pp = plotrect(lr[[1]])
    lines(pp)
    print("success")
    },
    error = function(e){
      print(paste("Error, skipping ", a, ", 1"))
      error_poly[index_poly,] <<- c(a,b)
      index_poly <<- index_poly + 1
    }
    )
  }
}



# Find max rectangle
# Loop through first index
for (i in 1:length(listMaxRect)){
  maxArea <- 0
  maxIndex <- 0
  # For each parcel, loop through each tested maxRect
  for (j in 1:length(listMaxRect[[i]])){
    lr <- listMaxRect[[i]][[j]]
    if (lr[[2]] > maxArea){
      maxArea <- lr[[2]]
      maxIndex <- j
    }
  }
  # At the end, assign 
  listMaxRect[[i]] <- lr
}


all_merged_rings[[4]][[1]]
all_merged_rings[[4]][[1]][1,]
View(all_merged_rings[[4]][[1]])
isFront(all_merged_rings[[4]][[1]], all_merged_rings[[4]][[2]])
eqscplot(all_merged_rings[[4]][[2]], type= "l")



##########################
# Accessory functions for troubleshooting

flip_order <- function(array, start, end){
  copy <- array(0, dim=c(end - start + 1, 2))
  for(n in 1:(end-start+1)){
    copy[n,] <- array[start+n-1,]
  }
  copy <- copy[nrow(copy):1,]
  for(n in 1:(end-start+1)){
    array[start + n - 1,1] <-  copy[n,1]
    array[start + n - 1,2] <-  copy[n,2]
  }
  return(array)
}

flip_all <- function(merged_ring){
  start_index <- 1
  end_index <- length(merged_ring[,1])
  merged_ring <- flip_order(merged_ring, start_index, end_index)
}
flip_ring1 <- function(merged_ring){         # flips ring1
  start_value <- merged_ring[1,] #coordinates of first point
  second_index <- 0              #initialize
  # find index for point that closes ring1, matches first point
  for (n in 2:length(merged_ring[,1])) {
    if (start_value[1] == merged_ring[n,1] && start_value[2] == merged_ring[n,2]) { 
      second_index <- n
      break
    }
  }
  merged_ring <- flip_order(merged_ring, 1, second_index)
  return(merged_ring)
}

flip_ring2 <- function(merged_ring){          # flips ring2
  start_value <- merged_ring[1,] #coordinates of first point
  second_index <- 0              #initialize
  third_index <- 0
  # find index for point that closes ring1, matches first point
  for (n in 2:length(merged_ring[,1])) {
    if (start_value[1] == merged_ring[n,1] && start_value[2] == merged_ring[n,2]) { 
      second_index <- n
      print(paste("second = ", n))
      break
    }
  }
  for (n in (second_index+1):length(merged_ring[,1])) {
    if (start_value[1] == merged_ring[n,1] && start_value[2] == merged_ring[n,2]) {
      third_index <- n
      print(paste("third = ", n))
      break
    }
  }
  merged_ring <- flip_order(merged_ring, second_index, third_index)
  return(merged_ring)
}

plot_call <- function(index1,index2){
  eqscplot(all_merged_rings[[index1]][[index2]], type= "l")
}

plot_merged <- function(index){
  eqscplot(all_merged_rings[[error_poly[index,1]]][[error_poly[index,2]]], type= "l")
}

########
#Export shapefiles

crds <- cbind(x=c(0, 0, 400, 400, 0), y=c(0, 400, 400, 0, 0))
str(crds)
Pl <- Polygon(crds)
str(Pl)
ID <- "400x400"
Pls <- Polygons(list(Pl), ID=ID)
str(Pls)
SPls <- SpatialPolygons(list(Pls))
str(SPls)
df <- data.frame(value=1, row.names=ID)
str(df)
SPDF <- SpatialPolygonsDataFrame(SPls, df)
str(SPDF)


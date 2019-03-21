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
  index <- findInterval(point[1], json_block_points[,1])
  while((index > 0) && (index <= dim(json_block_points)[1]) && (abs(point[1] - json_block_points[index,1]) <= 0.01)){         # search all coordinates w/ same X coord
    if(abs(point[2] - json_block_points[index,2]) <= 0.01){return(TRUE)}
    index <- index + 1
  }
  index <- index + 1
  while((index > 0) && (index <= dim(json_block_points)[1]) && (abs(point[1] - json_block_points[index,1]) <= 0.01)){         # search all coordinates w/ same X coord
    if(abs(point[2] - json_block_points[index,2]) <= 0.01){return(TRUE)}
    index <- index + 1
  }
  return(FALSE)
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
    poly <- drop(json_data[[i]][[1]])
  }
  else if (typeof(json_data[[i]]) == "double"){
    if (length(dim(json_data[[i]])) > 3){
      # poly <- drop(json_data[[i]])[1,,]
      poly <- drop(json_data[[i]])
    }
    else{poly <- json_data[[i]][1,,]}
  }
  else{
    break
  }
  indexFront[[i]] <- vector("logical", dim(poly)[1]-1)
  for (j in 1:(dim(poly)[1]-1)){
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
offsetFront <- vector("double", len_json)
for (i in 1:len_json){
  preVal <- FALSE
  arr <- indexFront[[i]]
  for (j in 1:length(arr)){
    if (preVal == FALSE && arr[j]){chunks[i] <- chunks[i] + 1}
    if (preVal && arr[j] == FALSE){false_chunks[i] <- false_chunks[i] + 1}
    preVal <- arr[j]
  }
  # if (arr[1] == TRUE && arr[length(arr)] == TRUE){   # Find number of spots to offset
  #   for (j in 1:length(arr)){
  #     while (arr[j]){
  #       offsetFront[i] <- offsetFront[i] + 1
  #     }
  #     if (!arr[j]){next}
  #   }
  # }
}
table(chunks)
table(false_chunks)

parcelFront <- vector(mode = "list", len_json)     # Keep track of the points that touch the front
for (i in 1:len_json){
  if (checkFront[i]){   # Check whether we need to save any points
    parcelFront[[i]] <- array(dim = c(numFront[i],2))
    index <- 1
    if (typeof(json_data[[i]]) == "list"){
      poly <- drop(json_data[[i]][[1]])
    }
    else if (typeof(json_data[[i]]) == "double"){
      if (length(dim(json_data[[i]])) > 3){
        poly <- drop(json_data[[i]])[1,,]
      }
      else{poly <- json_data[[i]][1,,]}
    }
    for (j in 1:(dim(poly)[1]-1)){
      if (isFront1(poly[j,])){
        parcelFront[[i]][index,] <- poly[j,]
        index <- index + 1
      }
    }
  }
}





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


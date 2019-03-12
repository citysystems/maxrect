# devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)
library(jsonlite)
library(readr)
library(MASS)

json_file <- "epaparcelsnobldg.json"
json_data <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
json_data <- json_data$json_geometry.coordinates
len_json <- length(json_data)

# ring1 and ring2 are arrays of dim [x, 2] AND/OR lists of already merged rings
# Function is recursively to merge rings if list(s) is passed as an input
# returns list of merged rings
merge_rings <- function(ringA, ringB){ 
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
      result <- append(result, merge_rings(ringA[[j]], ringB))
    }
  } 
  
  # Case 2: ringB is a list, ringA is an array
  else if (indexA == 1 && indexB > 1){
    for(k in 1:indexB){
      result <- append(result, merge_rings(ringA, ringB[[k]]))
    }
  }
  
  # Case 3: ringA and ringB are both lists
  else if (indexA > 1 && indexB > 1){
    for(j in 1:indexA){
      for(k in 1:indexB){
        result <- append(result, merge_rings(ringA[[j]], ringB[[k]]))
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
    
    result <- vector("list", len_r2)
    
    for(i in 1:len_r2){
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
    return(result)
  }
  return(result)
}


# Create recursive function to unpack list elements
# Takes a list (maybe of more lists)
# Returns a list of merged arrays
# Account for errors later
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
        result <- merge_rings(result, unpackList(lst[[i]])) # merge onto existing result, recursively call function
      } else if (typeof(lst[[i]]) == "double"){
        arr <- drop(lst[[i]])
        if (length(dim(arr)) == 2 && dim(arr)[2] == 2){   # check that array has R x 2 dimensions
          result <- merge_rings(result, arr)                # merge onto existing result
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
    if(dim(arr)[1] == 2 && dim(arr)[3] == 2) { # if array is [2 x b x 2]
      ring1 <- arr[1,,]
      ring2 <- arr[2,,]
      all_merged_rings[[a]] <- merge_rings(ring1,ring2)
      # all_merged_rings[[a]] <- 1
    }
    else if (dim(arr)[2] == 2){    # if is just one polygon, return that polygon
      all_merged_rings[[a]] <- arr
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

# Test that shows find_lr works with output
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
    for(b in 1:len){
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
                listMaxRect[[a]][[b]] <- lr[[1]]
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
            listMaxRect[[a]][[b]] <- lr[[1]]
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
}

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

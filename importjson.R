library("jsonlite")
library("dplyr")

json_file <- "epaparcelsnobldg.json"
json_data <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
json_data <- json_data$json_geometry.coordinates
len_json <- length(json_data)

test <- drop(json_data[[1]])

all_merged_rings <- vector("list", len_json)
for(a in 1:len_json) {
  all_merged_rings[[a]] <- NA
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
    else {}                                       # otherwise return nothing
  } 
  else if (typeof(json_data[[a]]) == "list") {
    if (length(json_data[[a]]) == 2){ # if list has length 2
      if(xor(typeof(json_data[[a]][[1]]) == "list", typeof(json_data[[a]][[2]]) == "list")){ # get rid of nested list cases
        print(a)
        print("nested case")
      }
      else if (typeof(json_data[[a]][[1]]) == "list" && typeof(json_data[[a]][[2]]) == "list"){ # both lists
        print("special case")
      }
      else{
        arr1 <- drop(arr[[1]])
        arr2 <- drop(arr[[2]])
        if(dim(arr1)[2] == 2 && dim(arr2)[2] == 2){
          ring1 <- arr1
          ring2 <- arr2
          all_merged_rings[[a]] <- merge_rings(ring1,ring2)
          # all_merged_rings[[a]] <- 2
        }
      }
    }
    else{ # otherwise return nothing
    }
  }
}



merge_rings <- function(ring1, ring2){ # ring1 and ring2 are arrays of dim [x, 2] || returns list of merged rings
  len_r1 <- length(ring1[,1])-1  #don't need to consider last point, which is the same as point 1
  len_r2 <- length(ring2[,1])-1  #don't need to consider last point, which is the same as point 1
    
  # find all pairs of closest points
  ring1_closest <- vector("double", len_r1)
  # cycle through each ring1 point
  for(m in 1:len_r1){
    #returns index of closest ring2 point to current ring1 point
    min_dist <- 1e99 #start with a very high number
    min_index <- 0   #start with index 0 (not in the index)
    for (n in 1:len_r2) {
      dx <- abs(ring1[m,1]-ring2[n,1])
      dy <- abs(ring1[m,2]-ring2[n,2])
      dist <- sqrt(dx*dx+dy*dy)
      if (dist < min_dist) {
        min_dist <- dist
        min_index <- n
      }
    }
    # saves index of closest ring2 point
    ring1_closest[m] <- min_index
  }
  
  rings_merged <- vector("list", len_r1)
  
  for(i in 1:len_r1){
    # merge rings for 1 iteration
    ring_merge <- cbind(vector("double", len_r1+len_r2+3),vector("double", len_r1+len_r2+3))
    index_merge <- 1   # keep track of index in ring_merge
    index1 <- i        # keep track of index in ring1
    # add ring1 to ring_merge
    for(m in 1:len_r1){
      ring_merge[index_merge,1] <- ring1[index1,1]  # transfer value
      ring_merge[index_merge,2] <- ring1[index1,2]  # transfer value
      index1 <- index1 %% len_r1 + 1 #increment, if at end of index, loops back to 1
      index_merge <- index_merge + 1 #increment, no need to loop back
    }
    #add last point to close ring1
    ring_merge[index_merge,1] <- ring1[index1,1]
    ring_merge[index_merge,2] <- ring1[index1,2]
    index_merge <- index_merge + 1
    #add ring2 to ring_merge
    index2 <- ring1_closest[index1]
    for(n in 1:len_r2){
      ring_merge[index_merge,1] <- ring2[index2,1]  # transfer value
      ring_merge[index_merge,2] <- ring2[index2,2]  # transfer value
      index2 <- index2 - 1 #increment, if at end of index, loops back to 1
      if (index2 == 0) {index2 <- len_r2}
      index_merge <- index_merge + 1 #increment, no need to loop back
    }
    ring_merge[index_merge,1] <- ring2[index2,1]
    ring_merge[index_merge,2] <- ring2[index2,2]
    index_merge <- index_merge + 1
    ring_merge[index_merge,1] <- ring1[index1,1]
    ring_merge[index_merge,2] <- ring1[index1,2]
    rings_merged[[i]] <- ring_merge
  }
  return(rings_merged)
}

flip_ring1 <- function(merged_ring){
  start_value <- merged_ring[1,] #coordinates of first point
  second_index <- 0              #initialize
  # find index for point that closes ring1, matches first point
  for (n in 2:length(merged_ring[,1])) {
    if (start_value[1] == merged_ring[n,1] && start_value[2] == merged_ring[n,2]) { 
      second_index <- n
      break
    }
  }
  copy <- array(0, dim=c(second_index,2)) # create an array to copy coordinates of ring1
  for (n in 1:second_index){
    copy[n,] <- merged_ring[n,]
  }
  for (n in 1:(second_index-2)){              # loop through copy to flip order of ring1
    merged_ring[n+1,] <- copy[second_index-n,]
  }
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
  copy <- array(0, dim=c(third_index - second_index + 1,2)) # create an array to copy coordinates of ring1
  for (n in second_index:third_index){
    copy[n,] <- merged_ring[n+second_index,]
  }
  for (n in second_index:(third_index-second_index-1)){              # loop through copy to flip order of ring1
    print(n)
    merged_ring[n+second_index,] <- copy[third_index-n,]
  }
  return(merged_ring)
}



# Test that shows find_lr works with output
count_success <- 0
count_error <- 0
error_poly <- array(0, dim=c(1000,2))
index_poly <- 1
for(a in 1:5051) {
  if(typeof(all_merged_rings[[a]]) == "list"){
    len = length(all_merged_rings[[a]])
    for(b in 1:len){
      print(paste(a, ",", b))
      if(a != 10 && b != 15){
        # ctx = initjs()
        poly <- sapply(all_merged_rings[[a]][[b]], round)
        tryCatch({lr = find_lr(ctx, poly)
        count_success <<- count_success + 1
        },
                 error = function(e){
                   print(paste("Error, skipping ", a, ", ", b))
                   error_poly[index_poly,] <<- c(a,b)
                   index_poly <<- index_poly + 1
                   }
        )
      }
    }
  }
}

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

test <- cbind(c(1,2,3,4,5),c(6,7,8,9,10))
test <- flip_order(test, 2, 3)
copy <- array(0, dim=c(4 - 2 + 1, 2))
copy[1,] <- test[2+1-1,]
copy[2,] <- test[2+2-1,]
copy[3,] <- test[2+3-1,]

array[4,] <-  copy[]

View(all_merged_rings[[359]][[3]])
test <- flip_ring1(all_merged_rings[[359]][[3]])
test <- flip_ring2(all_merged_rings[[359]][[3]])
lr = find_lr(ctx, test)
eqscplot(test, type= "l")

plot_call(359,3)

plot_call <- function(index1,index2){
  eqscplot(all_merged_rings[[index1]][[index2]], type= "l")
}

plot_merged <- function(index){
  eqscplot(all_merged_rings[[error_poly[index,1]]][[error_poly[index,2]]], type= "l")
}

x_merge1 <- c(6087144, 6087144, 6087275, 6087275, 6087275, 6087144, 6087165, 6087166, 6087215, 6087214, 6087165, 6087144)
y_merge1 <- c(2000084, 2000134, 2000134, 2000091, 2000084, 2000084, 2000091, 2000130, 2000129, 2000090, 2000091, 2000084)
xy_merge1 <- cbind(x_merge1, y_merge1)
x_merge2 <- all_merged_rings[[359]][[3]][,1]
y_merge2 <- all_merged_rings[[359]][[3]][,2]
xy_merge2 <- cbind(x_merge2, y_merge2)

test <- sapply(xy_merge2, function(x) round(x, 4))


xy_merge1
xy_merge2


eqscplot(xy_merge2, type= "l")
lr = find_lr(ctx, test)
pp = plotrect(lr[[1]])
lines(pp)

lr = find_lr(ctx, cbind(all_merged_rings[[4]][[2]][,1], all_merged_rings[[4]][[2]][,2]))
pp = plotrect(lr[[1]])
lines(pp)

lr = find_lr(ctx, all_merged_rings[[4]][[5]])



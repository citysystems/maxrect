library("jsonlite")

json_file <- "epaparcelsnobldg.json"
json_data <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
json_data <- json_data$json_geometry.coordinates
len_json <- length(json_data)

clean_result <- all_merged_rings

count <- 0

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
      all_merged_rings[[a]] <- 1
      count <- count + 1
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


length(dim(json_data[[5]]))
dim(drop(json_data[[4]][[1]]))[1]


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
      index2 <- index2 %% len_r2 + 1 #increment, if at end of index, loops back to 1
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


# Test that shows find_lr works with output
eqscplot(json_data[[5]][1,,], type= "l")
lr = find_lr(ctx, rings_merged[[4]])
pp = plotrect(lr[[1]])
lines(pp)




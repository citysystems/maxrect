# library("rjson")
library("jsonlite")

json_file <- "epaparcelsnobldgtest.json"
json_data <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
# json_full <- fromJSON(txt=json_file, flatten = TRUE, simplifyDataFrame = TRUE)
View(json_full$json_geometry.coordinates)
ring1 <- json_data$features$geometry.rings[[1]][[1]]
ring2 <- json_data$features$geometry.rings[[1]][[2]]
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

# Test that shows find_lr works with output
eqscplot(rings_merged[[4]], type= "l")
lr = find_lr(ctx, rings_merged[[4]])
pp = plotrect(lr[[1]])
lines(pp)




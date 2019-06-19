
# Input: st_polygon. May contain holes, need to separate those out
largestRect <- function(polygon, print = FALSE, debug = FALSE){
  poly <- st_geometry(polygon)
  if (st_area(poly) < 160){return ("Area less than 160")}
  if (st_is_empty(poly)){return ("Empty geometry")}
  if (!st_is_valid(poly)){return ("Invalid geometry")}
  
  # Extract coordinates
  coord <- as_tibble(st_coordinates(poly))
  if (dim(coord)[1] <= 5){
    return("Polygon has 4 or less points.")
  }
  # Separate out if there are holes
  poly1 <- coord %>% filter(L1 == 1) %>% select(X, Y) %>% unlist() %>% unname() %>% matrix(ncol = 2)
  poly2 <- coord %>% filter(L1 == 2) %>% select(X, Y) %>% unlist() %>% unname() %>% matrix(ncol = 2)
  if (dim(poly2)[1] == 0) {poly2 <- NULL}
  # return(list(poly1,poly2))
  
  if (print){
    color_transparent <- adjustcolor(2, alpha.f = 0.5)
    
    plot(st_geometry(polygon), col = 'green', axes = TRUE)
    result <- findBuildable(poly1, poly2, debug)
    plot(st_geometry(result), col = color_transparent, add = TRUE)
    return(result)
  }
  
  return(findBuildable(poly1, poly2, debug))
}


## New customized algorithm (V2)

tsrects <<- NULL
tsedge <<- NULL
tsparSeg <<- NULL
tspoly1mod <<- NULL
tspoly2mod <<- NULL
tsintersections <<- NULL

findBuildable <- function(poly1, poly2 = NULL, debug = FALSE){
  # Make sure that polygons are not closed
  if (identical(poly1[1,],poly1[nrow(poly1),])){
    poly1 <- poly1[-1,]
  }
  # Check if hole is present
  h <- !is.null(poly2)
  if (h && identical(poly2[1,],poly2[nrow(poly2),])){
    poly2 <- poly2[-1,]
  }
  validRects <- vector(mode = "list", length = 0)
  for (i in 1:nrow(poly1)){
    # print(i)
    if (debug) {print(i)}
    # Polygon is formatted as a 2 x N array of coordinates, no other information. If hole present, hole is in same format.
    # N rows does not includes final row to close polygon
    # Selected edge is poly1[i %% nrow(poly1) + 1,] and poly1[i,]
    # Step 0: rotate polygon so selected edge is horizontal (0 degrees)
    iplus1 <- i%%nrow(poly1) + 1
    angle <- atan2((poly1[iplus1,2]-poly1[i,2]),(poly1[iplus1,1]-poly1[i,1]))
    poly1mod <- rotatePoly(poly1, -angle, poly1[i,])
    if (h) {poly2mod <- rotatePoly(poly2, -angle, poly1[i,])}
    if (debug) { print(poly1mod)}
    # eqscplot(poly1mod, type='l')
    edge <- rbind(poly1mod[i,],poly1mod[iplus1,]) # edge = rbind(c(x1, y1), c(x2, y2))
    vec <- edge[2,] - edge[1,]
    edgeExt <- rbind(edge[1,] - 1000*vec, edge[2,] + 1000*vec)
    
    # if (i == 3){
    #   eqscplot(poly1mod, type = 'l')
    #   lines(edgeExt)
    # }
    
    # Step 1: find midpoint of edge
    midpt <- (poly1mod[iplus1,] + poly1mod[i,]) / 2
    # Step 2: Take 2 coordinates a small normal dist away. ID which one is inside poly
    side1 <- midpt + c(0, 1e-3)
    side2 <- midpt - c(0, 1e-3)    ## ***Note: this algorithm won't work if there is only a tiny sliver where the midpoint of segment is. Hopefully this doesn't pose an issue, or we will need to refine the process further...
    offset <- c(0,0)
    # Case A: side1 is inside polygon and outside of hole
    if (pointInPoly(side1, poly1mod, offset = 0) && (!h || (h && !pointInPoly(side1, poly2mod, offset = 0)))){
      offset <- c(0, 8)
      # Just to check that there isn't an error, see if side1 is also valid. If yes, there is some issue that needs to be resolved.
      if (pointInPoly(side2, poly1mod, offset = 0) && (!h || (h && !pointInPoly(side2, poly2mod, offset = 0)))){
        print("Error: side 1 and 2 both valid")
      }
    }else if(pointInPoly(side2, poly1mod, offset = 0) && (!h || (h && !pointInPoly(side2, poly2mod, offset = 0)))){ # Case B: side2 is inside polygon and outside of hole
      offset <- c(0, -8)
    }else{
      print("Error: both side 1 and 2 invalid. Skip")
      next
    }
    # Step 3: construct parallel line segment ("parSeg") 8 ft away in appropriate direction (we will call the two || lines "the ribbon")
    parSeg <- edgeExt + rbind(offset, offset)
    
    # Step 4: find all intersections between parSeg and other edges (polygon and hole)
    intersections <- vector("numeric",0) # initialize vector to store intersecting X coordinates
    intersections <- c(intersections, midpt[1] - 10, midpt[1] + 10, edge[1,1], edge[1,1] - 20, edge[1,1] + 20, edge[2,1], edge[2,1] - 20, edge[2,1] + 20)
    # intersections <- rbind(intersections, edge[1,], edge[2,])
    for (j in 1:nrow(poly1mod)){
      jplus1 <- j %% nrow(poly1mod) + 1
      nrow(poly1mod)
      seg <- rbind(poly1mod[j,], poly1mod[jplus1,])
      # If line segment intersects with parSeg, add to collection
      if (segmentsIntersect(seg[1,], seg[2,], parSeg[1,], parSeg[2,], endsCount = TRUE, eps = 1) == TRUE){
        intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], parSeg[1,], parSeg[2,])[1])
      }
      # If line segment intersects with edgeExt, add to collection
      if (segmentsIntersect(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,], endsCount = TRUE, eps = 1) == TRUE){
        intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,])[1])
      }
    }
    if (h){
      for (j in 1:nrow(poly2mod)){
        jplus1 <- j %% nrow(poly2mod) + 1
        seg <- rbind(poly2mod[j,], poly2mod[jplus1,])
        # If line segment intersects, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], parSeg[1,], parSeg[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], parSeg[1,], parSeg[2,])[1])
        }
        # If line segment intersects with edgeExt, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,])[1])
        }
      }
    }
    # Step 5: Find all poly vertices inside 8 ft wide ribbon.
    for (j in 1:nrow(poly1mod)){
      pt <- poly1mod[j,]
      # edge = c(x1, y1, x2, y2)
      if ((pt[2] >= edgeExt[1,2] && pt[2] <= parSeg[1,2]) || (pt[2] <= edgeExt[1,2] && pt[2] >= parSeg[1,2])){
        intersections <- c(intersections, pt[1])
      }
    }
    
    if (h) {
      for (j in 1:nrow(poly2mod)){
        pt <- poly2mod[j,]
        if ((pt[2] >= edgeExt[1,2] && pt[2] <= parSeg[1,2]) || (pt[2] <= edgeExt[1,2] && pt[2] >= parSeg[1,2])){
          intersections <- c(intersections, pt[1])
        }
      }
    }
    
    # Get rid of redundant X coordinates
    intersections <- round(intersections, digits = 5)
    intersections <- unique(intersections)
    if (debug) {print(edge, digits = 4)}
    if (debug) {print(parSeg, digits = 4)}
    if (debug) {print(intersections, digits = 4)}
    # Step 6: Construct all possible rectangles
    # Initialize list vector for rectangle coordinates
    rects <- vector(mode = "list", length = 0)
    for (j in 1:(length(intersections)-1)){
      if (debug) {print(paste("j =", j))}
      for (k in (j+1):length(intersections)){
        if (debug) {print(paste("k =", k))}
        x1 <- intersections[j]
        x2 <- intersections[k]
        y1 <- midpt[2]
        y2 <- (midpt + offset)[2]
        if (x1 > x2){ # Make sure x1 is smaller than x2
          temp <- x2
          x2 <- x1
          x1 <- temp
        }
        if (y1 > y2){ # Make sure y1 is smaller than y2
          temp <- y2
          y2 <- y1
          y1 <- temp
        }
        # Quick check: if distance between x1 and x2 is less than 20, then skip
        if (abs(x2-x1) < 20){
          next
        }
        rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
        rects[[length(rects)+1]] <- rect
      }
    }
    # Add a rectangle at the midpoint in the other orientation
    x1 <- midpt[1] - 4
    x2 <- midpt[1] + 4
    y1 <- midpt[2]
    y2 <- midpt[2] + offset[2]/8*20
    rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
    rects[[length(rects)+1]] <- rect
    # Add two rectangle at the corners in the other orientation
    left <- edge[1,1]
    right <- edge[2,1]
    if (edge[1,1] > edge[2,1]){
      left <- edge[2,1]
      right <- edge[1,1]
    }
    x1 <- left
    x2 <- left + 8
    rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
    rects[[length(rects)+1]] <- rect
    x1 <- left + 8
    x2 <- left + 16
    rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
    rects[[length(rects)+1]] <- rect
    x1 <- right - 8
    x2 <- right
    rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
    rects[[length(rects)+1]] <- rect
    x1 <- right - 16
    x2 <- right - 8
    rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
    rects[[length(rects)+1]] <- rect
    if (debug) {print(rects, digits = 10)}
    
    # if (i == 3){
    #   tsrects <<- rects
    #   tsedge <<- edgeExt
    #   tsparSeg <<- parSeg
    #   tspoly1mod <<- poly1mod
    #   if (h){
    #     tspoly2mod <<- poly2mod
    #   }
    #   tsintersections <<- intersections
    #   return()
    # }
    
    
    # Check which rectangles are inside poly and keep them
    if (length(rects) > 0){
      for (j in 1:length(rects)){
        rect <- rects[[j]]
        if (!polyIntersect(rect, poly1mod) && polyInsidePoly(rect, poly1mod, offset = 1e-7, partial = FALSE) && (!h || (h && !polyIntersect(rect, poly2mod) && !polyInsidePoly(rect, poly2mod, offset = 1e-7) && !polyInsidePoly(poly2mod, rect, offset = 1e-7)))){
          # Rotate rectangle back to original orientation
          rect <- rotatePoly(rect, angle, poly1[i,])
          rect <- rbind(rect, rect[1,])
          validRects[[length(validRects)+1]] <- rect
        }
      }
      if (debug) {print(validRects, digits = 4)}
    }
  }
  if (h){
    for (i in 1:nrow(poly2)){
      # print(i)
      if (debug) {print(i)}
      # Polygon is formatted as a 2 x N array of coordinates, no other information. If hole present, hole is in same format.
      # N rows does not includes final row to close polygon
      # Selected edge is poly2[i %% nrow(poly2) + 1,] and poly2[i,]
      # Step 0: rotate hole & polygon so selected hole edge is horizontal (0 degrees)
      iplus1 <- i%%nrow(poly2) + 1
      angle <- atan2((poly2[iplus1,2]-poly2[i,2]),(poly2[iplus1,1]-poly2[i,1]))
      poly1mod <- rotatePoly(poly1, -angle, poly2[i,])
      poly2mod <- rotatePoly(poly2, -angle, poly2[i,])
      edge <- rbind(poly2mod[i,],poly2mod[iplus1,]) # edge = c(x1, y1, x2, y2)
      vec <- edge[2,] - edge[1,]
      edgeExt <- rbind(edge[1,] - 1000*vec, edge[2,] + 1000*vec)
      
      # Step 1: find midpoint of edge
      midpt <- (poly2mod[iplus1,] + poly2mod[i,]) / 2
      # Step 2: Take 2 coordinates a small normal dist away. ID which one is inside poly
      side1 <- midpt + c(0, 1e-3)
      side2 <- midpt - c(0, 1e-3)    ## ***Note: this algorithm won't work if there is only a tiny sliver where the midpoint of segment is. Hopefully this doesn't pose an issue, or we will need to refine the process further...
      offset <- c(0,0)
      # Case A: side1 is inside polygon and outside of hole
      if (pointInPoly(side1, poly1mod, offset = 0) && !pointInPoly(side1, poly2mod, offset = 0)){
        offset <- c(0, 8)
        # Just to check that there isn't an error, see if side1 is also valid. If yes, there is some issue that needs to be resolved.
        if (pointInPoly(side2, poly1mod, offset = 0) && !pointInPoly(side2, poly2mod, offset = 0)){
          print("Error: side 1 and 2 both valid")
        }
      }else if(pointInPoly(side2, poly1mod, offset = 0) && !pointInPoly(side2, poly2mod, offset = 0)){ # Case B: side2 is inside polygon and outside of hole
        offset <- c(0, -8)
      }else {
        print("Error: both side 1 and 2 are invalid. Skip")
        next
      }
      # Step 3: construct parallel line segment ("parSeg") 8 ft away in appropriate direction (we will call the two || lines "the ribbon")
      parSeg <- edgeExt + rbind(offset, offset)
      # Step 4: find all intersections between parSeg and other edges (polygon and hole)
      intersections <- vector("numeric",0) # initialize vector to store intersecting X coordinates
      intersections <- c(intersections, midpt[1] - 10, midpt[1] + 10, edge[1,1], edge[1,1] - 20, edge[1,1] + 20, edge[2,1], edge[2,1] - 20, edge[2,1] + 20)
      for (j in 1:nrow(poly1mod)){
        jplus1 <- j %% nrow(poly1mod) + 1
        seg <- rbind(poly1mod[j,], poly1mod[jplus1,])
        # If line segment intersects with parSeg, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], parSeg[1,], parSeg[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], parSeg[1,], parSeg[2,])[1])
        }
        # If line segment intersects with edgeExt, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,])[1])
        }
      }
      for (j in 1:nrow(poly2mod)){
        jplus1 <- j %% nrow(poly2mod) + 1
        seg <- rbind(poly2mod[j,], poly2mod[jplus1,])
        # If line segment intersects with parSeg, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], parSeg[1,], parSeg[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], parSeg[1,], parSeg[2,])[1])
        }
        # If line segment intersects with edgeExt, add to collection
        if (segmentsIntersect(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,], endsCount = TRUE, eps = 1) == TRUE){
          intersections <- c(intersections, lineIntersection(seg[1,], seg[2,], edgeExt[1,], edgeExt[2,])[1])
        }
      }
      
      # Step 5: Find all poly vertices inside 8 ft wide ribbon.
      for (j in 1:nrow(poly1mod)){
        pt <- poly1mod[j,]
        # edge = c(x1, y1, x2, y2)
        if ((pt[2] >= edgeExt[1,2] && pt[2] <= parSeg[1,2]) || (pt[2] <= edgeExt[1,2] && pt[2] >= parSeg[1,2])){
          intersections <- c(intersections, pt[1])
        }
      }
      for (j in 1:nrow(poly2mod)){
        pt <- poly2mod[j,]
        if ((pt[2] >= edgeExt[1,2] && pt[2] <= parSeg[1,2]) || (pt[2] <= edgeExt[1,2] && pt[2] >= parSeg[1,2])){
          intersections <- c(intersections, pt[1])
        }
      }
      
      # Get rid of redundant X coordinates
      intersections <- round(intersections, digits = 5)
      intersections <- unique(intersections)
      
      # Step 6: Construct all possible rectangles
      # Initialize list vector for rectangle coordinates
      rects <- vector(mode = "list", length = 0)
      for (j in 1:(length(intersections)-1)){
        for (k in (j+1):length(intersections)){
          x1 <- intersections[j]
          x2 <- intersections[k]
          y1 <- midpt[2]
          y2 <- (midpt + offset)[2]
          if (x1 > x2){ # Make sure x1 is smaller than x2
            temp <- x2
            x2 <- x1
            x1 <- temp
          }
          if (y1 > y2){ # Make sure y1 is smaller than y2
            temp <- y2
            y2 <- y1
            y1 <- temp
          }
          # Quick check: if distance between x1 and x2 is less than 20, then skip
          if (abs(x2-x1) < 20){
            next
          }
          rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
          rects[[length(rects)+1]] <- rect
        }
      }
      # Add a rectangle at the midpoint in the other orientation
      x1 <- midpt[1] - 4
      x2 <- midpt[1] + 4
      y1 <- midpt[2]
      y2 <- midpt[2] + offset[2]/8*20
      rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
      rects[[length(rects)+1]] <- rect
      # Add two rectangle at the corners in the other orientation
      left <- edge[1,1]
      right <- edge[2,1]
      if (edge[1,1] > edge[2,1]){
        left <- edge[2,1]
        right <- edge[1,1]
      }
      x1 <- left
      x2 <- left + 8
      rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
      rects[[length(rects)+1]] <- rect
      x1 <- left + 8
      x2 <- left + 16
      rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
      rects[[length(rects)+1]] <- rect
      x1 <- right - 8
      x2 <- right
      rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
      rects[[length(rects)+1]] <- rect
      x1 <- right - 16
      x2 <- right - 8
      rect <- cbind(c(x1, x2, x2, x1), c(y1, y1, y2, y2))
      rects[[length(rects)+1]] <- rect
      
      # Check which rectangles are inside poly and keep them
      if (length(rects) > 0){
        for (j in 1:length(rects)){
          rect <- rects[[j]]
          if (!polyIntersect(rect, poly1mod) && !polyIntersect(rect, poly2mod) && polyInsidePoly(rect, poly1mod, offset = 1e-5, partial = FALSE) && !polyInsidePoly(rect, poly2mod, offset = 1e-5) && !polyInsidePoly(poly2mod, rect, offset = 1e-5)){
            # Rotate rectangle back to original orientation
            rect <- rotatePoly(rect, angle, poly2[i,])
            rect <- rbind(rect, rect[1,])
            validRects[[length(validRects)+1]] <- rect
          }
        }
      }
    }
  }
  if (length(validRects) > 1){
    rects <- vector("list", length(validRects))
    for (j in 1:length(validRects)){
      rects[[j]] <- st_polygon(list(validRects[[j]]))
    }
    merged_rects <- st_sfc(rects) %>% st_cast("POLYGON") %>% st_buffer(0) %>% st_union()
    # merged_rects <- merged_rects + c(minX,minY)
    sf <- st_sfc(merged_rects)
    print("success")
    return(sf)
  } else { # Case 2: Nothing to show
    # sf <- st_sfc(st_polygon())
    # print("empty polygon")
    return("Empty polygon")
  }
}



# ## Based on D3's algorithm
# # Input: st_polygon. May contain holes, need to separate those out
# largestRect <- function(polygon, angles = NULL){
#   ## For keeping all the buildable area
#   rectOptions <- vector("list", 10000)
#   
#   ##### User's input normalization #####
#   aspectRatioStep <- 0.1
#   angleStep <- 5
#   maxAspectRatio <- 15
#   minWidth <- 8
#   minHeight <- 20
#   # tolerance <- 0.02
#   
#   nTries <- 100
#   
#   # Angles
#   if (is.null(angles)){
#     angles <- seq(-90, 90+angleStep, angleStep)*pi/180
#   }
#   else{ # Add all possible "square" orientations for each angle (e.g. 45, 135, -45, -135)
#     angles <- unlist(angles)
#     angles <- unique(c(angles,
#       angles + pi/2,
#       angles + pi,
#       angles - pi/2
#     ))
#     angles <- angles %% (2*pi) %>% sort() %>% unique()
#   }
#   
#   ## print(angles)
#   
#   
#   ##########################################
#   area <- st_area(polygon)
#   
#   # Simple case: just one polygon, no hole, not closed
#   coord <- as_tibble(st_coordinates(polygon))
#   
#   poly1 <- (coord %>% filter(L1 == 1) %>% select(X, Y))[-1,] %>% unlist() %>% unname() %>% matrix(ncol = 2)
#   poly2 <- NULL
#   if (mean(coord$L1) > 1){
#     poly2 <- (coord %>% filter(L1 == 2) %>% select(X, Y))[-1,] %>% unlist() %>% unname() %>% matrix(ncol = 2)
#   }
#   ## print(poly1)
#   ## print(poly2)
#   
#   # get the width of the bounding box of the original polygon to determine tolerance
#   minx <- min(poly1[,1])
#   miny <- min(poly1[,2])
#   maxx <- max(poly1[,1])
#   maxy <- max(poly1[,2])
#   
#   bBox <- rbind(c(minx, miny), c(maxx, miny), c(maxx, maxy), c(minx, maxy))
#   boxWidth <- maxx - minx
#   boxHeight <- maxy - miny
#   
#   # discretize the binary search for optimal width to a resolution of this times the polygon width
#   widthStep <-  min(boxWidth, boxHeight)/100
#   
#   # populate possible center points with random points inside the polygon
#   origins <- array(dim = c(0,2))
#   # get the centroid of the polygon
#   centroid <- st_coordinates(st_centroid(polygon)) %>% as.vector()
#   if (pointInPoly(centroid, poly1) && !pointInPoly(centroid, poly2)){
#     origins <- rbind(origins, centroid)
#   }
#   # get few more points inside the polygon
#   while (nrow(origins) < nTries){
#     rndX <- runif(1) * boxWidth + minx
#     rndY <- runif(1) * boxHeight + miny
#     rndPoint <- c(rndX, rndY)
#     if (pointInPoly(rndPoint, poly1) && !pointInPoly(rndPoint, poly2)){
#       origins <- rbind(origins, rndPoint)
#     }
#   }
#   ## print(origins)
#   
#   maxArea <- 0
#   maxRect <- NULL
#   rectOptions_i <- 1
#   iterMaxRect <- NULL
#   
#   # Mother of for loops
#   for (i in 1:length(angles)){
#     ## print(paste0("i =", i))
#     angleRad <- angles[i]
#     
#     for (j in 1:nrow(origins)){
#       ## print(paste0("j =", j))
#       origOrigin <- origins[j,]
#       origW <- intersectPoints(poly1, origOrigin, angleRad)
#       ## print(paste0("origW =", origW))
#       origH <- intersectPoints(poly1, origOrigin, angleRad + pi/2)
#       ## print(paste0("origH =", origH))
#       modifOrigins <- rbind(colMeans(origW), colMeans(origH)) # Two modified origins, using center of horizontal and verticle ray segments
#       ## print(paste0("modifOrigins =", modifOrigins))
#       iterMaxArea <- 0
#       
#       for (k in 1:nrow(modifOrigins)){
#         ## print(paste0("k =", k))
#         origin <- modifOrigins[k,]
#         W <- intersectPoints(poly1, origin, angleRad)
#         ## print(paste0("W =", W))
#         minSqDistW <- min(squaredDist(origin, W[1,]), squaredDist(origin, W[2,]))
#         maxWidth <- 2*sqrt(minSqDistW)
#         ## print(paste0("maxWidth =", maxWidth))
#         
#         H <- intersectPoints(poly1, origin, angleRad + pi/2)
#         ## print(paste0("H =", H))
#         minSqDistH <- min(squaredDist(origin, H[1,]), squaredDist(origin, H[2,]))
#         maxHeight <- 2*sqrt(minSqDistH)
#         ## print(paste0("maxHeight =", maxHeight))
#         
#         minAspectRatio <- max(1, minWidth/maxHeight)
#         maxAspectRatio <- max(min(maxAspectRatio, maxWidth/minHeight),1)
#         if (minAspectRatio > maxAspectRatio){
#           aRatios <- minAspectRatio
#         }else{
#           aRatios <- seq(abs(minAspectRatio), abs(maxAspectRatio + aspectRatioStep), abs(aspectRatioStep))
#         }
#         ## print(paste0("aRatios =", aRatios))
#         
#         for (l in 1:length(aRatios)){
#           ## print(paste0("l =", l))
#           aRatio <- aRatios[l]
#           # do a binary search to find the max width that works
#           left <- minWidth
#           right <- min(maxWidth, maxHeight*aRatio)
#           
#           while ((right - left) >= widthStep){
#             width <- (left + right) / 2
#             height <- width / aRatio
#             x0 <- origin[1]
#             y0 <- origin[2]
#             rectPoly <- rbind(
#               c(x0 - width/2, y0 - height/2),
#               c(x0 + width/2, y0 - height/2),
#               c(x0 + width/2, y0 + height/2),
#               c(x0 - width/2, y0 + height/2)
#             )
#             ## print(paste0("rectPoly =", rectPoly))
#             rectPoly <- rotatePoly(rectPoly, angleRad, origin)
#             ## print(paste0("Rotated rectPoly =", rectPoly))
#             # (1) rectPoly does not intersect with both poly1 and poly2, (2) rectPoly is inside poly1, (3) rectPoly and poly2 are separate
#             if (!polyIntersect(rectPoly, poly1) && !polyIntersect(rectPoly, poly2) && polyInsidePoly(rectPoly, poly1) && !polyInsidePoly(rectPoly, poly2) && !polyInsidePoly(poly2, rectPoly)){
#               ## print("rectPoly is inside poly")
#               insidePoly <- TRUE
#               
#               if (width * height > iterMaxArea){
#                 iterMaxArea <- width * height
#                 rectPoly <- rbind(rectPoly, rectPoly[1,]) # close the polygon to export
#                 iterMaxRect <- rectPoly
#               }
#               left <- width # increase the width in the binary search
#               # we know that the area is already greater than the maxArea found so far
#               if (width * height > maxArea){
#                 ## print("Max Area")
#                 maxArea <- width * height
#                 maxRect$cx <- x0
#                 maxRect$cy <- y0
#                 maxRect$width <- width
#                 maxRect$height <- height
#                 maxRect$angle
#               }
#             }
#             else{
#               insidePoly <- FALSE
#               right <- width
#             } 
#           }
#         }
#         rectOptions[[rectOptions_i]] <- iterMaxRect
#         rectOptions_i <- rectOptions_i + 1
#       }
#     }
#   }
#   rectOptions <- compact(rectOptions)  # Remove null entries
#   ## print(maxRect)
#   ## print(maxArea)
#   ## print(rectOptions)
#   return (list(maxRect, maxArea, rectOptions))
# }



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Helper functions
#------------------------------------------------------------------------------

# Returns the squared euclidean distance between points a and b
squaredDist <- function(a, b){
  deltax = b[1] - a[1]
  deltay = b[2] - a[2]
  return (deltax * deltax + deltay * deltay)
}

# Checks whether the horizontal ray going through point p intersects the segment p1p2
# Implementation from: http://rosettacode.org/wiki/Ray-casting_algorithm#CoffeeScript
rayIntersectsSegment <- function(p, p1, p2){
  pf <- p + c(0,1e10)
  if (p1[2] < p2[2]){
    a <- p1
    b <- p2
  }
  else{
    a <- p2
    b <- p1
  }
  if (p[2] == b[2] || p[2] == a[2]){
    p[1] <- p[1] + 1e-5
  }
  if (p[2] > b[2] || p[2] < a[2]) {return(FALSE)}
  else if (p[1] > a[1] && p[1] > b[1]) {return(FALSE)}
  else if (p[1] < a[1] && p[1] < b[1]) {return(TRUE)}
  else{
    mAB <- (b[2] - a[2]) / (b[1] - a[1])
    mAP <- (p[2] - a[2]) / (p[1] - a[1])
    return (mAP > mAB)
  }
}


# Checks whether the point p is inside a polygon using the Ray-Casting algorithm
# Implementation from: http://rosettacode.org/wiki/Ray-casting_algorithm#CoffeeScript
# If exclusive, points on the polygon are not counted as inside polygon
pointInPoly <- function(p, poly, exclusive = FALSE, offset = 0, eps = 1e-9){
  if(pointOnPolygon(p, poly, eps)){
    return (!exclusive)
  }
  # Check if point is within bounding box of poly
  # xmax <- max(poly[,1])
  # xmin <- min(poly[,1])
  # ymax <- max(poly[,2])
  # ymin <- min(poly[,2])
  # if (p[1] > xmax + eps || p[1] < xmin - eps || p[2] > ymax + eps || p[1] < ymax - eps){return(FALSE)}
  
  p[2] <- p[2] + offset
  
  # Go through algorithm
  pf <- p + c(1e10,0)
  n <- nrow(poly)
  b <- poly[n,]
  c <- 0
  for (i in 1:n){
    a <- b
    b <- poly[i,]
    if (segmentsIntersect(p, pf, a, b, TRUE, eps)){
      c <- c + 1
    }
  }
  return (c %% 2 != 0)
}

# Checks whether the point p is on a line segment
pointOnSegment <- function(p, p1, p2, eps = 1e-2){
  return (abs(sqrt(squaredDist(p, p1)) + sqrt(squaredDist(p, p2)) - sqrt(squaredDist(p1, p2))) <= eps)
}

# Checks whether the point p is on a polygon
pointOnPolygon <- function(p, poly, eps = 1e-2){
  n <- nrow(poly)
  b <- poly[n,]
  for (i in 1:n){
    a <- b
    b <- poly[i,]
    if (pointOnSegment(p, a, b, eps)){
      return (TRUE)
      }
  }
  return (FALSE)
}

# Checks whether the point p is inside the bounding box of the line segment p1q1
pointInSegmentBox <- function(p, p1, q1, eps = 1e-9){
  # allow for some margins due to numerical errors
  px <- p[1]
  py <- p[2]
  if (px < (min(p1[1], q1[1]) - eps) || 
      px > (max(p1[1], q1[1]) + eps) || 
      py < (min(p1[2], q1[2]) - eps) || 
      py > (max(p1[2], q1[2]) + eps)){return(FALSE)}
  return(TRUE)
}

# Finds the intersection point (if there is one) of the lines p1q1 and p2q2
lineIntersection <- function(p1, q1, p2, q2, eps = 1e-1){
  # allow for some margins due to numerical errors
  # find the intersection point between the two infinite lines
  dx1 <- p1[1] - q1[1]
  dy1 <- p1[2] - q1[2]
  dx2 <- p2[1] - q2[1]
  dy2 <- p2[2] - q2[2]
  denom <- dx1 * dy2 - dy1 * dx2
  if (abs(denom) < eps) {return(NULL)}
  cross1 <- p1[1]*q1[2] - p1[2]*q1[1]
  cross2 <- p2[1]*q2[2] - p2[2]*q2[1]
  
  px <- (cross1*dx2 - cross2*dx1) / denom
  py <- (cross1*dy2 - cross2*dy1) / denom
  return (c(px, py))
}


# Checks whether the line segments p1q1 and p2q2 intersect. If eps = 0, line segment ends also count
# endsCount: if TRUE, then returns true even if intersection happens at one of the ends of a line segment
segmentsIntersect <- function(p1, q1, p2, q2, endsCount = FALSE, eps = 1e-1){
  p <- lineIntersection(p1, q1, p2, q2)
  if (is.na(p) || is.null(p)){return (FALSE)}
  # If intersection point is one of the line points, return false
  
  if (!endsCount){
    if (squaredDist(p, p1) < eps * eps || squaredDist(p, q1) < eps * eps || squaredDist(p, p2) < eps * eps || squaredDist(p, q2) < eps * eps){return (FALSE)}
  }
  return (pointInSegmentBox(p, p1, q1, eps) && pointInSegmentBox(p, p2, q2, eps))
}

# Check if polygon polyA is inside polygon polyB
# Full: If any point of polyA is outside polyB, return FALSE
# Partial: If any point of polyA is inside polyB, return TRUE
polyInsidePoly <- function(polyA, polyB, offset = 0, partial = TRUE){
  n <- nrow(polyA)
  # Get midpoints for polyA edges
  a <- polyA[n,]
  for (i in 1:n){
    b <- a
    a <- polyA[i,]
    polyA <- rbind(polyA, (a + b)/2)
  }
  # print(polyA)
  n <- nrow(polyA)
  # Run through all the vertices, midpoints, and centroid for polyA
  for (i in 1:n){
    # Skip if the points are on polyB
    if (pointOnPolygon(polyA[i,], polyB)){next}
    # If the point is inside and not on polyB, return TRUE. Otherwise, keep going
    else if (!pointInPoly(polyA[i,], polyB, offset, exclusive = TRUE)){
      if (partial){next}
      else{return(FALSE)}
    }
    else {
      if (partial){return(TRUE)}
      else{next}
    }
  }
  # If all points pass, return TRUE
  if (partial) {return(FALSE)}
  else{return (TRUE)}
}

# Check if polygon polyA and polygon polyB intersect
polyIntersect <- function(polyA, polyB, eps = 1e-1){
  nA <- nrow(polyA)
  nB <- nrow(polyB)
  bA <- polyA[nA,]
  
  for (iA in 1:nA){
    aA <- bA
    bA <- polyA[iA,]
    
    bB <- polyB[nB,]
    for (iB in 1:nB){
      aB <- bB
      bB <- polyB[iB,]
      if (segmentsIntersect(aA, bA, aB, bB, FALSE, eps)){
        # print(aA)
        # print(bA)
        # print(aB)
        # print(bB)
        return (TRUE)
      }
    }
  }
  return (FALSE)
}



# Rotates the point p for alpha radians around the origin
rotatePoint <- function(p, alpha, origin){
  if (is.null(origin) || is.na(origin)){
    origin <- c(0,0)
  }
  xshifted <- p[1] - origin[1]
  yshifted <- p[2] - origin[2]
  return (c(cos(alpha) * xshifted - sin(alpha) * yshifted + origin[1],sin(alpha) * xshifted + cos(alpha) * yshifted + origin[2]))
}


# Rotates the polygon for alpha radians around the origin
rotatePoly <- function(poly, alpha, origin){
  for (i in 1:nrow(poly)){
    poly[i,] <- rotatePoint(poly[i,], alpha, origin)
  }
  return(poly)
}

# Gives the 2 closest intersection points between a ray with alpha radians
# from the origin and the polygon. The two points should lie on opposite sides of the origin
intersectPoints <- function(poly, origin, alpha) {
  eps <- 1e-1
  origin <- c(origin[1] + eps * cos(alpha), origin[2] + eps * sin(alpha))
  x0 <- origin[1]
  y0 <- origin[2]
  shiftedOrigin <- c(x0 + cos(alpha), y0 + sin(alpha))
  
  idx <- 1
  if (abs(shiftedOrigin[1] - x0) < eps) {
    idx <- 2
  }
  n <- nrow(poly)
  b <- poly[n,]
  minSqDistLeft <- 1e99
  minSqDistRight <- 1e99
  closestPointLeft <- NULL
  closestPointRight <- NULL
  for (i in 1:n) {
    a <- b
    b <- poly[i,]
    p <- lineIntersection(origin, shiftedOrigin, a, b)
    if (!is.null(p) && !is.na(p) && pointInSegmentBox(p, a, b)) {
      sqDist <- squaredDist(origin, p)
      if (p[idx] < origin[idx]) {
        if (sqDist < minSqDistLeft) {
          minSqDistLeft <- sqDist
          closestPointLeft <- p
        }
      }
      else if (p[idx] > origin[idx]) {
        if (sqDist < minSqDistRight) {
          minSqDistRight <- sqDist
          closestPointRight <- p
        }
      }
    }
  }
  return (rbind(closestPointLeft, closestPointRight))
}

save.image("hackMaxRect.RData")

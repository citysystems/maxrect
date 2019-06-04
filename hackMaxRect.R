## New customized algorithm (V2)


##### Run this for each edge in polygon
# Polygon is formatted as a 2 x N array of coordinates, no other information. If hole present, hole is in same format.
# N rows does not includes final row to close polygon
# Selected edge is poly1[i %% nrow(poly1) + 1,] and poly1[i,]
# Step 0: rotate polygon so selected edge is horizontal (0 degrees)
angle <- atan2((poly1[i%%nrow(poly1)+1,2]-poly1[i,2]),(poly1[i%%nrow(poly1)+1,1]-poly1[i,1]))
poly1 <- rotatePoly(poly1, -angle, poly1[i,])
poly2 <- rotatePoly(poly2, -angle, poly1[i,])
edge <- c(poly1[i,],poly1[i%%nrow(poly1)+1,]) # edge = c(x1, y1, x2, y2)

# Step 1: find midpoint of edge
midpt <- (poly1[i%%nrow(poly1)+1,] + poly1[i,]) / 2
# Step 2: Take 2 coordinates a small normal dist away. ID which one is inside poly
side1 <- midpt + c(0, 0.5)
side2 <- midpt - c(0, 0.5)    ## ***Note: this algorithm won't work if there is only a tiny sliver where the midpoint of segment is. Hopefully this doesn't pose an issue, or we will need to refine the process further...
offset <- c(0,0)
# Case A: side1 is inside polygon and outside of hole
if (pointInPoly(side1, poly1) && !pointInPoly(side1, poly2)){
  offset <- c(0, 8)
  # Just to check that there isn't an error, see if side1 is also valid. If yes, there is some issue that needs to be resolved.
  if (pointInPoly(side2, poly1) && !pointInPoly(side2, poly2)){
    print("Error: side 1 and 2 both valid")
  }
}else{ # Case B: side2 is inside polygon and outside of hole
  offset <- c(0, -8)
}
# Step 3: construct parallel line segment ("parSeg") 8 ft away in appropriate direction (we will call the two || lines "the ribbon")
parSeg <- edge + c(offset, offset)

# Step 4: find all intersections between parSeg and other edges (polygon and hole)
intersections <- array(dim = c(0,2)) # initialize array to store intersecting coordinates (x, y)
for (j in 1:nrow(poly1)){
  seg <- c(poly1[j%%nrow(poly1)+1,], poly1[j%%nrow(poly1)+1,])
  # If line segment intersects, add to collection
  if (lineSegIntersects(seg,parSeg)){
    intersections <- rbind(intersections, seg)
  }
}
for (j in 1:nrow(poly2)){
  seg <- c(poly2[j%%nrow(poly2)+1,], poly2[j%%nrow(poly2)+1,])
  # If line segment intersects, add to collection
  if (lineSegIntersects(seg,parSeg)){
    intersections <- rbind(intersections, seg)
  }
}

# Step 5: Find all poly vertices inside 8 ft wide ribbon.
for (j in 1:nrow(poly1)){
  pt <- poly1[j,]
  # edge = c(x1, y1, x2, y2)
  if (pt[1] >= edge[1] && pt[1] <= edge[3] && pt[2] >= edge[2] && pt[2] <= edge[4]){
    intersections <- rbind(intersections, pt)
  }
}
for (j in 1:nrow(poly2)){
  pt <- poly2[j,]
  if (pt[1] >= edge[1] && pt[1] <= edge[3] && pt[2] >= edge[2] && pt[2] <= edge[4]){
    intersections <- rbind(intersections, pt)
  }
}

# Step 6: Construct all possible rectangles
# Initialize list vector for rectangle coordinates
rects <- vector(mode = "list", length = 0)
for (j in 1:(nrow(intersections)-1)){
  for (k in (j+1):nrow(intersections)){
    x1 <- intersections[j,1]
    x2 <- intersections[k,1]
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

# Check which rectangles are inside poly and keep them
validRects <- vector(mode = "list", length = 0)
for (j in 1:length(rects)){
  rect <- rects[[j]]
  if ()
}


#### Run this again for each edge in hole (if present)



## Based on D3's algorithm
# Input: st_polygon. May contain holes, need to separate those out
largestRect <- function(polygon, angles = NULL){
  ## For keeping all the buildable area
  rectOptions <- vector("list", 10000)
  
  ##### User's input normalization #####
  aspectRatioStep <- 0.1
  angleStep <- 5
  maxAspectRatio <- 15
  minWidth <- 8
  minHeight <- 20
  # tolerance <- 0.02
  
  nTries <- 100
  
  # Angles
  if (is.null(angles)){
    angles <- seq(-90, 90+angleStep, angleStep)*pi/180
  }
  else{ # Add all possible "square" orientations for each angle (e.g. 45, 135, -45, -135)
    angles <- unlist(angles)
    angles <- unique(c(angles,
      angles + pi/2,
      angles + pi,
      angles - pi/2
    ))
    angles <- angles %% (2*pi) %>% sort() %>% unique()
  }
  
  ## print(angles)
  
  
  ##########################################
  area <- st_area(polygon)
  
  # Simple case: just one polygon, no hole, not closed
  coord <- as_tibble(st_coordinates(polygon))
  
  poly1 <- (coord %>% filter(L1 == 1) %>% select(X, Y))[-1,] %>% unlist() %>% unname() %>% matrix(ncol = 2)
  poly2 <- NULL
  if (mean(coord$L1) > 1){
    poly2 <- (coord %>% filter(L1 == 2) %>% select(X, Y))[-1,] %>% unlist() %>% unname() %>% matrix(ncol = 2)
  }
  ## print(poly1)
  ## print(poly2)
  
  # get the width of the bounding box of the original polygon to determine tolerance
  minx <- min(poly1[,1])
  miny <- min(poly1[,2])
  maxx <- max(poly1[,1])
  maxy <- max(poly1[,2])
  
  bBox <- rbind(c(minx, miny), c(maxx, miny), c(maxx, maxy), c(minx, maxy))
  boxWidth <- maxx - minx
  boxHeight <- maxy - miny
  
  # discretize the binary search for optimal width to a resolution of this times the polygon width
  widthStep <-  min(boxWidth, boxHeight)/100
  
  # populate possible center points with random points inside the polygon
  origins <- array(dim = c(0,2))
  # get the centroid of the polygon
  centroid <- st_coordinates(st_centroid(polygon)) %>% as.vector()
  if (pointInPoly(centroid, poly1) && !pointInPoly(centroid, poly2)){
    origins <- rbind(origins, centroid)
  }
  # get few more points inside the polygon
  while (nrow(origins) < nTries){
    rndX <- runif(1) * boxWidth + minx
    rndY <- runif(1) * boxHeight + miny
    rndPoint <- c(rndX, rndY)
    if (pointInPoly(rndPoint, poly1) && !pointInPoly(rndPoint, poly2)){
      origins <- rbind(origins, rndPoint)
    }
  }
  ## print(origins)
  
  maxArea <- 0
  maxRect <- NULL
  rectOptions_i <- 1
  iterMaxRect <- NULL
  
  # Mother of for loops
  for (i in 1:length(angles)){
    ## print(paste0("i =", i))
    angleRad <- angles[i]
    
    for (j in 1:nrow(origins)){
      ## print(paste0("j =", j))
      origOrigin <- origins[j,]
      origW <- intersectPoints(poly1, origOrigin, angleRad)
      ## print(paste0("origW =", origW))
      origH <- intersectPoints(poly1, origOrigin, angleRad + pi/2)
      ## print(paste0("origH =", origH))
      modifOrigins <- rbind(colMeans(origW), colMeans(origH)) # Two modified origins, using center of horizontal and verticle ray segments
      ## print(paste0("modifOrigins =", modifOrigins))
      iterMaxArea <- 0
      
      for (k in 1:nrow(modifOrigins)){
        ## print(paste0("k =", k))
        origin <- modifOrigins[k,]
        W <- intersectPoints(poly1, origin, angleRad)
        ## print(paste0("W =", W))
        minSqDistW <- min(squaredDist(origin, W[1,]), squaredDist(origin, W[2,]))
        maxWidth <- 2*sqrt(minSqDistW)
        ## print(paste0("maxWidth =", maxWidth))
        
        H <- intersectPoints(poly1, origin, angleRad + pi/2)
        ## print(paste0("H =", H))
        minSqDistH <- min(squaredDist(origin, H[1,]), squaredDist(origin, H[2,]))
        maxHeight <- 2*sqrt(minSqDistH)
        ## print(paste0("maxHeight =", maxHeight))
        
        minAspectRatio <- max(1, minWidth/maxHeight)
        maxAspectRatio <- max(min(maxAspectRatio, maxWidth/minHeight),1)
        if (minAspectRatio > maxAspectRatio){
          aRatios <- minAspectRatio
        }else{
          aRatios <- seq(abs(minAspectRatio), abs(maxAspectRatio + aspectRatioStep), abs(aspectRatioStep))
        }
        ## print(paste0("aRatios =", aRatios))
        
        for (l in 1:length(aRatios)){
          ## print(paste0("l =", l))
          aRatio <- aRatios[l]
          # do a binary search to find the max width that works
          left <- minWidth
          right <- min(maxWidth, maxHeight*aRatio)
          
          while ((right - left) >= widthStep){
            width <- (left + right) / 2
            height <- width / aRatio
            x0 <- origin[1]
            y0 <- origin[2]
            rectPoly <- rbind(
              c(x0 - width/2, y0 - height/2),
              c(x0 + width/2, y0 - height/2),
              c(x0 + width/2, y0 + height/2),
              c(x0 - width/2, y0 + height/2)
            )
            ## print(paste0("rectPoly =", rectPoly))
            rectPoly <- rotatePoly(rectPoly, angleRad, origin)
            ## print(paste0("Rotated rectPoly =", rectPoly))
            # (1) rectPoly does not intersect with both poly1 and poly2, (2) rectPoly is inside poly1, (3) rectPoly and poly2 are separate
            if (!polyIntersect(rectPoly, poly1) && !polyIntersect(rectPoly, poly2) && polyInsidePoly(rectPoly, poly1) && !polyInsidePoly(rectPoly, poly2) && !polyInsidePoly(poly2, rectPoly)){
              ## print("rectPoly is inside poly")
              insidePoly <- TRUE
              
              if (width * height > iterMaxArea){
                iterMaxArea <- width * height
                rectPoly <- rbind(rectPoly, rectPoly[1,]) # close the polygon to export
                iterMaxRect <- rectPoly
              }
              left <- width # increase the width in the binary search
              # we know that the area is already greater than the maxArea found so far
              if (width * height > maxArea){
                ## print("Max Area")
                maxArea <- width * height
                maxRect$cx <- x0
                maxRect$cy <- y0
                maxRect$width <- width
                maxRect$height <- height
                maxRect$angle
              }
            }
            else{
              insidePoly <- FALSE
              right <- width
            } 
          }
        }
        rectOptions[[rectOptions_i]] <- iterMaxRect
        rectOptions_i <- rectOptions_i + 1
      }
    }
  }
  rectOptions <- compact(rectOptions)  # Remove null entries
  ## print(maxRect)
  ## print(maxArea)
  ## print(rectOptions)
  return (list(maxRect, maxArea, rectOptions))
}



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Helper functions
#------------------------------------------------------------------------------

# Returns the squared euclidean distance between points a and b
squaredDist <- function(a, b){
  deltax = b[1] - a[1]
  deltay = b[2] - a[1]
  return (deltax * deltax + deltay * deltay)
}

# Checks whether the horizontal ray going through point p intersects the segment p1p2
# Implementation from: http://rosettacode.org/wiki/Ray-casting_algorithm#CoffeeScript
rayIntersectsSegment <- function(p, p1, p2){
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
pointInPoly <- function(p, poly){
  n <- nrow(poly)
  b <- poly[n,]
  c <- 0
  for (i in 1:n){
    a <- b
    b <- poly[i,]
    if (rayIntersectsSegment(p, a, b)){
      c <- c + 1
    }
  }
  return (c %% 2 != 0)
}


# Checks whether the point p is inside the bounding box of the line segment p1q1
pointInSegmentBox <- function(p, p1, q1){
  # allow for some margins due to numerical errors
  eps <- 1e-1
  px <- p[1]
  py <- p[2]
  if (px < (min(p1[1], q1[1]) - eps) || 
      px > (max(p1[1], q1[1]) + eps) || 
      py < (min(p1[2], q1[2]) - eps) || 
      py > (max(p1[2], q1[2]) + eps)){return(FALSE)}
  return(TRUE)
}

# Finds the intersection point (if there is one) of the lines p1q1 and p2q2
lineIntersection <- function(p1, q1, p2, q2){
  # allow for some margins due to numerical errors
  eps <- 1e-1
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


# Checks whether the line segments p1q1 and p2q2 intersect
segmentsIntersect <- function(p1, q1, p2, q2){
  p <- lineIntersection(p1, q1, p2, q2)
  if (is.na(p) || is.null(p)){return (FALSE)}
  return (pointInSegmentBox(p, p1, q1) && pointInSegmentBox(p, p2, q2))
}

# Check if polygon polyA is inside polygon polyB
polyInsidePoly <- function(polyA, polyB){
  return (pointInPoly(polyA[1,], polyB))
}

# Check if polygon polyA and polygon polyB intersect
polyIntersect <- function(polyA, polyB){
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
      if (segmentsIntersect(aA, bA, aB, bB)){
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

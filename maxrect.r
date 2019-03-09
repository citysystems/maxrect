devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)
library(readr)
library(MASS)

setwd("~/GitHub/maxrect")

#maxrectangle package doesn't work for plotrect, so calling function directly
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

ctx = initjs()

#create test polygon
th = seq(0,2*pi,len=11)[-11]
r = runif(10)
xy = cbind(r*cos(th), r*sin(th))
xy = rbind(xy, xy[1,])

#this is plotting of two separate polygons
x_inner <- c(6088887.5778603116,6088924.7236851258,6088925.4778041998,6088888.3320386177,6088887.5778603116)
y_inner <- c(1994253.6826348184,1994253.0360036378,1994296.3581886755,1994297.0048188232,1994253.6826348184)
xy_inner <- cbind(x_inner,y_inner)

x_outer <- c(6088969.3339999998,6088969.5630000085,6088869.5660000127,6088869.3009999944,6088969.2960000103,6088969.3339999998)
y_outer <- c(1994294.6689999939,1994249.6080000065,1994248.8560000029,1994300.7799999823,1994302.0189999985,1994294.6689999939)
xy_outer <- cbind(x_outer,y_outer)

# xy_outer <- read_delim("xy_outer.txt", delim = ",")

# plot the inner and outer ring
eqscplot(xy_outer, type= "l")
lines(xy_inner, type= "l")


#simple coordinates
x_outer2 <- c(0,0,10,10,0)
y_outer2 <- c(0,5,5,0,0)
xy_outer2 <- cbind(x_outer2,y_outer2)

x_inner2 <- c(1,1,3,3,1)
y_inner2 <- c(1,3,3,1,1)
xy_inner2 <- cbind(x_inner2,y_inner2)

# plot the inner and outer ring
eqscplot(xy_outer2, type= "l")
lines(xy_inner2, type= "l")

#Outer: clockwise, inner: counter-clockwise (kind of works?)
x_merge2_cwccw <- c(0,10,10,0,0,1,1,3,3,1,0)
y_merge2_cwccw <- c(5,5,0,0,5,3,1,1,3,3,5)
#Outer: clockwise, inner: clock-wise (kind of works?)
x_merge2_cwcw <- c(0,10,10,0,0,1,3,3,1,1,0)
y_merge2_cwcw <- c(5,5,0,0,5,3,3,1,1,3,5)

xy_merge2_cwccw <- cbind(x_merge2_cwccw,y_merge2_cwccw)
xy_merge2_cwcw <- cbind(x_merge2_cwcw,y_merge2_cwcw)
lr_cwccw = find_lr(ctx, xy_merge2_cwccw)
lr_cwcw = find_lr(ctx, xy_merge2_cwcw)
eqscplot(xy_merge2_cwccw, type= "l")
pp = plotrect(lr_cwccw[[1]])
lines(pp)
# plot the inner and outer ring
eqscplot(xy_inner2, type= "l")
lines(xy_inner, type= "l")


####### 


#this is plotting a merged polygon 
# xy_merge <- read_delim("xy_merge.txt", delim = ",")
x_merge <- c(6088888.3320386177,6088887.5778603116,6088924.7236851258,6088925.4778041998,6088888.3320386177,
             6088869.3009999944,6088969.2960000103,6088969.3339999998,6088969.5630000085,6088869.5660000127,6088869.3009999944,
             6088888.3320386177)
y_merge <- c(1994297.0048188232,1994253.6826348184,1994253.0360036378,1994296.3581886755,1994297.0048188232,
             1994300.7799999823,1994302.0189999985,1994294.6689999939,1994249.6080000065,1994248.8560000029,1994300.7799999823,
             1994297.0048188232)
xy_merge <- cbind(x_merge,y_merge)
View(xy_merge)
eqscplot(xy_merge, type= "l")

# find largest rect
lr = find_lr(ctx, xy_merge)

#plot largest rect, not working
pp = plotrect(lr[[1]])
lines(pp)

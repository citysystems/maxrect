library(devtools)
devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

ctx = initjs() 

th = seq(0,2*pi,len=11)[-11]
r = runif(10)
xy = cbind(r*cos(th), r*sin(th))
xy = rbind(xy, xy[1,])
plot(xy, type="l") 

lr = find_lr(ctx, xy) 


library(js)
library(V8)
library(readr)
# 
# demo <- readLines("largestRect.coffee")
# js <- coffee_compile(demo)
# js_validate_script(js)
# js <- readLines("largestRect.js")
# 
# ct <- v8()
# ct$source("https://requirejs.org/docs/release/2.3.6/minified/require.js")
# ct$source("https://raw.githubusercontent.com/mourner/simplify-js/master/simplify.js")
# ct$source(system.file("simplify.js", package="V8"))
# 
# ct$eval(js)
# ct$console()

##' Init a JS context
##'
##' Init a JS context

initjs <- function(){
  ### initialise a v8 context and load d3, simplify, and the largest rectangle
  ### scripts
  ct = v8()
  ct$eval("window={}")
  ct$source("http://d3js.org/d3.v3.min.js")
  ct$source("https://requirejs.org/docs/release/2.3.6/minified/require.js")
  ct$source("http://d3plus.org/assets/posts/largestRect/lib/simplify.js")
  
  # ct$source("https://raw.githubusercontent.com/angular/angular.js/master/src/minErr.js")
  # ct$source("https://raw.githubusercontent.com/angular/angular.js/master/src/Angular.js")
  ct$source("bundle.js")
  
  # ct$source("https://raw.githubusercontent.com/mourner/simplify-js/master/simplify.js")
  
  ### simplify loads into the window context but the largestRect needs it in main:
  ct$eval("simplify = window.simplify")
  
  
  ### transpile to JS and evaluate
  # lr = coffee_compile(readLines("largestRect.coffee"))
  lr = readLines("largestRect.js")
  ct$eval(lr)
  
  ### return the context
  ct
}
##' Return the largest fitting rectangle
##'
##' Return the largest fitting rectangle
##' @title Largest Fitting Rectangle
##' @param ct a V8 context
##' @param xy a two-column matrix
##' @param options list of options
##' @return rectangle structure
##' @author Barry Rowlingson
##' @examples
##' \dontrun {
##' ctx = initjs()
##' th = seq(0,2*pi,len=11)[-11]
##' r = runif(10)
##' xy = cbind(r*cos(th), r*sin(th))
##' xy = rbind(xy, xy[1,])
##' plot(xy, type="l",asp=1)
##' lr = find_lr(ctx, xy)
##' pp = plotrect(lr[[1]])
##' lines(pp)
##' }
##' @export
##' 
find_lr <- function(ct, xy, options){
  ## xy MUST be a two-column matrix. This transfers it as
  ## an Array of Arrays:
  ct$assign("xy",xy)
  ## call the largest rectangle routine and return the result
  ## TODO: pass options here
  lrect = ct$get("window.largestRect(xy)")
  lrect
}
##' Rotation matrix
##'
##' Rotation Matrix
##' @title Rotation Matrix
##' @param ang rotation angle
##' @return a 2d rotation matrix
##' @author Barry Rowlingson
##' @export
rmat <- function(ang){
  ## rotation matrix
  ang = ang*pi/180
  sa = sin(ang)
  ca = cos(ang)
  rbind(c(ca,-sa),c(sa,ca))
}

##' Convert rectangle structure to coordinates for plotting
##'
##' Convert rectangle structure to coordinates for plotting
##' @title .. content for \details{} ..
##' @param lr1 a rectangle structure
##' @return a two-column matrix
##' @author Barry Rowlingson
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

###
### example
###

# ct = initjs()
# # then repeat:
#   xy = rbind(c(0,0),c(1,0),c(0.5,1),c(0,0))
#   lr = find_lr(ct, xy)
#   pp = plotrect(lr[[1]])
#   plot(xy, type="l",asp=1)
#   lines(pp)
# # (the context `ct` can be reused)
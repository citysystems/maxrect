devtools::install_git('https://gitlab.com/b-rowlingson/maxrectangle')

library(maxrectangle)

## Line by line inspection, making sure there's no errors

#ct = v8()
#ct$eval("window={}")
#ct$source("http://d3js.org/d3.v3.min.js")
#ct$source("http://d3plus.org/assets/posts/largestRect/lib/simplify.js")
#ct$eval("simplify = window.simplify")
#lr = coffee_compile(readLines(system.file("js/largestRect.coffee", 
#                                          package = "maxrectangle")))
#ct$eval(lr)

# example

# init context
ctx = initjs()

#create test polygon
th = seq(0,2*pi,len=11)[-11] 
r = runif(10) 
xy = cbind(r*cos(th), r*sin(th)) 
xy = rbind(xy, xy[1,]) 

#plot polygon
plot(xy, type= "l")

# find largest rect
lr = find_lr(ctx, xy)

#plot largest rect, not working

pp = plotrect(lr[[1]])
lines(pp)


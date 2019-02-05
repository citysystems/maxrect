library(js)

demo <- readLines("largestRect.coffee")
js <- coffee_compile(demo)

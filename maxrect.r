
# Function to install, load packages as appropiate.
# Takes package name as argument as character '' or string
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Packages
pkgTest("js")
pkgTest("sf")

#-------------------------------------------------------------------

demo <- readLines("largestRect.coffee")
js <- coffee_compile(demo)


# Read test parcel

epaparcel_test<- st_read("epaparcelsnobldgtest")

epaparcel_nobldg <- st_read("epaparcelsnobldg")

epaparcel<- st_read("epaparcels")

extplot(st_geometry(epaparcel))

plot(st_geometry(epaparcel_nobldg))

plot(st_geometry(epaparcel_test))

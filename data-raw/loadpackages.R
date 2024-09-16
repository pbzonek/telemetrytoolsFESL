#load packages
library(roxygen2)
library(devtools)

#Update namespace
#devtools::document() #supersceded by roxygenize
roxygen2::roxygenize()

#load package
devtools::load_all()

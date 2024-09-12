#load packages
library(roxygen2)
library(devtools)

#Update namespace
devtools::document() #supersceded by roxygenize

#load package
devtools::load_all()

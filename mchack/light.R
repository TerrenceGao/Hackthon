
library(devtools)
needs(httpuv)
needs(rgdal)
needs(geojsonio)
needs(rgeos)
options(devtools.install.args = "--no-multiarch") # if you have 64-bit R only, you can skip this
install_github("Microsoft/LightGBM", subdir = "R-package",force=T)
                                                
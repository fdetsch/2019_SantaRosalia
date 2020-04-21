#' ---
#' title: 'Santa Rosalia: Map Georectification'
#' author: "[Florian Detsch](mailto:fdetsch@web.de)"
#' output:
#'   html_notebook:
#'     code_folding: show
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     collapsed: false
#' ---
#+ setup, include=FALSE
knitr::opts_chunk[['set']](collapse=FALSE, message=FALSE, warning=FALSE, prompt=FALSE)

### 0 SETUP ====

## load packages
library(sf)
library(raster)
library(mapview)
library(graticule)

## parallelization
library(parallel)
cl = makePSOCKcluster(3L)
jnk = clusterEvalQ(cl, library(raster))

## import map template
template = brick("02_docu/aoi.png")


### 1 PREREQUISITES ====

### . area of interest (aoi) ----

aoi = st_as_sf(
  data.frame(
    lon = c(-112.266944, -111.980833)
    , lat = c(27.338889, 26.891667)
    , venue = c("Santa Rosalía", "Heroica Mulegé")
  )
  , coords = c("lon", "lat")
  , crs = 4326
)

## interactive view
m = mapview(
  aoi
  , map.types = mapviewGetOption("basemaps")[c(3, 1:2, 4:5)]
  , legend = FALSE
)
m

## project to utm zone 12n for later use, see https://epsg.io/4485
aoi_utm = st_transform(
  aoi
  , crs = 4485
)


### . graticules ----
### (https://cran.r-project.org/web/packages/graticule/vignettes/graticule.html)

## specify meridians and parallels
lons = seq(-112.8, -110.2, 0.1)
lats = seq(26.5, 28, 0.1)

## build coastlines
grat = graticule(lons, lats, xlim = range(lons), ylim = range(lats))
grat = st_as_sf(
  grat
)


### . coastlines ----

# ## reclassify into black (ie. [0;0;0]) and white pixels (ie. [255;255;255])
# rcl = reclassify(
#   template[[1:3]]
#   , rcl = matrix(
#     c(0, 25, 0
#       , 25, 255, 255)
#     , byrow = TRUE
#     , ncol = 3
#   )
# )
# 
# w = matrix(rep(1, 9), ncol = 3)
# clusterExport(cl, "w")
# fcl = stack(
#   parLapply(cl, unstack(rcl), function(i) {
#     fcl1 = i
#     for (j in 1:3) {
#       fcl1 = focal(fcl1, w = w, fun = modal, na.rm = TRUE)
#     }
#     return(fcl1)
#   })
# )
# 
# fcl = parLapply(
#   cl
#   , unstack(rcl)
#   , function(i) {
#     focal(
#       i
#       , w = matrix(rep(1/25, 25), ncol = 5)
#       , fun = modal
#     )
#   }
# )
# 
# plot(focal(template[[1]], w = matrix(rep(1/3, 9), ncol = 3), fun = modal))
# sum(apply(as.matrix(template)[, 1:3], 1, function(x) all(x == 0)))
# 
# extent(template) = c(xmin = -112.808367, xmax = -110.267684, ymin = 26.529618, ymax = 27.991675)
# projection(template) = "+init=epsg:4326"
# 
# ## coastal data from gshhs
# ## import gshhs high-res coastlines
# coasts = st_read(
#   "00_basedata/001_data/gshhg/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp"
#   , quiet = TRUE
# )
# 
# coasts_crp = st_intersection(
#   st_as_sfc(
#     st_bbox(
#       grat
#     )
#   )
#   , coasts
# )

## coastal data from gadm
coasts = getData(
  country = "MEX"
  , level = 0
  , path = "00_basedata/001_data/gadm"
)

coasts = st_intersection(
  st_as_sfc(
    st_bbox(
      grat
    )
  )
  , st_as_sf(
    coasts
  )
)

plot(
  st_union(
    grat
  )
  , col = "grey"
)

plot(
  coasts
  , col = "lightgrey"
  , add = TRUE
)

deg1 = st_read(
  "~/../Downloads/ne_10m_graticules_1/ne_10m_graticules_1.shp"
  , quiet = TRUE
)
deg1 = st_intersection(
  st_as_sfc(
    st_bbox(
      grat
    )
  )
  , deg1
)
plot(
  deg1
  , col = "orange"
  , add = TRUE
)

plot(
  aoi
  , pch = 20
  , col = "darkred"
  , fill = "darkred"
  , cex = 1.8
  , add = TRUE
)


### 2 MAP DIGITIZATION ====

raster::getData()

#'
#' ### ZZ. Final things last
#'
#' <details><summary>Session info (click to view)</summary>
devtools::session_info()
#' </details>

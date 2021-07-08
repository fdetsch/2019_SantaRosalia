library(sf)
library(openxlsx)
library(poorman)
library(DStoolboxLite)
library(raster)
library(mapview)


### . fishing sites ----

fish = st_read(
  "01_analysis/0101_data/fishing_sites.shp"
)


### . catch figures ----

## fishing trips
trips = read.xlsx(
  "00_basedata/001_data/03_04_21_SITIOS_PESCA.xlsx"
  , sheet = 2
) %>% 
  mutate(
    `SITIO.DE.PESCA` = practicalNames(`SITIO.DE.PESCA`)
  )

names(trips) = practicalNames(names(trips))

## catch figures
figures = read.xlsx(
  "00_basedata/001_data/03_04_21_SITIOS_PESCA.xlsx"
  , sheet = 1
  , rowNames = TRUE
)

names(figures) = practicalNames(names(figures))
rownames(figures) = practicalNames(rownames(figures))

## check site consistency
dfs = Map(
  function(x, y) {
    setdiff(x, y)
  }
  , list(
    trips$sitio_de_pesca
    , names(figures)
  )
  , list(
    names(figures)
    , trips$sitio_de_pesca
  )
)

if (any(lengths(dfs) > 0)) {
  stop("Fishing sites do not line up.")
}


### . coastline ----

coasts = getData(
  country = "MEX"
  , level = 0
  , path = "00_basedata/001_data/gadm"
)

crp = st_crop(
  fish
  , st_as_sf(coasts)
)

fish = st_difference(
  fish
  , st_as_sf(coasts)
)[, names(fish)]

# st_write(
#   fish
#   , dsn = "01_analysis/0101_data/fishing_sites_clipped"
#   , driver = "ESRI Shapefile"
# )


## CPUE ====

### site-wise ----

per_site = Map(
  function(x, y) {
    # species-level
    cpue_spl = round(figures[[x]] / y * 100, digits = 1)
    # site-level
    cpue_stl = sum(cpue_spl, na.rm = TRUE)
    
    list(
      "species" = cpue_spl
      , "site" = cpue_stl
    )
  }
  , trips$sitio_de_pesca
  , trips$numero_de_veces_que_se_pesco
)

## merge info
out = rbind(
  do.call(
    cbind
    , lapply(
      per_site
      , "[["
      , "species"
    )
  )
  , sapply(
    per_site
    , "[["
    , "site"
  )
)

rownames(out) = c(
  rownames(figures)
  , "total"
)

## spatial display
Map(
  function(x, y) {
    setdiff(x, y)
  }
  , list(
    fish$name
    , trips$sitio_de_pesca
  )
  , list(
    trips$sitio_de_pesca
    , fish$name
  )
)

popups = lapply(
  1:nrow(fish)
  , function(i) {
    if (!fish$name[i] %in% colnames(out)) {
      return(NULL)
    }
    dat = t(data.frame(cpue = na.omit(out[, fish$name[i]])))
    dat = fish[i, "name"] %>% 
      cbind(dat)
    
    leafpop::popupTable(
      dat
      , feature.id = FALSE
      , row.numbers = FALSE
    )
  }
)

tmp = out
tmp[is.na(tmp)] = 0
write.csv(
  tmp
  , "03_results/cpue_sites.csv"
  , quote = FALSE
)

totals = do.call(
  rbind
  , lapply(
    1:nrow(fish)
    , function(i) {
      if (!fish$name[i] %in% colnames(out)) {
        return(NULL)
      }
      dat = t(data.frame(cpue = na.omit(out[, fish$name[i]])))
      fish[i, "name"] %>% 
        cbind(
          total = dat[
            , "total"
          ]
        )
    }
  )
)

m = mapview(
  totals[, "total"]
  , layer.name = "cpue"
  , popup = Filter(
    Negate(
      is.null
    )
    , popups
  )
  , legend = FALSE
  , col = "grey65"
  , col.regions = viridis::plasma
  , map.types = mapviewGetOption("basemaps")[c(5, 1:4)]
)

mapshot(
  m
  , url = "03_results/cpue_sites.html"
)


### global ----

n_trips = sum(trips$numero_de_veces_que_se_pesco)
out_glob = data.frame(
  sapply(
    1:nrow(figures)
    , function(i) {
      round(sum(figures[i, ], na.rm = TRUE) / n_trips * 100, digits = 1)
    }
  )
)

out_glob = rbind(out_glob, colSums(out_glob))
rownames(out_glob) = c(rownames(figures), "total")
names(out_glob) = "cpue"

write.csv(
  out_glob
  , "03_results/cpue_global.csv"
  , quote = FALSE
)

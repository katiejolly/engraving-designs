library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")


metro_streets <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        primary_secondary_roads(x)
      })
    )
  } else {
    tr <- primary_secondary_roads(x)
  }
  
  # Now, find out which primary_secondary_roads are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
  
}


msp_streets <- metro_streets("Minneapolis")



ffx_places <- st_read("https://opendata.arcgis.com/datasets/64191f811f284007b27c6d29a9266411_3.geojson")
mclean <- ffx_places %>%
  filter(NAME == "McLean") %>%
  st_transform(26917)


ffx_roads <- roads(state = "VA", county = "Fairfax County") %>%
  st_transform(26917)


mcl_roads <- st_crop(ffx_roads, st_bbox(mclean))

library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

##################################################### mclean roads

# metro_streets <- function(metro_name) {
#   
#   # First, identify which states intersect the metro area using the
#   # `states` function in tigris
#   st <- states(cb = TRUE)
#   cb <- core_based_statistical_areas(cb = TRUE)
#   metro <- filter(cb, grepl(metro_name, NAME))
#   
#   stcodes <- st[metro,]$STATEFP
#   
#   # Then, fetch the tracts, using rbind_tigris if there is more
#   # than one state
#   if (length(stcodes) > 1) {
#     tr <- rbind_tigris(
#       map(stcodes, function(x) {
#         roads(x)
#       })
#     )
#   } else {
#     tr <- roads(x)
#   }
#   
#   # Now, find out which primary_secondary_roads are within the metro area
#   within <- st_within(tr, metro)
#   
#   within_lgl <- map_lgl(within, function(x) {
#     if (length(x) == 1) {
#       return(TRUE)
#     } else {
#       return(FALSE)
#     }
#   })
#   
#   # Finally, subset and return the output
#   output <- tr[within_lgl,]
#   
#   return(output)
#   
# }
# 
# 
# msp_streets <- metro_streets("Minneapolis")



ffx_places <- st_read("https://opendata.arcgis.com/datasets/64191f811f284007b27c6d29a9266411_3.geojson")
mclean <- ffx_places %>%
  filter(NAME == "McLean") %>%
  st_transform(26917)


ffx_roads <- roads(state = "VA", county = "Fairfax County") %>%
  st_transform(26917)


mcl_roads <- st_crop(ffx_roads, st_bbox(mclean))

mcl_point <- st_point(c(832758.2, 4315754)) %>%
  st_sfc(crs = 26917)


ggplot() +
  geom_sf(data = mcl_roads, color = "gray80") +
  geom_sf(data = mcl_point, color = "#D8973C") + 
  geom_sf(data = st_buffer(mcl_point, 800),
          color = "#e83778", fill = NA) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

cropped_mcl_roads <- st_intersection(mcl_roads, st_buffer(mcl_point, 550))


ggplot(cropped_mcl_roads) +
  geom_sf(color = "#515b72")  +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


# ggsave("svg/brookhaven.svg")


########################################################################## regional parks

regional_parks <- st_read("data/plan_parks_regional.gpkg")

ggplot() +
  geom_sf(data = regional_parks %>% group_by(COUNTY_ID, PARKNAME) %>% count(), fill = "#7EB2AF", color = NA) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# ggsave("svg/regional_parks.svg")


########################################################################### water mn


water_lakes_rivers <- st_read("data/water_lakes_rivers.gpkg")

ggplot() +
  geom_sf(data = water_lakes_rivers %>% filter(NAME_DNR == "Mississippi"), fill = "#1B4965", color = NA) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# ggsave("svg/mississippi_river.svg")



########################################## bike rte fairfax cnty

bike_ffx <- st_read("https://opendata.arcgis.com/datasets/0dacd6f1e697469a81d6f7292a78d30e_16.geojson")

bike_fc <- st_read("https://opendata.arcgis.com/datasets/42b3063a47f04fee9385b88b86b00791_1.geojson")

wod <- bike_fc %>% filter(Name == "W&OD Trail")

ggplot(wod) +
  geom_sf(color = "#D05353") +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))



################################ pimmit

water_ffx <- st_read("https://opendata.arcgis.com/datasets/2ffa8ec68ffb4f46a9bc9e471c012783_4.geojson")


pimmit <- water_ffx %>%
  filter(grepl("Pimmit", NAME, ignore.case = TRUE))

ggplot() +
  geom_sf(data = pimmit, color = "#1B4965") +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# ggsave("svg/pimmit.svg")

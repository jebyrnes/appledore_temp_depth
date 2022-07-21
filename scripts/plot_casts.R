library(readr)
library(dplyr)
library(ggplot2)


dat <- read_csv("data/casts_with_loc.csv") |>
  filter(!is.na(Location)) 


ggplot(dat %>% filter(updown == "up"), 
       aes(y = `Temp(°C)`,
           x = `Depth(m)`)) +
  coord_flip() +
  scale_x_reverse() +
  geom_line(size = 1) +
  facet_wrap(vars(id,Location)) +
  theme_bw()


### With geofacets
library(geofacet)

mygrid <- data.frame(
  code = c("Transect9	1", "DevilsDanceFloor", "Transect14", "Transect7a", "Transect7", "NorwegianCove", "Transect20", "BabbsCove", "DevilsGlen", "Transect28", "SmithsCove", "Transect25", "HalftideLedges"),
  name = c("Transect9	1", "DevilsDanceFloor", "Transect14", "Transect7a", "Transect7", "NorwegianCove", "Transect20", "BabbsCove", "DevilsGlen", "Transect28", "SmithsCove", "Transect25", "HalftideLedges"),
  row = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6),
  col = c(2, 3, 4, 1, 1, 4, 5, 2, 5, 1, 1, 3, 4),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)


ggplot(dat %>% filter(updown == "up"), 
       aes(y = `Temp(°C)`,
           x = `Depth(m)`)) +
  coord_flip() +
  scale_x_reverse() +
  geom_line(size = 1) +
  facet_geo(vars(Location), grid = mygrid) +
  theme_bw()

# A script to generate a banner for the @cougrstats Twitter account using data
# about Whitman County

library(tidycensus)
library(tidyverse)
library(viridis)
library(OpenStreetMap)
library(ggpubr)
library(ggimage)


# Population map ----------------------------------------------------------

census_api_key(key = "c46052a0d499cb61b7c9c71c690829203abdccce")

# Get a basemap
# Options: https://www.r-bloggers.com/the-openstreetmap-package-opens-up/
base_map <- openmap(upperLeft = c(46.37, -118.4),
                    lowerRight = c(47.3, -116.9),
                    type = 'osm') %>%
  openproj()

terrain_map <- openmap(upperLeft = c(46.37, -118.4),
                       lowerRight = c(47.3, -116.9),
                    type = 'stamen-terrain') %>%
  openproj()

# Get census population data
pop_data <- get_decennial(geography = "tract",
                          year = 2010,
                          variables = "P001001",
                          state = "WA",
                          county = "Whitman",
                          geometry = TRUE)

# Plot census data over OpenStreetMap data
pop_map <- autoplot(base_map) +
  geom_sf(data = pop_data,
          inherit.aes = FALSE, # This is needed to overlay
          aes(fill = value), color = "grey2", alpha = 0.7) +
  scale_fill_viridis(option = "inferno", direction = -1,
                     limits = c(0, 8000)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

pop_map


# iNaturalist map ---------------------------------------------------------

pullman_animals <- read.csv(file = "inaturalist_animals_pullman/observations-60968.csv",
                         stringsAsFactors = FALSE)

whitman <- sf::st_as_sf(tigris::tracts(state = "Washington",
                                       county = "Whitman"))

animal_map <- autoplot(terrain_map) +
  geom_sf(data = whitman, inherit.aes = FALSE,
          fill = NA) +
  geom_hex(data = filter(pullman_animals, coordinates_obscured == "false"),
           aes(x = longitude, y = latitude)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

animal_map


# Ag plot -----------------------------------------------------------------

# Spring wheat data from
# https://www.nass.usda.gov/Statistics_by_State/Washington/Publications/Historic_Data/index.php
spring <- read.csv(file = "spring_wheat_historical_washington.csv",
                  stringsAsFactors = FALSE) %>%
  filter(County == "Whitman",
         Year >= 1990)

img <- tiff::readTIFF("wheat_ear_19617.tif")

wheat_plot <- ggplot() +
  background_image(img) +
  geom_emoji(data = spring,
             aes(x = factor(Year), y = Harvested, image = "1f69c")) +
  xlab("Year") +
  annotate(geom = "text", x = 10.75, y = 100, label = "Clipart courtesy FCIT",
           size = 3, color = "grey10") +
  scale_x_discrete(breaks = c("1990", "1992", "1994", "1996", "1998", "2000",
                              "2002")) +
  scale_y_continuous(labels = scales::comma) +
  theme_pubr()


# Combine into banner -----------------------------------------------------

arrange_figs <- ggarrange(pop_map, wheat_plot, animal_map, ncol = 3,
                          nrow = 1)

arrange_figs

ggsave(filename = "cougr_header.png", plot = arrange_figs, device = "png",
       units = "in", width = 15.625, height = 5.208)


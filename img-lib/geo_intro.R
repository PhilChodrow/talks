library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(ggtext)
library(gganimate)

theme_set(theme_void())

world_map <- maps::map("state", fill = TRUE)

world <- sf::st_as_sf(world_map)

coords <- tibble(
	city = c("", "Staunton", "Swarthmore", "Boston", "Los Angeles", "Middlebury"),
	years = c("0", "17 years", "4 years",  "7 years", "2 years", "0 years"),
	state = c("", "VA", "PA",  "MA", "CA", "VT"),
	lat  = c(0, 38.1496, 39.9526,  42.3601, 34.0522, 44.00822),
	lon  = c(0, -79.0717, -75.1652 , -71.0589, -118.2437, -73.17585),
	label_lat = c(0, 37.3, 43.4926 , 40.3601, 31.2522, 47.60822),
	label_lon = c(0, -79.3717, -75,  -62.5, -125.2437, -62.17585),
	arrow_offset_lat = c(0, 0, 0,  -.4, .4, NA),
	arrow_offset_lon = c(0, -1, -1, 1, -1.2, NA),
	description = c("",
									"Grew up!",
									"College<br>Shenanigans",
									"Married!<br>Nonprofit<br>PhD<br>Aikido",
									"Sunshine<br>Avocados<br>Pandemic =(",
									"Hi!<br>New friends"),
	highlight = c(.4, .4, .4,  .4, .4, 1.5), 
	fill = c("#a8cff1", "#a8cff1",  "#a8cff1", "#a8cff1", "#a8cff1", "#002f6c"),
	color = c("black", "black", "black",  "black", "black", "white")
)

coords <- coords %>%
	mutate(label = glue::glue("**{city}, {state}**<br>*{description}*"))




coord_cxns <- coords %>%
	mutate(
		# need this to create transition state ----
		city_order = row_number() + 1,
		# where I moved to next, for curved arrows ----
		lat_next = lead(lat),
		lon_next = lead(lon),
		label_lat_next = lead(label_lat),
		label_lon_next = lead(label_lon),
		# label to show in plot, styled using ggtext ---
		label = glue::glue("**{city}, {state}**<br>*{description}*"),
		# label of next location ----
		label_next = lead(label)
	)

coord_cxns <- coord_cxns %>%
		# get first row of residence ----
	slice(1) %>%
		# manually modify for plotting ----
	mutate(
		city_order = 1,
		label_next = label,
		lat_next = lat,
		long_next = lon,
		label_lat_next = label_lat,
		label_long_next = label_lon,
	) %>%
		# combine with all other residences ----
	bind_rows(coord_cxns) %>%
		# last (7th) row irrelevant ----
	slice(1:8) %>%
		# keep what we need ----
	dplyr::select(city_order, lat, lon, label_lat, label_lon, lat_next, lon_next, label_lat_next, label_lon_next, label_next, arrow_offset_lat, arrow_offset_lon, highlight, fill) %>%
	mutate(fact = row_number())


plot <- ggplot(data = world) +
	geom_sf(fill = "#b1ccbe", color = "grey55") +
	xlab("Longitude") + ylab("Latitude") +
	geom_curve(data = coord_cxns %>% slice(-1),
						 aes(y = lat,
						 		x = lon,
						 		yend = lat_next + arrow_offset_lat,
						 		xend = lon_next + arrow_offset_lon,
						 		group = seq_along(city_order)),
						 color = "gray30",
						 curvature = -0.47,
						 arrow = arrow(type = "closed", length = unit(0.02, "npc")),
						 size  = 0.5) +
	geom_point(data = coords,
						 aes(x = lon, y = lat, fill = fill, color = color),
						 size = 4,
						 pch = 21) +
	geom_richtext(
		data = coords,
		aes(
			x     = ifelse(label_lon < -100, label_lon+1, label_lon - 1),
			y     = label_lat + 1,
			label = label,
			vjust = "top",
			hjust = ifelse(label_lon < -100, 0, 1),
			# group is used to create the transition ----
			# group = seq_along(city_order),
			fill = fill, 
			color = color
		),
		size = 2.6,
		# R ladies purple ----
		# R ladies font used in xaringan theme ----
		family = "Lato"
	) +
	xlim(-125, -60) +
	ylim(25, 55) +
	guides(color = FALSE, fill = FALSE) +
	scale_color_identity() +
	scale_fill_identity() 

plot

ggsave("geo-intro-light-bg.png", width = 6, height = 4, bg = "transparent")
# ggsave("geo-intro.png", width = 6, height = 4, bg = "transparent")

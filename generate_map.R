
# This file generates a map of cities I've lived.


library(ggplot2)
library(dplyr)
library(sf)
library(rworldmap)
library(stplanr)
library(emojifont)

emojifont::load.fontawesome()
uni <- fontawesome('fa-university')
work <- fontawesome('fa-briefcase')

world <- getMap(resolution = "high") %>% 
  st_as_sf()

cities <- st_sfc(
  st_point(c(8.4515, 49.5252)), # Mannheim
  st_point(c(7.2666, 43.7444)), # Nice
  st_point(c(6.5800, 46.5486)), # Lausanne
  st_point(c(10.6742, 60.0170)), # Oslo
  st_point(c(5.32823, 60.38989)) # Bergen
) %>%
  st_sf(
    name = c(
      'Mannheim, Germany',
      'Nice, France', 
      "Lausanne, Switzerland", 
      "Oslo, Norway", 
      "Bergen, Norway"
    )
  ) %>%
  st_set_crs("+proj=longlat") %>%
  mutate(label = c(rep(uni,4), paste0(uni, work))) %>% 
  distinct()

col <- "navyblue"

p <- 
  ggplot(data = world) +
  geom_sf(color = 'white', lwd = 0.1) +
  xlab("") + 
  ylab("") +
  geom_sf_text(
    data = cities,
    aes(label = label), 
    size = 5,
    nudge_y = 0.50,
    color = col,
    family = "fontawesome-webfont"
  ) +
  geom_sf_label(
    data = cities,
    aes(label = name), 
    # position = "jitter",
    nudge_y = -0.50,
    size = 3,
    color = col
  ) +
  coord_sf(crs = "+proj=longlat", xlim = c(-32, 45), ylim = c(40, 65)) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = 'transparent'),
    plot.margin = margin(-1, 0, -1, 0, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggplot2::ggsave("cities_lived.png", plot = p)

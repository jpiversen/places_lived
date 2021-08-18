
# This file generates a map of cities I've lived.

library(ggplot2)
library(dplyr)
library(sf)
library(rworldmap)
library(stplanr)
library(emojifont)
library(tibble)
library(lubridate)
library(gganimate)


emojifont::load.fontawesome()
uni <- fontawesome('fa-university')
work <- fontawesome('fa-briefcase')

world <- getMap(resolution = "high") %>% 
  st_as_sf()



cities <- tribble(
  ~name,       ~lon,   ~lat,
  "Mannheim",  8.4515, 49.5252,
  "Nice",      7.2666, 43.7444,
  "Lausanne",  6.5800, 46.5486,
  "Oslo",     10.6742, 60.0170,
  "Bergen",    5.3282, 60.3899
)


been <-
  tribble(
    ~name,       ~from,         ~what,
    "Bergen",    "2012-08-01", "BSc Business at BI Norwegian Business School",
    "Mannheim",  "2015-08-01", "Associate Degree Marketing at BI Norwegian Business School (online) \n German language at University of Mannheim",
    "Oslo",      "2016-08-01", "MSc in Finance at BI Norwegian Business School",
    "Nice",      "2017-01-01", "QTEM Exchange at EDHEC",
    "Lausanne",  "2017-08-01", "QTEM Excgabge at HEC Lausanne",
    "Oslo",      "2018-01-01", "MSc in Finance at BI Norwegian Business School",
    "Bergen",    "2018-08-01", "Working as a data science consultant at PwC"
  ) %>% 
  mutate(
    from = ymd(from),
    to = lead(from, default = today())
  ) %>% 
  left_join(cities) %>% 
  as.data.frame() %>% # needed to store label correctly
  mutate(label = c(rep(uni, 6), work)) %>% 
  mutate(
    enter = difftime(to, from),
    exit = difftime(to, to)
  )


col <- "navyblue"
nudge_y <- 0.15

p <- 
  ggplot() +
  geom_sf(data = world, color = 'white', lwd = 0.1) +
  geom_text(
    data = been,
    aes(label = label, x = lon, y = lat, group = seq_along(name)),
    size = 5,
    nudge_y = nudge_y,
    vjust = 0,
    color = col,
    family = "fontawesome-webfont"
  ) +
  geom_label(
    data = been,
    aes(label = name, x = lon, y = lat, group = seq_along(name)), 
    nudge_y = -nudge_y,
    vjust = 1,
    size = 4,
    label.padding = unit(0.15, "lines"),
    color = col,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = been,
    aes(label = what, x = lon, y = lat-2, group = seq_along(name)), 
    nudge_y = -nudge_y,
    vjust = 1,
    size = 4,
    label.padding = unit(0.15, "lines"),
    color = "black",
    inherit.aes = FALSE
  ) +
  coord_sf(crs = "+proj=longlat", xlim = c(-20, 35), ylim = c(40, 65)) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = 'transparent'),
    plot.margin = margin(-1, 0, -1, 0, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = 'white')
  )


a <- 
  p + 
labs(title = "Date: {frame_time}") + 
  transition_components(
    from,
    enter_length = been$from,
    exit_length = as_date(interval(been$from, been$to)/days(1)),
    range = c(ymd("2012-08-01"), today())
  )

# For testing:
# animate(a, nframes = 100, duration = 25)

gganimate::anim_save(
  filename = "cities_lived.gif", 
  animation = a, 
  nframes = 100, 
  duration = 20
)



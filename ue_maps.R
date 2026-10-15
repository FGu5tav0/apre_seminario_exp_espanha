library(giscoR)

# Set the same resolution for a perfect fit

res <- "20"

all_countries <- gisco_get_countries(resolution = res) %>%
  st_transform(3035)

eu_countries <- gisco_get_countries(resolution = res, region = "EU") %>%
  st_transform(3035)

ccaa <- esp_get_ccaa(moveCAN = FALSE, resolution = res) %>%
  st_transform(3035)


ggplot(all_countries) +
  geom_sf(fill = "#DFDFDF", color = "#656565") +
  geom_sf(data = eu_countries, fill = "#FDFBEA", color = "#656565") +
  geom_sf(data = provs, fill = "#b7603a", color = "grey80", linewidth = .1) +
  # Center in Europe: EPSG 3035
  coord_sf(xlim = c(2377294, 7453440),
           ylim = c(1313597, 5628510)) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "#DFDFDF", linetype = "dotted")
  )

ggsave(width = 6, height = 4, dpi = 600,
       filename = "EU.jpeg")

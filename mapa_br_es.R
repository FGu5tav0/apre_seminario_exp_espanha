# Carregar pacotes
library(ggplot2)
library(maps)
library(geosphere)
library(sf)
library(dplyr) # Para manipulação de dados

# Obter dados do mapa mundial como polígonos
world_map <- map_data("world")

# Converter os dados do mapa para o formato sf (Simple Features)
world_sf <- st_as_sf(world_map, coords = c("long", "lat"), crs = 4326) %>%
  group_by(group, region) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Filtrar os dados para destacar Brasil e Espanha
brasil_sf <- world_sf %>% filter(region == "Brazil")
espanha_sf <- world_sf %>% filter(region == "Spain")

# Definir as coordenadas aproximadas do Brasil e da Espanha
brasil <- c(-47.91, -15.78) # Coordenadas de Brasília
espanha <- c(-3.70, 40.42) # Coordenadas de Madrid

# Criar um data frame com as coordenadas dos pontos
pontos <- data.frame(
  lon = c(brasil[1], espanha[1]),
  lat = c(brasil[2], espanha[2]),
  label = c("Brasil", "Espanha")
)

# Calcular a rota de conexão (grande círculo) entre Brasil e Espanha
conexao <- gcIntermediate(brasil, espanha, n = 100, addStartEnd = TRUE, breakAtDateLine = FALSE)
conexao <- as.data.frame(conexao) # Converter para data frame

# Converter a linha de conexão para o formato sf
conexao_sf <- st_as_sf(conexao, coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

# Converter os pontos para o formato sf
pontos_sf <- st_as_sf(pontos, coords = c("lon", "lat"), crs = 4326)

# Definir a longitude central para girar o globo (ajuste este valor)
proj_ortografica <- "+proj=ortho +lon_0=-20 +lat_0=10"

# Transformar os dados para a projeção ortográfica
world_sf_proj <- st_transform(world_sf, crs = proj_ortografica)
brasil_sf_proj <- st_transform(brasil_sf, crs = proj_ortografica)
espanha_sf_proj <- st_transform(espanha_sf, crs = proj_ortografica)
conexao_sf_proj <- st_transform(conexao_sf, crs = proj_ortografica)
pontos_sf_proj <- st_transform(pontos_sf, crs = proj_ortografica)

# Criar o mapa com ggplot2
ggplot() +
  # Adicionar o mapa mundial como polígonos
  geom_sf(data = world_sf_proj, fill = "#fcca8b", color = "gray10", linewidth = 0.2) +
  # Destacar o Brasil com uma cor
  geom_sf(data = brasil_sf_proj, fill = "#b7603a", color = "gray50", linewidth = 0.5) +
  # Destacar a Espanha com outra cor
  geom_sf(data = espanha_sf_proj, fill = "#b7603a", color = "gray50", linewidth = 0.5) +
  # Adicionar a linha de conexão
  geom_sf(data = conexao_sf_proj, color = "green", linewidth = 1,
          linetype = 1) +
  # Adicionar os pontos
  geom_sf(data = pontos_sf_proj, aes(color = label), size = 3) +
  # Adicionar rótulos aos pontos
  geom_sf_label(
    data = pontos_sf_proj, aes(label = label),
    hjust = 1.2, vjust = 1.2, size = 4
  ) +
  # Definir cores dos pontos
  scale_color_manual(values = c(
    "Brasil" = "blue",
    "Espanha" = "blue"
  )) +
  labs(
    # title = "Brasil - Espanha",
    x = NULL,
    y = NULL,
    color = "País"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) -> p1


# spain -------------------------------------------------------------------
use("patchwork")
use("mapSpain")

provs <- esp_get_prov_siane(c(
  "Andalucía", "Ciudad Real", "Murcia", "Ceuta",
  "Melilla"
))
provs <- esp_get_prov_siane()

can_box <- esp_get_can_box()

provs <- provs |>
  mutate(cor = if_else(ine.prov.name %in% c("Córdoba","Madrid"),
                       "darkolivegreen3",
                       "gray"),
         nome = case_when(
           ine.prov.name == "Madrid" ~ "Madrid",
           ine.prov.name == "Córdoba" ~ "Córdoba",
           .default = NULL
         ))

anda <- provs |> filter(nuts2.name == "Andalucía") |>
  filter(cldr.prov.name.en != "Córdoba")

p2 <- ggplot(provs) +
  geom_sf(aes(fill = cor), alpha = 0.9) +
  geom_sf(data = can_box) +
  geom_sf(data = anda, fill = "gray99") +
  geom_sf_label(mapping = aes(label = nome),
                hjust = 1.2, vjust = 1.2, size = 4
  ) +
  scale_fill_identity() +
  theme_minimal() +
  labs(fill = NULL) +
  theme(legend.position = "none",
        axis.title = element_blank())

p2
# p1 + p2

ggsave(p2,width = 6, height = 4, dpi = 600,
       filename = "es.jpeg")


# Mapa de localiza??o --------------------------------------------------------------------

library(geobr)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(sf)
library(ggrepel)
library(ggspatial)

# ----------------------------------------------------------------------------------------

proj.orto <- "+proj=ortho +lon_0=-70"

terra <- read_sf("F:/R_studio/dados/leituraDadosmercel/mapa_localização/ne_50m_land.shp")

# terra <- read_sf("dados/leituraDadosmercel/mapa_localiza??o/ne_50m_land.shp")


# reprojetar  ----------------------------------------------------------------------------

#terra
terra.orth <- st_transform(terra, crs = proj.orto)

#america do sul
as <- ne_countries(continent = "South America", returnclass = "sf")

#baixar a fran?a para guiana
fr <- ne_countries(country = "France", returnclass = "sf")

#estado do brasil
estados <- read_state(code_state = "all")

#santa catarina
sc <- read_state(code_state = "RS")

#plot(terra.orth$geometry)

# dados do BR ----------------------------------------------------------------------------

#Com o pacote rnaturalearth
# br2 <- ne_countries(country = "Brazil", returnclass = "sf")

#Com o pacote geobr
br <- read_country() %>% 
      st_transform(crs = proj.orto) %>% 
      st_bbox() %>%  #latitude e longitude max?ma do territ?rrio 
      st_as_sfc() %>%  #mudando a classe para polygon
      st_sf() #mudando a classe para sf com data.frame

br.contorno <- read_country()


# map 1 -----------------------------------------------------------------------------------

m1 <- ggplot()+
        geom_sf(data = terra.orth, fill = "#FCCA8B", lwd = .2)+
        geom_sf(data = br, col = "#B7603A", fill = "transparent", lwd = 1)+
        scale_x_continuous(breaks = seq(-180, 180, 10))+
        scale_y_continuous(breaks = seq(-90,95,10))+
        theme_minimal()+
        theme(plot.background = element_rect(fill = alpha("white", .9),
                                             colour = alpha("black", 0.5)))


# map 2 ----------------------------------------------------------------------------------
lims.br <- br.contorno %>% st_bbox()
st_centroid(br.contorno) #centroide do brasil
st_centroid(sc) #centroide de santa catarina

m2 <- ggplot()+
        geom_sf(data = as,fill = "#FCCA8B", lwd = .2)+
        geom_sf(data = fr, fill = "#FCCA8B", lwd = .2)+
        geom_sf(data = estados, fill = "#e0b47b", lwd = .1, col =alpha("gray20", .3))+
        geom_sf(data = br.contorno, fill = "transparent", col =alpha("gray20", .6))+
        geom_sf(data = sc, fill ="#ef8118")+
        xlim(lims.br[1], lims.br[3])+
        ylim(lims.br[2], lims.br[4])+
        geom_label_repel(aes(-53.1873, -10.58913, label = "Brasil"), 
                         fill = alpha("white", .6), label.size = NA,
                         nudge_x = 0.1, nudge_y = 0.1)+
        geom_label_repel(aes(-53.31893, -29.69869, label = "Rio Grande Do Sul"),
                         fill = NA, label.size = NA, size = 3, 
                         nudge_x = 10, nudge_y = 0)+
        labs(x = NULL, y = NULL)+
        annotation_scale(location = "br")+
        annotation_north_arrow(location = "tr",
                               style = north_arrow_fancy_orienteering())+
        theme_minimal()


# sobrepor  ------------------------------------------------------------------------------

library(patchwork)

plot.1 <- m2 + inset_element(p = m1, bottom = -0.01, top = 0.4, left = 0, right = 0.4)


ggsave(plot = plot.1, filename = "map_location.png", 
       width = unit(10, "cm"), 
       height = unit(9, "cm"),
       scale = .5,
       dpi = 400)






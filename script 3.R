## Paquetes

library(tidyverse)
library(sf)
library(plotly)
library(readxl)
library(viridis)
library(scales)
library(dplyr)
library(readr)
library(geoAr)
library(stringi)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)

emisiones_prov <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx", skip = 3)

##2. Emisiones (2010-2022)
###2.a Por provincia
colnames(emisiones_prov)[1] <- "Provincia"
colnames(emisiones_prov)[-1] <- as.character(colnames(emisiones_prov)[-1])

emisiones_por_prov <- emisiones_prov[-c(1, 2, 4, 30, 31, 32, 33, 34, 35), ]

emisiones_por_prov_long <- emisiones_por_prov %>%
  pivot_longer(
    cols = -Provincia,
    names_to = "Año",
    values_to = "Emisiones"
  ) %>%
  mutate(Año = as.integer(as.numeric(Año)))


emisiones_por_prov <- emisiones_por_prov_long %>%
  filter(!Provincia %in% c("Total Pais", "Sin asignar"))

ggplot(emisiones_por_prov, aes(x = Año, y = Emisiones, color = Provincia)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 100)) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Evolución de emisiones de CO₂e por provincia (2010–2022)",
       x = "Año", y = "Emisiones (Mt CO₂e)", color = "Provincia")


ggplot(emisiones_por_prov, aes(x = Año, y = Emisiones)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Provincia, scales = "free_y") +
  theme_minimal() +
  labs(title = "Evolución de emisiones por provincia (2010–2022)",
       x = "Año", y = "Emisiones (Mt CO₂e)")

###2.b Mapa coroplético
colnames(emisiones_prov)[1] <- "Provincia"
colnames(emisiones_prov)[-1] <- as.character(colnames(emisiones_prov)[-1])

emisiones_2022 <- emisiones_prov %>%
  select(Provincia, `2022.0`) %>%
  rename(Emisiones = `2022.0`)

emisiones_prov_2022 <- emisiones_2022[-c(1, 2, 4, 30, 31, 32, 33, 34, 35), ]

emisiones_prov_2022 <- emisiones_prov_2022 %>%
  mutate(Provincia_key = norm(Provincia))

emisiones_prov_2022 <- emisiones_prov_2022 %>%
  mutate(
    Provincia = recode(
      Provincia,
      "TIERRA DEL FUEGO ANTARTIDA E ISLAS DEL ATLANTICO SUR" = "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR"
    )
  )

#arg <- ne_states(country = "Argentina", returnclass = "sf") %>%
#  mutate(Provincia = coalesce(name_es, name, gn_name),
#         Provincia_key = Provincia)

arg <- read_sf("data/mapas/provincia/provinciaPolygon.shp", options = "ENCODING=LATIN1")

#norm <- function(x) stri_trans_general(x, "Latin-ASCII") |> toupper() |> trimws()
#arg <- arg %>%
#  mutate(Provincia_key = norm(coalesce(fna)))

mapa <- arg %>%
  left_join(emisiones_prov_2022 %>% select(Provincia_key, Emisiones), by = "Provincia_key")

summary(mapa$Emisiones)

ggplot(mapa) +
  geom_sf(aes(fill = Emisiones), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "CO₂e 2022 (Mt)") +
  theme_minimal() +
  labs(title = "Argentina · Emisiones de CO₂e por provincia (2022)",
       caption = "Fuente: Inventario Nacional de GEI
")

###2.c Serie de tiempo y densidad provincias
####2.c.1 Neuquén
neu <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                  sheet = "58_Neuquen", skip = 2) |>
  mutate(Provincia = "Neuquén", .before = 1)

neu <- neu [-c(1, 2, 4, 9, 11, 17, 19, 26, 28, 39, 41, 46, 47, 48, 49, 50, 51), ]
neu <- neu [-c(3), ]
neu <- neu |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

neu_limpio <- neu |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

neu_sector <- neu_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

neu_prop <- neu_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(neu_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Neuquén · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )

####2.c.2 Córdoba
cba <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                  sheet = "14_Cordoba", skip = 2) |>
  mutate(Provincia = "Córdoba", .before = 1)

cba <- cba [-c(1, 2, 4, 9, 11, 17, 19, 26, 28, 39, 41, 46, 47, 48, 49, 50, 51), ]
cba <- cba [-c(3), ]
cba <- cba |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

cba_limpio <- cba |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

cba_sector <- cba_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

cba_prop <- cba_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(cba_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Córdoba · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )

####2.c.3 Chaco
cha <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                  sheet = "22_Chaco", skip = 2) |>
  mutate(Provincia = "Chaco", .before = 1)

cha <- cha [-c(1, 2, 4, 9, 11, 17, 19, 26, 28, 39, 41, 46, 47, 48, 49, 50, 51), ]
cha <- cha [-c(3), ]
cha <- cha |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

cha_limpio <- cha |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

cha_sector <- cha_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

cha_prop <- cha_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(cha_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Chaco · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )

####2.c.4 Santiago del Estero
sde <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                  sheet = "86_Santiago del Estero", skip = 2) |>
  mutate(Provincia = "Santiago del Estero", .before = 1)

sde <- sde [-c(1, 2, 4, 9, 11, 17, 19, 26, 28, 39, 41, 46, 47, 48, 49, 50, 51), ]
sde <- sde [-c(3), ]
sde <- sde |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

sde_limpio <- sde |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

sde_sector <- sde_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

sde_prop <- sde_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(sde_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Santiago del Estero · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )

####2.c.5 Santa Fe
sf <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                  sheet = "82_Santa Fe", skip = 2) |>
  mutate(Provincia = "Santa Fe", .before = 1)

sf <- sf [-c(1, 2, 4, 9, 11, 17, 19, 26, 28, 39, 41, 46, 47, 48, 49, 50, 51), ]
sf <- sf [-c(3), ]
sf <- sf |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

sf_limpio <- sf |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

sf_sector <- sf_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

sf_prop <- sf_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(sf_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Santa Fe · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )

####2.c.7 Ciudad Autónoma de Buenos Aires
caba <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx",
                 sheet = "02_CABA", skip = 2) |>
  mutate(Provincia = "CABA", .before = 1)

caba <- caba [-c(1, 2, 4, 6, 9, 11, 17, 19, 26, 28, 39, 40, 42, 47, 48, 49, 50, 51, 52), ]
caba <- caba |>
  rename(Sector = `Sector / Categoría`) |>
  rename_with(as.character) |>
  rename_with(~ sub("\\.0$", "", .x))

caba_limpio <- caba |> 
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Año",
               values_to = "Emisiones",
               values_transform = list(Emisiones = as.numeric)) |>
  mutate(Año = as.numeric(Año))

caba_sector <- caba_limpio |>
  filter(grepl("^\\d+\\.?\\s*Sector", Sector))

caba_prop <- caba_sector %>%
  mutate(
    Emisiones = as.numeric(Emisiones),
    Emisiones = replace_na(Emisiones, 0),
    Emis_pos = pmax(Emisiones, 0)
  ) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Emis_pos / sum(Emis_pos)) %>%
  ungroup()

ggplot(caba_prop, aes(x = Año, y = Porcentaje, fill = Sector)) +
  geom_area(color = "white", size = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2022, labels = 2010:2022, expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "CABA · Composición porcentual de emisiones de CO₂e por sector (2010–2022)",
    x = "Año", y = "Participación (%)", fill = "Sector"
  )
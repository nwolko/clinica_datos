## Paquetes

library(tidyverse)
library(sf)
library(plotly)
library(readxl)
library(ggplot2)
library(sf)
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

emisiones_por_prov <- emisiones_por_prov %>%
  mutate(across(-Jurisdicción, as.numeric))

emisiones_por_prov <- emisiones_por_prov %>%
  pivot_longer(
    cols = -Jurisdicción,
    names_to = "Año",
    values_to = "Emisiones"
  ) %>%
  mutate(Año = as.numeric(Año))

emisiones_por_prov <- emisiones_por_prov_long %>%
  filter(!Jurisdicción %in% c("Total Pais", "Sin asignar"))

ggplot(emisiones_por_prov, aes(x = Año, y = Emisiones, color = Jurisdicción)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 100)) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Evolución de emisiones de CO₂e por provincia (2010–2022)",
       x = "Año", y = "Emisiones (Mt CO₂e)", color = "Provincia")


ggplot(emisiones_por_prov, aes(x = Año, y = Emisiones)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Jurisdicción, scales = "free_y") +
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

arg <- ne_states(country = "Argentina", returnclass = "sf") %>%
  mutate(Provincia = coalesce(name_es, name, gn_name),
         Provincia_key = Provincia)

norm <- function(x) stri_trans_general(x, "Latin-ASCII") |> toupper() |> trimws()

arg <- arg %>%
  mutate(Provincia_key = norm(coalesce(name_es, name, gn_name)))

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


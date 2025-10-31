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
install.packages("purr")
library(purrr)
library(tidyr)

## Algunos datasets no tan trabajados
evolucion_co2 <- read.csv("data/Argendata//CAMCLI/01_evolucion_CO2_historico.csv")
diferencia_paises <- read.csv("data/Argendata/CAMCLI/diferencia_temperatura_paises.csv")
emisiones_mundo <- read_csv("data/Argendata/CAMCLI/emisiones_arg_mundo.csv")
emisiones_global <- read_csv("data/Argendata/CAMCLI/emisiones_global_sec_1850_2014.csv")
emisiones_global2 <- read_csv("data/Argendata/CAMCLI/emisiones_sector_global_2016.csv")
intensidad <- read_csv("data/Argendata/TRANEN/intensidad_carbono_electri_mundo.csv")
energ <- read_csv("data/Argendata/TRANEN/intensidad_energ_mundo.csv")
emisiones_prov <- read_excel("data/desagregacion-provincial_hasta_2022.xlsx", skip = 3)


## Empezamos a trabajar con esta
prueba <- read.csv("data/datos_gob_ar/emisiones_datos_totales_1990_2022.csv", sep = ";")

## Primeras pruebas para pasar datos a tablas
prueba %>% group_by(año, tipo_de_gas) %>%  summarise(tipo_de_gas,
                                        Emisiones = sum(valor_en_toneladas_de_co2e))

## Agrupaciones por emisión y tipo de gas
tabla_tipo_gas <- prueba %>% 
  group_by(año, tipo_de_gas) %>%  
  summarise(Emisiones = sum(valor_en_toneladas_de_co2e))

tabla_tipo_gas_ok <- prueba %>% 
  group_by(año, actividad) %>% 
  summarise(Emisiones = sum(valor_en_toneladas_de_co2e))


## Primeros gráficos para trabajar los datos
plot_evolucion_gases <- 
  ggplot(data = prueba, aes(x= año, y = Emisiones, color = tipo_de_gas)) +
  geom_line() +
  labs(title = "Evolución de Emisiones por Tipo de Gas",
                    x = "Año",
                    y = "Emisiones (toneladas de CO2E)",
                    color = "Tipo de Gas") +
  theme_minimal()
  


<<<<<<< HEAD
ggplot(data = prueba, aes(x=año, y = Emisiones, color = actividad)) +
  geom_density()


=======
>>>>>>> c78704ac2061c997a05e61202747ae47cf457f87

ggplot(data = prueba, aes(x = Emisiones, color = actividad)) +
  geom_density() +
  theme_minimal()

## Datos
prueba3 <- prueba %>%
  group_by(año) %>%
  mutate(Proporcion = Emisiones / sum(Emisiones) * 100) %>%
  ungroup()

## Este es el gráfico de área sin el plotly
plot2 <- ggplot(prueba3, aes(x = año, y = Proporcion, fill = actividad))+
                             #text = paste0("Año: ", año,
                              #             "\nActividad: ", actividad,
                               #            "\nEmisiones: ", round(Proporcion, 0)))) +
  geom_area() +
  labs(title = "Distribución de Emisiones por Actividad",
       subtitle = "En Argentina",
       y = "Porcentaje (%)",
       x = "Año",
       fill = "Distribución por Actividad") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    legend.position = "right"
  )

ggplotly(plot2)


## Este es el mismo pero en formato barra
grafico <- ggplot(prueba3, aes(x = año, y = Emisiones, fill = actividad,
                               text = paste0("Año: ", año, 
                                             "\nSubactividad: ", actividad,
                                             "\nEmisiones: ", round(Emisiones, 0)))) +
  geom_col(position = "fill") +
  theme_minimal()


## Este es el que intenté para plotly pero tengo problemas con el text
grafico <- ggplot(prueba3, aes(x = año, y = Proporcion, fill = actividad,
                               text = paste0("Año: ", año,
                                             "\nSubactividad: ", actividad,
                                             "\nPorcentaje: ", round(Proporcion, 1), "%"))) + 
  geom_area() + 
  labs(title = "Distribución de Emisiones por Subactividad", 
       y = "Porcentaje (%)", 
       x = "Año", 
       fill = "Subactividad") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"
  )


ggplotly(grafico, tooltip = "text")


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

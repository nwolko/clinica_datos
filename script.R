## Paquetes

library(tidyverse)
library(sf)
install.packages("plotly")
library(plotly)

## Algunos datasets no tan trabajados
evolucion_co2 <- read.csv("data/Argendata//CAMCLI/01_evolucion_CO2_historico.csv")
nivel_mar <- read.csv("data/CAMCLI/06_evolucion_nivel_del_mar_1993_2022.csv")
diferencia_paises <- read.csv("data/CAMCLI/diferencia_temperatura_paises.csv")
emisiones_mundo <- read_csv("data/Argendata/CAMCLI/emisiones_arg_mundo.csv")
emisiones_global <- read_csv("data/Argendata/CAMCLI/emisiones_global_sec_1850_2014.csv")
emisiones_global2 <- read_csv("data/Argendata/CAMCLI/emisiones_sector_global_2016.csv")
emisiones_mundo <- read_csv("data/Argendata/CAMCLI/emisiones")
intensidad <- read_csv("data/Argendata/TRANEN/intensidad_carbono_electri_mundo.csv")
energ <- read_csv("data/Argendata/TRANEN/intensidad_energ_mundo.csv")


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
  ggplot(data = prueba1, aes(x= año, y = Emisiones, color = tipo_de_gas)) +
  geom_line() +
  labs(title = "Evolución de Emisiones por Tipo de Gas",
                    x = "Año",
                    y = "Emisiones (toneladas de CO2E)",
                    color = "Tipo de Gas") +
  theme_minimal()
  



ggplot(data = prueba2, aes(x = Emisiones, color = actividad)) +
  geom_density() +
  theme_minimal()

## Datos
prueba3 <- prueba2 %>%
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



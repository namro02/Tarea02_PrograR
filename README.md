---
title: "Tarea02_ProgramacionR_B34608"
author: "Natalia Mora"
format: html
editor: visual
editor_options: 
  chunk_output_type: console
---

## Descripción

Este documento incluye el código, ejecución y documentación de la Tarea 02 que ha sido dividida en partes según las instrucciones con algunas adaptaciones propias.

## Preparación de la data

Importación de datos en archivos csv como dataframes en R sobre el COVID en Costa Rica para el día 30/05/2022:

```{r}
getwd()
setwd("~/Programacion R/Tarea02")
General <- read.csv("Covid_General.csv")
General_bu <- General
Positivos <- read.csv("Covid_Positivos.csv")
```

Checkar tipos de datos de cada columna de los dataframes:

```{r}
str(General)
```

```{r}
str(Positivos)
```

Nuevo dataframe General que contiene únicamente las columnas necesarias para lo que requiere la tarea y conversión del tipo de dato de la columna Fecha de caracter a datetime:

```{r}

# Seleccionar las columnas necesarias y renombrandolas y cambiado el tipo de dato de la columna Fecha a date. 

library("dplyr")
library("lubridate")

General <- General %>% transmute(fecha = dmy(FECHA), hombres = hom_posi, mujeres= muj_posi, menores = menor_posi, adultos = adul_posi, adultos_mayores = am_posi, nuevos = nue_posi)

```

```{r}

str(General)

```

Eliminar filas con NA values.

```{r}

General <- na.omit(General)

```

Calcular acumulados de casos covid según hombres, mujeres, menores de edad, adultos, adultos mayores y total de casos positivos.

```{r}
Acumulados = data.frame(sum(General$hombres),sum(General$mujeres),sum(General$menores),sum(General$adultos),sum(General$adultos_mayores),sum(General$nuevos))
```

```{r}
General$hombres = cumsum(General$hombres)
General$mujeres = cumsum(General$mujeres)
General$menores = cumsum(General$menores)
General$adultos = cumsum(General$adultos)
General$adultos_mayores = cumsum(General$adultos_mayores)
General$nuevos = cumsum(General$nuevos)

```

## Tabla de acumulados casos COVID - 30/05/20220

| Clasificador  | Hombres   | Mujeres   | Menores  | Adultos   | Adultos Mayores | Nuevos |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
| **Acumulado** | 133785875 | 134986310 | 28023984 | 223480321 | 17176751        | 904922 |

## Gráfico de barras

Gráfico de barras de casos positivos desde 03/06/2020 hasta 30/05/2022.

Filtrado de los datos en un subset de casos positivos por fecha con su respectiva provincia.

```{r}

library("dplyr")

subset_positivos <- Positivos |> select(X3.6.2020:X30.5.2022)
```

Obtener la suma de la cantidad de casos por día:

```{r}

positivos_acum_diario  = apply(X= subset_positivos, MARGIN=2, FUN= sum) 

positivos_acum_diario = as.data.frame(positivos_acum_diario)


```

```{r}

#add index column to data frame

positivos_acum_diario <- cbind(Fecha = rownames(positivos_acum_diario), positivos_acum_diario)
rownames(positivos_acum_diario) <- 1:nrow(positivos_acum_diario)

```

Limpiar la columna de fechas utilizando regex para llegar a tener la columna Feccha en tipo de dato de fecha para luego graficar.

```{r}

library("dplyr")

positivos_acum_diario[] <- lapply(positivos_acum_diario, sub, pattern = '^X',  replacement ="")

```

```{r}
library(dplyr)

str(positivos_acum_diario$Fecha)

```

```{r}

positivos_acum <- positivos_acum_diario |> mutate(Fecha=gsub('\\.',"-",Fecha))

str(positivos_acum$Fecha)
```

```{r}
library(lubridate)
positivos_acum[["Fecha"]] <- dmy(positivos_acum$Fecha)
positivos_acum[["positivos_acum_diario"]] <- as.integer(positivos_acum$positivos_acum_diario)

str(positivos_acum)
```

**Gráfico de barras**

```{r}
# Gráfico de barras con población de países 
library(ggplot2)
library(ggthemes)
library(plotly)

grafico_barras_ggplot2 <-
 ggplot(positivos_acum, aes(x=Fecha, y=positivos_acum_diario)) + 
  geom_bar(stat="identity") +
  ggtitle("Casos acumulados por día desde el 2020-06-03 hasta el 2022-05-30") +
  xlab("Día") +
  ylab("Cantidad") +
  labs(caption = "Fuente: oges.ministeriodesalud.org") +
  theme_economist()

# Gráfico de barras plotly

ggplotly(grafico_barras_ggplot2) |>
config(locale = 'es')
```

## Gráfico de líneas de los casos positivos acumulados de hombres y de los casos positivos acumulados de mujeres.

```{r}

# Gráfico de líneas con la evolución de los casos de COVID

grafico_lineas_ggplot2 <-
  ggplot(General, aes(x = fecha, y = value, color = variable)) +
  geom_line(aes(y = hombres, color = "Hombres")) +
  geom_line(aes(y = mujeres, color = "Mujeres")) +
  scale_color_manual( # colores
    "",
    values = c(
      "Hombres" = "blue",
      "Mujeres" = "green"
    )
  ) +
  ggtitle("Casos de COVID en Costa Rica de hombres y mujeres del 2022-05-30") +
  xlab("Fecha") +
  ylab("Casos") +
  theme_economist()  

# Gráfico de dispersión plotly
ggplotly(grafico_lineas_ggplot2) |>
  config(locale = 'es')


```

### Gráfico de líneas que muestre la evolución a través del tiempo de los casos positivos acumulados de menores, los casos positivos acumulados de adultos y de los casos positivos acumulados de adultos mayores.

```{r}

# Gráfico de líneas con la evolución de los casos de COVID
grafico_lineas_ggplot2 <-
  General |>
  ggplot(aes(x = fecha, y = value, color = variable)) +
  geom_line(aes(y = menores, color = "Menores")) +
  geom_line(aes(y = adultos, color = "Adultos")) +
  geom_line(aes(y = adultos_mayores, color = "Adultos Mayores")) +
  scale_color_manual( # colores
    "",
    values = c(
      "Menores" = "blue",
      "Adultos" = "green",
      "Adultos Mayores" = "red"
      
    )
  ) +
  ggtitle("Casos de COVID acumulados en el tiempo en Costa Rica de menores, adultos y adultos mayores del 2022-05-30") +
  xlab("Fecha") +
  ylab("Casos") +
  theme_economist()  

# Gráfico de dispersión plotly
ggplotly(grafico_lineas_ggplot2) |>
  config(locale = 'es')


```

## Tabla que muestre la cantidad casos positivos en cantones

```{r}

positivos_prov_canton <- Positivos %>% transmute(Provincia = provincia, Canton = canton, Mayo30_2022 = X30.5.2022)


```

```{r}

# Eliminando la fila "Otros" 

positivos_prov_can <- head(positivos_prov_canton, - 1)                               
```

```{r}

# Creando una tabla para 

library(DT)

datatable(positivos_acum_diario)

```

## Histograma que muestre la distribución de los casos positivos en cantones

```{r}
#Histograma de casos de covid por cantones

histograma_ggplot2 <-

  positivos_prov_canton |>

  ggplot(aes(x=Mayo30_2022)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Casos de COVID por caton 2022-05-30") +
  xlab("Cantones") +
  ylab("Density") +
  labs(caption = "Fuente: Gapminder.org") +
  theme_economist()

 ggplot(positivos_prov_canton, aes(x=Mayo30_2022)) +
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="#FF6666")

# Histograma plotly
  ggplotly(histograma_ggplot2) |>
  config(locale = 'es')

```



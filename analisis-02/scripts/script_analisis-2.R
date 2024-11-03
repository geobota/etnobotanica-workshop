# Script de Análisis 1 ----


## Configuración ----


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidymodels ")) install.packages("tidymodels")
if (!require("here")) install.packages("here")
if (!require("rstatix")) install.packages("rstatix")
if (!require("infer")) install.packages("infer")

here::here()


## Normalidad ----


### Importación y preparación de los datos ----


#| label: datos-prueba-normalidad
datos_prueba_normalidad <-
  readr::read_delim(
    here::here(
      "analisis-2",
      "datos",
      "datos_prueba-normalidad-varianza.csv"
    ),
    delim = ";",
    col_names = TRUE
  )


#| label: glimpse-datos-prueba-normalidad
datos_prueba_normalidad |>
  dplyr::glimpse()


### Prueba de normalidad ----


#| label: prueba-normalidad-shapiro
prueba_normalidad_shapiro <-
  datos_prueba_normalidad |>
  rstatix::shapiro_test(
    Preparacion,
    Posologia
  )


## Prueba homogeneidad de varianza ----


#| label: prueba-homogeneidad-levene
prueba_homogeneidad_levene <-
  datos_prueba_normalidad |>
  dplyr::mutate(
    Preparacion = forcats::as_factor(Preparacion)
  ) |>
  rstatix::levene_test(
    Posologia ~ Preparacion,
    center = median
  )

## Test de Bartlett ----


#| label: prueba-barlett-preparacion

prueba_barlett_preparacion <-
  stats::bartlett.test(
    Preparacion ~ Enfermedad,
    data = datos_prueba_normalidad
  ) |>
  broom::tidy()


#| label: prueba-barlett-posologia

prueba_barlett_posologia <-
  stats::bartlett.test(
    Posologia ~ Enfermedad,
    data = datos_prueba_normalidad
  ) |>
  broom::tidy()


## Chi cuadrado ----


### Importación y preparación de los datos ----


#| label: datos-prueba-chi-cuadrado
datos_prueba_chi_cuadrado <-
  readr::read_delim(
    here::here(
      "analisis-2",
      "datos",
      "datos_chi-cuadrado.csv"
    ),
    delim = ";",
    col_names = TRUE
  )


#| label: glimpse-datos-prueba-chi_cuadrado
datos_prueba_chi_cuadrado |>
  dplyr::glimpse()


# para cambiar los caracters a factores hay varias formas. También se podría hacer cuando se importan los datos con `readr::read_delim`.
# datos_prueba_chi_cuadrado <-
#   datos_prueba_chi_cuadrado |>
#   dplyr::mutate(
#     dplyr::across(
#       dplyr::where(base::is.character),
#       forcats::as_factor
#     )
#   )
#
# datos_prueba_chi_cuadrado <-
#   datos_prueba_chi_cuadrado |>
#   dplyr::mutate(
#     Enfermedad = forcats::as_factor(Enfermedad),
#     Preparacion = forcats::as_factor(Preparacion)
#   )


### Prueba Chi cuadrado ----


#| label: prueba-chi-cuadrado

prueba_chi_cuadrado <-
  datos_prueba_chi_cuadrado |>
  infer::chisq_test(
    Enfermedad ~ Preparacion
  )


## t-Student ----


### Importación y preparación de los datos ----


#| label: datos-prueba-t-student

datos_prueba_t_student <-
  readr::read_delim(
    here::here(
      "analisis-2",
      "datos",
      "datos_t-test.csv"
    ),
    delim = ";",
    col_names = TRUE
  )


#| label: glimpse-datos-prueba-t-student

datos_prueba_t_student |>
  dplyr::glimpse()


### Prueba t-Student ----


# label: prueba-t-student

prueba_t_student <-
  datos_prueba_t_student |>
  infer::t_test(
    formula = Preparacion ~ Enfermedad,
    order = c("DDS", "SNC"),
    alternative = "two-sided",
    conf_level = 0.95
  )

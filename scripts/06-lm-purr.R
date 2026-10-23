# Librerias ---------------------------------------------------------------

library(tidyverse)
library(broom)
library(glue)
library(datos)

# usamos datos de pinguinos -----------------------------------------------

datos_pinguinos <- datos::pinguinos

# Revisamos los datos -----------------------------------------------------

datos_pinguinos %>% glimpse()

# Nos piden una regresion entre masa y largo de aleta ---------------------

lm_masa_aleta_obj <- lm(masa_corporal_g ~ largo_aleta_mm, # y ~ x
                    data = datos_pinguinos)
glm_masa_aleta_obj <- glm(masa_corporal_g ~ largo_aleta_mm, # y ~ x
                    data = datos_pinguinos)

broom::tidy(lm_masa_aleta_obj) # cambiar nombre objeto general
broom::glance(lm_masa_aleta_obj)

# Lo visualizamos ---------------------------------------------------------

datos_pinguinos %>%
  ggplot(aes(x = largo_aleta_mm, y = masa_corporal_g)) + # y ~ x
  geom_point() +
  geom_smooth(method = "lm")

# Interesante! Ahora por isla y sexo --------------------------------------

datos_pinguinos %>%
  ggplot(aes(x = largo_aleta_mm, y = masa_corporal_g, color = especie)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(isla,sexo))

# Vamos a usar la funcion nest() ------------------------------------------

datos_pinguinos_anidados <-
  datos_pinguinos %>%
  drop_na(sexo) %>%
  group_by(isla,sexo) %>%
  nest()

# Revisamos el objeto que creamos -----------------------------------------

datos_pinguinos_anidados

datos_pinguinos_anidados$data[1]

# Repasemos ---------------------------------------------------------------

# purrr::map(vector,funcion)

# Creamos una funcion -----------------------------------------------------

lm_masa_aleta <- function(datos){
  lm(masa_corporal_g ~ largo_aleta_mm, data = datos)
}

# map(datos_pinguinos_anidados, lm_masa_aleta) ### error explosivo!

map(datos_pinguinos_anidados$data, lm_masa_aleta)

# Podemos trabajar en un esquema tidyverse --------------------------------

datos_pinguinos_anidados %>%
  mutate(modelos_lm = map(data, lm_masa_aleta))

# Usamos la funciones de broom para extraer coef --------------------------

datos_pinguinos_anidados %>%
  mutate(modelos_lm = map(data, lm_masa_aleta),
         tidy_lm = map(modelos_lm, tidy),
         glance_lm = map(modelos_lm, glance))

# Usamos la funcion unnest ------------------------------------------------

datos_modelos <-
  datos_pinguinos_anidados %>%
  mutate(modelos_lm = map(data, lm_masa_aleta),
         tidy_lm = map(modelos_lm, tidy),
         glance_lm = map(modelos_lm, glance)) %>%
  unnest(glance_lm)

# Hacemos un arreglo ------------------------------------------------------

datos_stat_limpios <- datos_modelos %>%
  mutate(
    r_cuadrado = signif(r.squared, 2),
    # redondeo a dos sign.
    pval = signif(p.value, 2),
    #  redondeo a dos sign.
    pval = case_when(
      pval > 0.05 ~ "not sig",
      pval < 0.05 ~ "*",
      pval < 0.001 ~ "**",
      pval < 0.0001 ~ "***"
    )
  ) %>%
  mutate(etiqueta = glue("R² = {r_cuadrado}, p = {pval}")) %>%
  select(isla, sexo , etiqueta)

# Clase: pivot, join y regex con pingüinos -------------------------------
# Visualización y Análisis de Datos Medioambientales (AGP3141)
# Script basado en:
# https://carpentries-incubator.github.io/r-tidyverse-4-datasets/07-data-reshaping.html

# Librerías ---------------------------------------------------------------

# install.packages("datos")
library(tidyverse)
library(datos)

# Miremos pinguinos -------------------------------------------------------

pinguinos <- datos::pinguinos

glimpse(pinguinos)

# Las medidas tienen guion bajo en el nombre (largo_pico_mm, masa_corporal_g)
# y las columnas que identifican al pingüino no (especie, isla, sexo, anio)

# 1. ALARGAR: pivot_longer() ===============================================

pinguinos %>%
  pivot_longer(contains("_"))

# Cambiar los nombres al mismo tiempo -------------------------------------

pinguinos %>%
  pivot_longer(contains("_"),
               names_to = "variable",
               values_to = "valor")

# Más detalle: separar el nombre con names_sep ----------------------------
# largo_pico_mm -> largo | pico | mm
# masa_corporal_g -> masa | corporal | g

pinguinos %>%
  pivot_longer(contains("_"),
               names_to = c("medida", "parte", "unidad"),
               names_sep = "_")

# Otras opciones (ver ?pivot_longer) --------------------------------------
# solo las medidas del pico y sin NA

pinguinos %>%
  pivot_longer(contains("pico"),
               values_drop_na = TRUE)

# Terminemos con un objeto ------------------------------------------------

pinguino_largo <- pinguinos %>%
  pivot_longer(contains("_"),
               names_to = c("medida", "parte", "unidad"),
               names_sep = "_",
               values_drop_na = TRUE)

pinguino_largo

# 2. ENSANCHAR: pivot_wider() ==============================================

pinguino_largo_simple <- pinguinos %>%
  pivot_longer(contains("_"))

pinguino_largo_simple %>%
  pivot_wider(names_from = name,
              values_from = value)

# Advertencia -------------------------------------------------------------

# Warning message:
#   Values from `value` are not uniquely identified; output will contain list-cols.
# • Use `values_fn = list` to suppress this warning.
# • Use `values_fn = {summary_fun}` to summarise duplicates.

# ¿Qué pasó? --------------------------------------------------------------

# Los valores no están identificados de forma única: hay varios pingüinos de
# la misma especie, isla, sexo y año. pivot_wider() no sabe en qué fila poner
# cada valor y los junta en columnas de lista.

# Falta columna <<<ID>>>

# Creamos otro largo, ahora con ID ----------------------------------------

pinguino_largo_simple <- pinguinos %>%
  mutate(id = row_number()) %>%
  pivot_longer(contains("_"))

pinguino_largo_simple

# Ahora sí podemos ensanchar ----------------------------------------------

pinguino_largo_simple %>%
  pivot_wider(names_from = isla,
              values_from = value)

# 3. ¿PARA QUÉ SIRVE CADA FORMATO? =========================================

# Ancho -> estadística ----------------------------------------------------
# imaginemos que empezamos con los datos a lo largo y queremos un ANOVA

pinguinos_ancho <- pinguino_largo_simple %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  filter(!is.na(masa_corporal_g))

aov(masa_corporal_g ~ especie, data = pinguinos_ancho) %>%
  summary()

# Largo -> ggplot2 --------------------------------------------------------

pinguinos %>%
  pivot_longer(contains("_"),
               values_drop_na = TRUE) %>%
  ggplot(aes(x = especie,
             y = value,
             fill = especie)) +
  geom_boxplot() +
  facet_wrap(~ name, scales = "free_y")

# 4. UNIONES: *_join() =====================================================

# left_join(x, y)  : todas las filas de x, con info de y si existe
# right_join(x, y) : todas las filas de y
# inner_join(x, y) : solo las que están en ambas
# full_join(x, y)  : todas, de ambas (rellena con NA)
# anti_join(x, y)  : las de x que NO están en y (revisar errores)
# semi_join(x, y)  : las de x que sí están en y, sin agregar columnas

# Una segunda tabla: información de cada especie --------------------------

especies_info <- tibble(
  nombre_comun      = c("Adelia", "Barbijo", "Papúa", "Rey"),
  nombre_cientifico = c("Pygoscelis adeliae", "Pygoscelis antarcticus",
                        "Pygoscelis papua", "Aptenodytes patagonicus"),
  nombre_ingles     = c("Adélie", "Chinstrap", "Gentoo", "King")
)

especies_info

# La llave se llama distinto en cada tabla: especie vs nombre_comun
# y el pingüino Rey no está en los datos

# left_join con llaves de distinto nombre ---------------------------------

pinguinos %>%
  left_join(especies_info,
            by = c("especie" = "nombre_comun")) |> View()

# sintaxis nueva de dplyr (equivalente)

pinguinos %>%
  left_join(especies_info,
            by = join_by(especie == nombre_comun))

# anti_join: ¿quién quedó afuera? -----------------------------------------

especies_info %>%
  anti_join(pinguinos,
            by = c("nombre_comun" = "especie"))

# Devuelve al pingüino Rey. Antes de unir, anti_join() ayuda a encontrar
# llaves mal escritas (Papua vs Papúa, mayúsculas, espacios)

# 5. PIVOT + JOIN JUNTOS ===================================================

# Conteo: pingüinos de cada especie en cada isla --------------------------

pinguinos %>%
  count(isla, especie) %>%
  pivot_wider(names_from = especie,
              values_from = n,
              values_fill = 0)

# Promedio de cada medida por especie + nombre científico -----------------

resumen_especie <- pinguinos %>%
  pivot_longer(contains("_"), values_drop_na = TRUE) %>%
  group_by(especie, name) %>%
  summarise(promedio = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = name,
              values_from = promedio)

resumen_especie %>%
  left_join(especies_info,
            by = c("especie" = "nombre_comun"))

# 6. EXTRA: EXPRESIONES REGULARES (REGEX) ==================================

# ^      inicio del texto        "^largo" empieza con largo
# $      fin del texto           "_mm$"   termina en _mm
# .      cualquier carácter      "p.co"   pico, paco
# * / +  cero o más / una o más  ".*"     cualquier texto
# [a-z]  un carácter del rango   "[a-z]+$" la última palabra
# \\d    un dígito               "\\d+"   2007, 3750
# |      o                       "mm|g"
# ( )    grupo                   "(.*)_(.*)" dos grupos

# Regex para seleccionar y filtrar ----------------------------------------

# nombres de columnas que terminan en _mm
names(pinguinos) %>%
  str_subset("_mm$")

# seleccionar columnas con un patrón
pinguinos %>%
  select(especie, matches("_mm$"))

# filtrar filas: islas que empiezan con D
pinguinos %>%
  filter(str_detect(isla, "^D"))

# Regex para renombrar y extraer ------------------------------------------

# quitar la unidad de los nombres
pinguinos %>%
  rename_with(~ str_remove(.x, "_(mm|g)$"))

# crear una columna con la unidad
pinguino_largo_simple %>%
  mutate(unidad = str_extract(name, "[a-z]+$"))

# Regex dentro de pivot_longer(): names_pattern ---------------------------
# (.*)       primer grupo: todo hasta el último guion bajo -> largo_pico
# _          el guion bajo que separa
# ([a-z]+)$  segundo grupo: letras al final del nombre    -> mm

pinguinos %>%
  pivot_longer(contains("_"),
               names_to = c("variable", "unidad"),
               names_pattern = "(.*)_([a-z]+)$",
               values_drop_na = TRUE)

# Las regex no siempre ayudan ---------------------------------------------
# (.) es UN solo carácter: toma la letra antes y después del primer _
# largo_pico_mm -> variable = "o", unidad = "p" ... sin ningún error

pinguinos %>%
  pivot_longer(contains("_"),
               names_to = c("variable", "unidad"),
               names_pattern = "(.)_(.)")

# Úsenlas solo cuando sean necesarias y revisen el resultado

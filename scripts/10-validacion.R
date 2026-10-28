# Librerias ---------------------------------------------------------------

library(tidyverse)
library(datos)

# Cargar los datos --------------------------------------------------------

pinguinos <- datos::pinguinos
glimpse(pinguinos)

# Antes de empezar: cat() -------------------------------------------------
#cat() en R sirve para imprimir texto en la consola, concatenando
a <- 5
b <- 3
cat("La suma de", a, "y", b, "es", a + b)

# print() es para ver los objetos

# Datos desordenados a proposito ------------------------------------------

pinguinos_desordenados <- read_csv("datos/pinguinos_desordenado.csv",
                                   show_col_types = FALSE)

# Revisión exploratoria ---------------------------------------------------

# Paso 1: vista general
summary(pinguinos_desordenados)

# Paso 2: categoricas
unique(pinguinos_desordenados$especie)
unique(pinguinos_desordenados$isla)
unique(pinguinos_desordenados$sexo)
table(pinguinos_desordenados$especie, useNA = "always")
table(pinguinos_desordenados$anio,    useNA = "always")

# Paso 3: rangos de numericas (na.rm = TRUE ignora NA)
range(pinguinos_desordenados$largo_pico_mm,   na.rm = TRUE)
range(pinguinos_desordenados$masa_corporal_g, na.rm = TRUE)

# Paso 4: faltantes por columna
colSums(is.na(pinguinos_desordenados))

# Funcion de validacion ---------------------------------------------------
# Usa las reglas del diccionario para aprobar o fallar

validar_pinguinos <- function(datos) {
  # Estructural: especie valida (NA permitido)
  especie_ok <- all(datos$especie %in% c("Adelia", "Barbijo", "Papúa") |
                      is.na(datos$especie))

  # Estructural: isla valida
  isla_ok <- all(datos$isla %in% c("Biscoe", "Dream", "Torgersen") |
                   is.na(datos$isla))

  # Contenido: largo del pico entre 30 y 60 mm
  pico_ok <- all(datos$largo_pico_mm >= 30 & datos$largo_pico_mm <= 60,
                 na.rm = TRUE)

  # Dominio: masa corporal entre 2500 y 6500 g
  masa_ok <- all(datos$masa_corporal_g >= 2500 & datos$masa_corporal_g <= 6500,
                 na.rm = TRUE)

  # Contenido: anio en 2007-2009
  anio_ok <- all(datos$anio %in% c(2007, 2008, 2009) | is.na(datos$anio))

  marca <- function(x) ifelse(isTRUE(x), "OK", "FALLA")

  cat("Especie valida (estructural): ", marca(especie_ok), "\n")
  cat("Isla valida (estructural):    ", marca(isla_ok),    "\n")
  cat("Pico 30-60 mm (contenido):    ", marca(pico_ok),    "\n")
  cat("Masa 2500-6500 g (dominio):   ", marca(masa_ok),    "\n")
  cat("Anio 2007-2009 (contenido):   ", marca(anio_ok),    "\n")

}

cat("\n== Datos limpios ==\n")
validar_pinguinos(pinguinos)              # deberia pasar todo

cat("\n== Datos desordenados ==\n")
validar_pinguinos(pinguinos_desordenados) # deberia fallar varias

# Reportes ----------------------------------------------------------------

pinguinos_flags <- pinguinos_desordenados %>%
  mutate(
    fila = row_number(),
    flag_especie = !(especie %in% c("Adelia", "Barbijo", "Papúa") | is.na(especie)),
    flag_isla    = !(isla %in% c("Biscoe", "Dream", "Torgersen") | is.na(isla)),
    flag_pico    = !is.na(largo_pico_mm) & (largo_pico_mm < 30 | largo_pico_mm > 60),
    flag_masa    = !is.na(masa_corporal_g) & (masa_corporal_g < 2500 | masa_corporal_g > 6500),
    flag_anio    = !(anio %in% c(2007, 2008, 2009) | is.na(anio))
  ) %>%
  mutate(
    # total de banderas por fila (TRUE suma 1)
    n_flags = flag_especie + flag_isla + flag_pico + flag_masa + flag_anio,
    # motivos en texto: pega los nombres de las reglas que fallaron
    motivos = pmap_chr(
      list(flag_especie, flag_isla, flag_pico, flag_masa, flag_anio),
      \(e, i, p, m, a) paste(
        c("especie", "isla", "pico", "masa", "anio")[c(e, i, p, m, a)],
        collapse = ", "
      )
    )
  )

# Solo las filas con al menos una red flag, de peor a mejor
filas_problematicas <- pinguinos_flags %>%
  filter(n_flags > 0) %>%
  arrange(desc(n_flags)) %>%
  select(fila, n_flags, motivos,
         especie, isla, largo_pico_mm, masa_corporal_g, anio)

cat("\nFilas con al menos una red flag:", nrow(filas_problematicas), "\n")
print(filas_problematicas)

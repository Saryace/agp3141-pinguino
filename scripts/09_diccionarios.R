# Librerias ---------------------------------------------------------------

# install.packages("datawizard") # instalar
library(tidyverse)
library(datos)
library(datawizard)

# Cargar los datos --------------------------------------------------------

pinguinos <- datos::pinguinos
glimpse(pinguinos)


# Genero información en un dataframe --------------------------------------
# Diccionario a mano (escrito con conocimiento del tema)
# Campos: variable, etiqueta, tipo, unidad, valores validos, faltantes

diccionario_manual <- tribble(
  ~variable,          ~etiqueta,              ~tipo,          ~unidad, ~valores_validos,            ~faltantes,
  "especie",          "Especie del pinguino", "categorica",   NA,      "Adelia, Barbijo, Papua",    "sin NA",
  "isla",             "Isla de observacion",  "categorica",   NA,      "Biscoe, Dream, Torgersen",  "sin NA",
  "largo_pico_mm",    "Largo del pico",       "numerica",     "mm",    "32.1 - 59.6",               "NA",
  "alto_pico_mm",     "Alto del pico",        "numerica",     "mm",    "13.1 - 21.5",               "NA",
  "largo_aleta_mm",   "Largo de la aleta",    "numerica",     "mm",    "172 - 231",                 "NA",
  "masa_corporal_g",  "Masa corporal",        "numerica",     "g",     "2700 - 6300",               "NA",
  "sexo",             "Sexo biologico",       "categorica",   NA,      "hembra, macho",             "NA",
  "anio",             "Ano de la medicion",   "numerica",     "ano",   "2007, 2008, 2009",          "sin NA"
)

print(diccionario_manual)
write_csv(diccionario_manual, "datos/pinguinos_diccionario_manual.csv")


# Paquetes que ayudan -----------------------------------------------------
#  Diccionario automatico con datawizard::data_codebook()
#  Aporta tipo, % de faltantes, rangos y frecuencias

codebook_auto <- data_codebook(pinguinos)
print(codebook_auto)

# Diccionario etiquetado --------------------------------------------------

pinguinos_etiquetado <- pinguinos %>%
  mutate(
    especie         = structure(especie,         label = "Especie observada en el archipielago Palmer"),
    isla            = structure(isla,            label = "Isla donde se observo al pinguino"),
    largo_pico_mm   = structure(largo_pico_mm,   label = "Largo del pico, de la punta a la base (mm)"),
    alto_pico_mm    = structure(alto_pico_mm,    label = "Alto del pico en la base (mm)"),
    largo_aleta_mm  = structure(largo_aleta_mm,  label = "Largo de la aleta, del cuerpo a la punta (mm)"),
    masa_corporal_g = structure(masa_corporal_g, label = "Masa corporal medida en terreno (g)"),
    sexo            = structure(sexo,            label = "Sexo biologico (hembra, macho o NA)"),
    anio            = structure(anio,            label = "Ano de la medicion (2007-2009)")
  )

codebook_etiquetado <- data_codebook(pinguinos_etiquetado)
print(codebook_etiquetado)

write.csv(codebook_etiquetado,
          "datos/pinguinos_codebook.csv",
          row.names = FALSE)

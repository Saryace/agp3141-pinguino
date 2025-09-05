# Fuente ------------------------------------------------------------------
# https://heads0rtai1s.github.io/2020/11/05/r-python-dplyr-pandas/
# https://allendowney.github.io/ThinkStats/chap10.html
# ChatGPT 5.0 para verificar opciones en equivalencias de funciones

# Cargo librerias ---------------------------------------------------------
library(tidyverse)

# Cargo datos pinguinos ---------------------------------------------------
pinguinos <- read_excel("datos/penguins.xlsx")

# Glimpse : mirar ---------------------------------------------------------
glimpse(pinguinos)

dim(pinguinos) # filas x columnas

# Head y summary ----------------------------------------------------------
head(pinguinos)

summary(pinguinos)

# Exploración -------------------------------------------------------------

pinguinos %>%
  filter(bill_length_mm < 34) %>%
  select(species, sex, bill_length_mm)

# Grupos ------------------------------------------------------------------

pinguinos %>%
  group_by(species) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE))

# EDA ---------------------------------------------------------------------

# Los Gentoo tienen mayor masa corporal promedio que Adelie y Chinstrap?

pinguinos_procesados <- pinguinos %>%
  filter(!is.na(body_mass_g) & !is.na(species))

# Por especie

pinguinos_procesados %>%
  group_by(species) %>%
  summarise(
    n = n(),
    promedio = mean(body_mass_g),
    sd = sd(body_mass_g)
  )

# EDA visual

ggplot(pinguinos_procesados, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Especie", y = "Masa corporal (g)", title = "Masa corporal por especie") +
  theme_bw() +
  theme(legend.position = "none")

# ANOVA (asumiendo normalidad)

ajuste_anova <- aov(body_mass_g ~ species, data = pinguinos_procesados)
summary(ajuste_anova) # Pr(>F)< 0.05 hay diferencias sig

# Post-hoc

TukeyHSD(ajuste_anova)






# Código replicado desde Allison Horst ------------------------------------

mass_flipper <- ggplot(data = pinguinos,
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")


# Guardar plot como TIFF --------------------------------------------------

ggsave("plot/plot-pinguino.tiff", mass_flipper, height = 7, width = 8,
       device = "tiff", dpi = 300)

GGenemy::gg_audit(mass_flipper)


# -------------------------------------------------------------------------

pinguinos |>
  ggauto(body_mass_g,flipper_length_mm) # interesante


# -------------------------------------------------------------------------

library(gghighlight)

ggplot(data = pinguinos,
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point(aes(color = bill_length_mm )) +
  gghighlight(bill_length_mm < 45) +
  facet_wrap(vars(species))

x <- c("Adelie", "Gentoo", "Chinstrap")
ggonion(x, ratio = 1, bias = 1, color = NA)


# -------------------------------------------------------------------------

ggplot(pinguinos, aes(x = 1, y = body_mass_g, fill = 	species)) +
  geom_rain(alpha = 0.3)

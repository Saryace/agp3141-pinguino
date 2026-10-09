gato_tiene_hambre <- function(gato_maulla, gato_toca_plato){

  if(gato_maulla == TRUE & gato_toca_plato == TRUE)

    return("tiene hambre! 🐟")

  else("todavía no tiene hambre 😴")
}

# Probamos
gato_tiene_hambre(gato_maulla = TRUE, gato_toca_plato = TRUE)

gato_tiene_hambre(gato_maulla = TRUE, gato_toca_plato = FALSE)

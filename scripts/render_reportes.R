# render_reportes.R
# Renderiza el .qmd una vez por especie y guarda un PDF por cada una.
# La clave: NO se generan varios PDF desde dentro del .qmd; se llama a
# quarto_render() una vez por valor del parámetro, aquí con purrr::walk().

library(quarto)
library(palmerpenguins)
library(glue)
library(purrr)

# Las 3 especies salen solas de los niveles del factor:
especies <- levels(penguins$species)   # "Adelie" "Chinstrap" "Gentoo"

# walk() = map() para efectos colaterales (no devuelve nada, solo renderiza)
walk(especies, \(sp) {
  message(glue("Renderizando: {sp}"))
  quarto_render(
    input          = "reporte_pinguino.qmd",
    execute_params = list(especie = sp),   # <- cambia el parámetro
    output_file    = glue("reporte_{sp}.pdf"),
    quiet          = TRUE                   # evita el bug cli/{} del paquete quarto
  )
})

message("Listo. Los PDF quedaron junto al .qmd.")

tti_get_registros_por_id <- function(
  id,
  tm = "f",
  sys_sleep = 0.01) {
  total_registros <- tti_get_registros_json_n(id)

  pages <- ceiling(total_registros/20)

  if (tm == "f") {
    tipo <- "fotográficos"
  }

  if (tm == "s") {
    tipo <- "de áudio"
  }

  if (pages >= 1) {
    glue::glue("ID {id} tem {total_registros} registros {tipo}.\n\n") |>
      crayon::yellow() |>
      cat()
  }

  if (total_registros == 0) {
    glue::glue("ID {id} tem {total_registros} registros {tipo}.\n\n") |>
      crayon::red() |>
      cat()

    return(
      tibble(
        id = as.integer(id)
      )
    )
  }

  pb <-
    progress::progress_bar$new(
      total = pages,
      format = glue::glue(
        "Obtendo metadados. [:bar] Página :current de :total."
      ),
      clear = F
    )

  registros <-
    seq(pages) |>
    purrr::map_dfr(
      ~{
        pb$tick()

        Sys.sleep(sys_sleep)

        tti_get_registros_json(
          id = id,
          p = .,
          tm = tm
        )
      }
    )

  return(registros)
}

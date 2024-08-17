tti_get_registros_json <- function(
    id,
    p = 1,
    rm = c("s", "f"),
    t = "s") {
    fromJSON_safe <- purrr::possibly(
      jsonlite::fromJSON,
      list(
        registros = list(
          titulo = "",
          link = "",
          total = "",
          itens = list(itens = "")
        )
      )
    )

    glue::glue("https://www.wikiaves.com.br/getRegistrosJSON.php?tm={tm[1]}&t={t}&s={id}&o=mp&p={p}") |>
      fromJSON_safe() |>
      purrr::pluck("registros") |>
      purrr::map_if(is.null, ~"") |>
      tibble::as_tibble() |>
      dplyr::mutate(
        itens = purrr::map(
          itens,
          as.data.frame,
          stringsAsFactors = F
        )
      ) |>
      dplyr::select(-link) |>
      tidyr::unnest_wider(itens)
}

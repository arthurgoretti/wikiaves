tti_metadata <- function(
    term,
    tm = "f",
    verbose = T,
    download = F,
    path = getwd(),
    mp3_file_name = "{label}-{id}.mp3",
    parallel = 1,
    metadata_sys_sleep = .1,
    download_sys_sleep = .0001,
    force = F) {
  wa_metadata <-
    tibble::tibble(
      term = term
    ) |>
    dplyr::mutate(
      taxonomy = purrr::map(
        term,
        tti_get_taxons_json
      )
    ) |>
    tidyr::unnest(taxonomy)

  if (verbose) {
    cat("Species IDs found from terms:\n")
  }

  wa_metadata <- wa_metadata |>
    dplyr::rename(species_id = id) |>
    dplyr::distinct(species_id,
             .keep_all = T) |>
    dplyr::mutate(
      verbose = purrr::map2(
        term,
        species_id,
        ~{
          if (verbose) {
            cat(glue::glue("species_id = {.y} (from term '{.x}')\n"))
            cat("\n")
          }
          return(NULL)
        }
      ),
      registers = purrr::map(
        species_id,
        ~{
          tti_get_registros_por_id(.,
                                 tm = tm,
                                 sys_sleep = metadata_sys_sleep)
        }
      )
    ) |>
    dplyr::select(-verbose) |>
    tidyr::unnest(registers) |>
    dplyr::mutate(
      mp3_name = glue::glue(mp3_file_name) |>
        stringr::str_replace(" ", "-"),
      mp3_link = link |>
        stringr::str_replace("jpg$", "mp3") |>
        stringr::str_replace("#_", "_")
    )

  if (verbose) {
    cat(nrow(wa_metadata),
        "registros avaliados com base em",
        dplyr::n_distinct(wa_metadata$species_id),
        "IDs distintos.\n")
  }

  if(download) {
    wa_download(wa_metadata, path = path, verbose = verbose, sys_sleep = download_sys_sleep, force = force)
  }

  return(wa_metadata)
}

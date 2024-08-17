tti_get_taxons_json <- function(
    term) {
  glue::glue("https://www.wikiaves.com.br/getTaxonsJSON.php?term={term}") |>
    URLencode() |>
    jsonlite::fromJSON()
}

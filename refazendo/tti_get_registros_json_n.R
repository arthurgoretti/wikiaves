tti_get_registros_json_n <- function(
    id,
    tm = "f") {
    result <-
      tti_get_registros_json(
        id = id,
        p = 1,
        tm = tm
      )

    n <- as.numeric(result$total[1])

    if(is.na(n)) n <- 0

    return(n)
}

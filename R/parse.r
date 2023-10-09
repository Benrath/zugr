# Parse response from calls to the fahrplan endpoint
# @noRd
parse_response <- function(x) {
  # shut up notes
  start <- NULL
  end <- NULL
  duration <- NULL
  
  # TODO: better way to parse this?
  result_list <- lapply(x, function(r) {
    bind_rows(lapply(r[["verbindungen"]], function(v) {
      n_abschnitt <- length(v[["verbindungsAbschnitte"]])
      tibble::tibble(
        id = v[["tripId"]],
        duration = v[["verbindungsDauerInSeconds"]],
        price = v[["angebotsPreis"]][["betrag"]],
        changes = v[["umstiegsAnzahl"]],
        start = v[["verbindungsAbschnitte"]][[1]][["abfahrtsZeitpunkt"]],
        end = v[["verbindungsAbschnitte"]][[n_abschnitt]][["ankunftsZeitpunkt"]]
      )
    }))
  })
  
  result <- dplyr::bind_rows(result_list)
  
  result <- result %>%
    dplyr::mutate(
      start = lubridate::force_tz(lubridate::ymd_hms(start), "Europe/Berlin"),
      end = lubridate::force_tz(lubridate::ymd_hms(end), "Europe/Berlin"),
      duration = lubridate::as.duration(duration)
    )
  
  return(result)
}

get_last <- function(x) {
  v <- utils::tail(x[["verbindungen"]], 1)
  a <- utils::tail(v[[1]][["verbindungsAbschnitte"]], 1)
  last_time <- lubridate::ymd_hms(a[[1]][["ankunftsZeitpunkt"]])
  last_time <- lubridate::force_tz(last_time, "Europe/Berlin")
  return(last_time)
}

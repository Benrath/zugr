# Make a request tot the fahrplan endpoint
# @noRd
get_results <- function(req_body) {
  response <- httr2::request("https://www.bahn.de/web/api/angebote/fahrplan")
  response <- httr2::req_headers(response, "user-agent" = "bahnr")
  response <- httr2::req_body_json(response, req_body)
  response <- httr2::req_throttle(response, 5 / 60)
  response <- httr2::req_perform(response)
  result <- httr2::resp_body_json(response)
  return(result)
}

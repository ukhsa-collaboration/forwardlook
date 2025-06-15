#' Get list of organisations in UK Government
#'
#' @return A tibble.
#' @export
#'
#' @importFrom httr GET status_code content
#' @importFrom cli cli_alert_warning
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange
#'
#' @examples
#' get_org_info()
get_org_info <- function() {

  # GOV.UK organisations API:
  base_url <- "https://www.gov.uk/api/organisations"

  # Start with page 1:
  page <- 1

  # Create empty list for results:
  all_orgs <- list()

  # For each page...
  repeat {

    # Complete URL by adding page number:
    url <- paste0(base_url, "?page=", page)

    # Check URL is valid:
    response <- httr::GET(url)

    # Stop if URL is invalid page:
    if (httr::status_code(response) != 200) {
      cli::cli_alert_warning("Failed to fetch page {i}.")
      break
    }

    # Read in list of organisations from JSON:
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content_json)

    if (length(data$results) == 0) break

    # Combine organisation name and GOV.UK homepage link into tibble:
    orgs <- tibble::tibble(
      title = data$results$title,
      url = data$results$web_url
    )

    # Combine into list:
    all_orgs[[page]] <- orgs

    # Increase page counter by 1:
    page <- page + 1

  }

  # Join all organisation info together:
  orgs_tbl <- dplyr::bind_rows(all_orgs)

  # Arrange organisation info in alphabetical order:
  orgs_tbl <- dplyr::arrange(orgs_tbl, title)

  # Convert ampersand to "and" in organisation name:
  orgs_tbl$title <- gsub("&", "and", orgs_tbl$title)

  # Create organisation name slug by removing beginning of URL:
  orgs_tbl$slug <- gsub("https://www.gov.uk/government/organisations/", "", orgs_tbl$url)

  # Print tibble of organisations:
  return(orgs_tbl)

}

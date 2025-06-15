#' Get research and statistics announcements from GOV.UK
#'
#' @param organisation UK Government department, agency or public body.
#' @param type Type of announcement: all research and statistics, published statistics, upcoming statistics, cancelled statistics or research.
#' @param start_date Currently not used.
#' @param end_date Currently not used.
#' @param drop_weekly If `TRUE`, announcements for weekly publications are removed.
#' @param verbose If `TRUE`, the default, update message(s) are returned.
#'
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom purrr map_df
#' @importFrom dplyr arrange filter mutate
#' @importFrom lubridate dmy
#' @importFrom stringr str_detect
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' forward_look("Department of Health and Social Care")
forward_look <- function(organisation = "UK Health Security Agency", type = "upcoming statistics", start_date = NULL, end_date = NULL, drop_weekly = FALSE, verbose = TRUE) {

  # Check `drop_weekly` is a logical:
  if (!is.logical(drop_weekly)) {
    cli::cli_abort("{.var drop_weekly} must be a logical vector (TRUE or FALSE).")
  }

  # Check `verbose` is a logical:
  if (!is.logical(verbose)) {

    cli::cli_abort("{.var verbose} must be a logical not a {class(verbose)}.")

  }

  # Get vector of announcement URLs:
  urls <- get_announcement_url(organisation, type)

  if (verbose) {

    # Print update message:
    cli::cli_alert_info("Scraping announcement information from GOV.UK...")

  }

  if (verbose) {

    # Get announcement information into a tibble with progress bar:
    tbl <-
      purrr::map_df(urls, get_announcement_info, .progress = TRUE) |>
      dplyr::mutate(date = lubridate::dmy(date)) |>
      dplyr::arrange(date)
    # dplyr::filter(date >= from & date <= to)

  } else {

    # Get announcement information into a tibble without progress bar:
    tbl <-
      purrr::map_df(urls, get_announcement_info) |>
      dplyr::mutate(date = lubridate::dmy(date)) |>
      dplyr::arrange(date)
    # dplyr::filter(date >= from & date <= to)

  }

  # Remove announcements for weekly publications:
  if (drop_weekly) {

    tbl <-
      tbl |>
      dplyr::filter(!stringr::str_detect(title, "Week|Weekly|week|weekly"))

  }

  # Print announcement information:
  return(tbl)

}

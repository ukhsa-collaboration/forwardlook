#' Get GOV.UK research and statistics announcement information
#'
#' @param url URL of a research or statistics announcement.
#' @param verbose If `TRUE`, the default, update message(s) are returned.
#'
#' @importFrom cli cli_abort
#' @importFrom rvest html_nodes html_text
#'
#' @return A list.
#' @export
#'
get_announcement_info <- function(url, verbose = TRUE) {

  # URL checker:
  is_url <- function(x) {
    grepl("^https?://", x)
  }

  # Check is URL begins https://
  if (!is_url(url)) {
    cli::cli_abort("{.var url} is not a valid URL.")
  }

  # Get title:
  get_title <- function(page) {
    page |>
      rvest::html_nodes(".govuk-heading-l") |>
      rvest::html_text(trim = TRUE)
  }

  # Get description:
  get_description <- function(page) {
    page |>
      rvest::html_nodes(".gem-c-lead-paragraph") |>
      rvest::html_text(trim = TRUE)
  }

  # Get type:
  get_type <- function(page) {
    page |>
      rvest::html_nodes(".gem-c-heading__context") |>
      rvest::html_text(trim = TRUE)
  }

  # Get organisation:
  get_org <- function(page) {

    scraped_page <-
      page |>
      rvest::html_nodes(".gem-c-metadata__definition") |>
      rvest::html_text(trim = TRUE)

    org <- date <- scraped_page[1]

    return(org)

  }

  # Get date:
  get_date <- function(page) {

    scraped_page <-
      page |>
      rvest::html_nodes(".gem-c-metadata__definition") |>
      rvest::html_text(trim = TRUE)

    # Explanation of the regex:
    # \\b\\d{1,2} — Day (1 or 2 digits, word boundary before)
    # \\w+ — Month (word characters)
    # \\d{4} — Year (4 digits)
    # \\d{1,2}:\\d{2}(am|pm) — Time in 12-hour format
    # \\([^()]+\\) — Status in parentheses (e.g., (provisional))

    pattern <- "\\b\\d{1,2} \\w+ \\d{4} \\d{1,2}:\\d{2}(am|pm) \\([^()]+\\)"

    #
    statement <- scraped_page[grepl(pattern, scraped_page)]

    date <- sub("^(\\d+ \\w+ \\d{4}).*$", "\\1", statement)

    #
    if (length(date) == 0) {
      statement <- scraped_page[grepl("provisional", scraped_page)]
      part_date <- gsub(" \\(provisional\\)", "", statement)
      date <- paste("1", part_date)
    }

    return(date)

  }

  # Get time:
  get_time <- function(page) {

    scraped_page <-
      page |>
      rvest::html_nodes(".gem-c-metadata__definition") |>
      rvest::html_text(trim = TRUE)

    #
    pattern <- "\\b\\d{1,2} \\w+ \\d{4} \\d{1,2}:\\d{2}(am|pm) \\([^()]+\\)"

    #
    statement <- scraped_page[grepl(pattern, scraped_page)]

    time <- sub("^\\d+ \\w+ \\d{4} (\\d+:\\d+[ap]m).*", "\\1", statement)

    #
    if (length(time) == 0) {
      time <- NA_character_
    }

    return(time)

  }

  # Get status:
  get_status <- function(page){

    scraped_page <-
      page |>
      rvest::html_nodes(".gem-c-metadata__definition") |>
      rvest::html_text(trim = TRUE)

    pattern <- "\\b\\d{1,2} \\w+ \\d{4} \\d{1,2}:\\d{2}(am|pm) \\([^()]+\\)"

    statement <- scraped_page[grepl(pattern, scraped_page)]

    status <- sub(".*\\(([^)]+)\\)$", "\\1", statement)

    #
    if (length(status) == 0) {
      statement <- scraped_page[grepl("provisional", scraped_page)]
      status <- sub(".*\\(([^)]+)\\)$", "\\1", statement)
    }

    return(status)

  }

  # Read HTML page:
  page <- rvest::read_html(url)

  # Scrape announcement information and collate into list:
  list(
    date = get_date(page),
    time = get_time(page),
    title = get_title(page),
    description = get_description(page),
    type = get_type(page),
    status = get_status(page),
    organisation = get_org(page),
    url = url
  )

}

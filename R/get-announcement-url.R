#' Get GOV.UK research and statistics announcement URLs
#'
#' @param organisation UK Government department, agency or public body.
#' @param type Type of announcement: all research and statistics, published statistics, upcoming statistics, cancelled statistics or research.
#' @param verbose If `TRUE`, the default, update message(s) are returned.
#'
#' @importFrom cli cli_abort cli_alert cli_alert_info cli_alert_warning
#' @importFrom httr GET status_code
#' @importFrom rvest read_html html_nodes html_attr
#'
#' @return A character vector of announcement URLs.
#' @export
#'
#' @examples
#' get_announcement_url()
get_announcement_url <- function(organisation = "UK Health Security Agency", type = "upcoming statistics", verbose = TRUE) {

  # Check `organisation` is a character:
  if (!is.character(organisation)) {
    cli::cli_abort("{.var organisation} must be a {.cls character} vector.")
  }

  # Load GOV.UK organisation list (if file doesn't exist then info will be scraped from GOV.UK organisations API):
  if (!file.exists("data/govukorgs.rda")) {

    if (verbose) {
      cli::cli_alert_info("Calling GOV.UK organisations API. Updating organisation information...")
    }

    # Scrape list of organisations from GOV.UK API and save in \data:
    update_org_info()

  }

  # Read in list of GOV.UK organisations:
  load("data/govukorgs.rda")

  # Check if user-specified `organisation` is a valid option:
  if (!organisation %in% govuk_orgs$title) {
    cli::cli_abort(c(
      x = "{.var organisation} is not a valid department, agency or public body.",
      i = "Check spelling and that organisation is indeed part of UK Government."
    ))
  }

  # Check `type` is a character:
  if (!is.character(type)) {
    cli::cli_abort("{.var type} must be a {.cls character} vector.")
  }

  # Force `type` to lower case:
  type <- tolower(type)

  # Create vector of statistics announcement types:
  type_vec <- c(
    "all",
    "published statistics",
    "upcoming statistics",
    "cancelled statistics",
    "research"
  )

  # Check if user-specified `type` is a valid option:
  if (!type %in% type_vec) {
    cli::cli_abort(c(
      x = "{.var type} is not a valid statistics announcement.",
      i = "{.var type} must be either all, published statistics, upcoming statistics, cancelled statistics, or research."
    ))
  }

  # Convert `start_date` and `end_date` to date:
  # start_date <- as.Date(start_date)
  # end_date <- as.Date(end_date)
  # Check `start date`:
  # Check `end date`:

  # Check `verbose` is a logical:
  if (!is.logical(verbose)) {

    cli::cli_abort("{.var verbose} must be a logical not a {class(verbose)}.")

  }

  # Extract user-specified organisation name from master list:
  matched_info <- subset(govuk_orgs, govuk_orgs$title == organisation)

  # Pull out organisation:
  matched_org <- matched_info$title

  # Pull out URL slug:
  matched_org_slug <- matched_info$slug

  # Match user-specified `type` with possible options:
  matched_type <- type_vec[grep(type, type_vec, ignore.case = TRUE)]

  # Get URL slug for type of statistics announcement:
  matched_type <- switch(
    matched_type,
    "all" = "all_research_and_statistics",
    "published statistics" = "statistics_published",
    "upcoming statistics" = "upcoming_statistics",
    "cancelled statistics" = "cancelled_statistics",
    "research" = "research"
  )

  # Paste organisation and type of R&S into URL:
  rands_url <- paste0(
    "https://www.gov.uk/search/research-and-statistics?content_store_document_type=",
    matched_type,
    "&order=release-date-oldest&organisations%5B%5D=",
    matched_org_slug,
    "&page="
  )

  # Start with page 1:
  i <- 1

  # Create empty list for results:
  all <- list()

  # Create empty vector to track number of announcements:
  n_announcements <- c()

  # For each page (1 to n) scrape all announcement URLs:
  repeat{

    # Paste page number into Research and Statistics URL:
    url <- paste0(rands_url, i)

    # Check URL is valid:
    response <- httr::GET(url)

    # Stop if URL is invalid page:
    if (httr::status_code(response) != 200) {
      cli::cli_alert_warning("Failed to fetch page {i}.")
      break
    }

    # Read HTML page:
    page <- rvest::read_html(url)

    # Pull out all URLs from HTML page:
    results <- page |>
      rvest::html_nodes(".gem-c-document-list__item-title a") |>
      rvest::html_attr("href")

    # Check number of items to see if a result will be returned:
    urls_on_page <- length(unlist(results))

    # If there are no announcements on the page then stop:
    if(urls_on_page == 0){
      break
    }

    if (verbose) {

      # Print update message:
      cli::cli_alert("{urls_on_page} announcements found on page {i}.")

    }

    # Combine results into list:
    all[[i]] <- results

    # Combine number of announcements on each page into vector:
    n_announcements <- c(n_announcements, urls_on_page)

    #
    i <- i + 1

  } # end of repeat

  # Get total number of announcements:
  announcements_total <- sum(n_announcements)

  if (verbose) {

    # Print update message:
    cli::cli_alert_info("Done! Total number of {type} announcements for {matched_org}: {announcements_total}.")

  }

  # Convert list of URLs into vector:
  all_urls <- unique(unlist(all))

  # Complete URLs by adding gov.uk to beginning:
  all_urls <- paste0("https://www.gov.uk", all_urls)

  # Print all statistics announcement URLs:
  return(all_urls)

}

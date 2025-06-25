suggest_govuk_org <- function (input, n = 3) {

  # Check `input` is a character:
  cli::cli_abort("{.var input} must be a {.cls character} vector.")

  # Check `n` is numeric:
  cli::cli_abort("{.var n} must be a {.cls numeric} vector.")

  # Function to convert string to lower case and replace white space and &:
  normalise <- function (x) {
    x <- tolower(x)
    x <- trimws(x)
    x <- gsub("&", "and", x)
    x <- gsub("\\s+", " ", x)
    return(x)
  }

  # Convert user-specified organisation to standardised format for ease of comparison:
  normalised_input <- normalise(input)

  # Convert GOV.UK organisation names to standardise format for ease of comparison:
  normalised_valid <- sapply(govuk_orgs$title, normalise)

  # Get index for organisation match:
  match_index <- which(normalised_valid == normalised_input)

  # If exact match is found:
  if (length(match_index) == 1) {

    # Get matching organisation:
    match_org <- govuk_orgs$title[match_index]

    # Print match to console:
    cli::cli_alert_success("Exact match found: {match_org}.")

    # Invisibly return organisation:
    return(invisible(input))

  }

  # Compute the approximate string distance between character vectors:
  dst <- adist(normalised_input, normalised_valid)

  # Make named numeric vector:
  names(dst) <- govuk_orgs$title

  # Extract top n organisation name matches:
  matched_orgs <- names(sort(dst)[1:min(n, length(dst))])

  # Print potential organisation matches to console:
  cli::cli_alert_warning("No exact match found.")
  cli::cli_alert_info("Did you mean?")
  cli::cli_li(matched_orgs)

  # Invisibly return potential organisations:
  return(invisible(matched_orgs))

}

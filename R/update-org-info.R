#' Gather and save list of organisations in UK Government
#'
#' @return Nothing. List of organisations saved in data/
#' @export
#'
#' @examples
#' update_org_info()
update_org_info <- function () {

  # Gather all UK Government organisations from API:
  govuk_orgs <- get_org_info()

  # Save to data/:
  save(govuk_orgs, file = "data/govukorgs.rda")

}

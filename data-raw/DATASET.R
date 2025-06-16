# Code to prepare `DATASET` dataset goes here:

# Get list of UK Government organisations from GOV.UK API:
govuk_orgs <- get_org_info()

# Write organisation data into R/sysdata.rda:
usethis::use_data(govuk_orgs, overwrite = TRUE)

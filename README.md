# forwardlook

> Get upcoming and cancelled UK government statistics announcements from [GOV.UK](https://www.gov.uk/search/research-and-statistics) into R.

## Overview

`forwardlook` is an R package which contains tools to scrape upcoming and cancelled statistics announcements from GOV.UK. It helps analysts, journalists, researchers, and policy professionals stay informed about what data and statistics are scheduled for publication by His Majesty's Government.

## Installation

You can install the current version of `forwardlook` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ukhsa-collaboration/forwardlook")
```

## Usage

The package contains 1 main function: `forward_look()`.

`forward_look()` takes 2 main arguments:

1.  `organisation`: the name of the organisation publishing the statistics
2.  `type`: the type of statistics being published

For example:

``` r
forward_look(
  organisation = "UK Health Security Agency",
  type = "upcoming"
  )
```

Will return the date, time, title, description, type, accreditation level, organisation and URL of all upcoming statistics announcements (105 as at 25 June 2025).

The user can also specify whether to include weekly publications in the output (`drop_weekly = TRUE`) and whether the function prints status updates in the console (`verbose = TRUE`). Note that there is currently no functionality for the `start_date` and `end_date` arguments.

The [GOV.UK Organisations API](https://www.gov.uk/api/organisations) is used to convert the user-specified organisation name into the format required to complete the URL. The URL is then scraped and the statistics announcement information returned in a tibble.

## Contributing

Please note that the `forwardlook` project is released with a [Contributor Code of Conduct](.github/CONTRIBUTING.md). By contributing to this project you agree to abide by its terms.

## Licence

Code: MIT Licence

Documentation: Crown copyright (c) 2025

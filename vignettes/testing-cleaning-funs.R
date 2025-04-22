if(require(testthat)){
  source("cleaning-funs.R")
  library(dplyr)

  testthat::test_that(
    "localize_beach",

    {
      testthat::skip_on_ci()
      swims <- read.csv( "swim.csv")

      expect_equivalent(
      localize_beach(swims),
      bind_cols(swims, tibble(english = c("US", "US", "UK", "US", "UK")))
      )

      expect_error(
        localize_beach(tibble(bad_col="see")),
                   "Join columns in")

      expect_equal(
        localize_beach(tibble(where="seeshore")) |>
          pull(english),
        NA_character_
      )

      expect_contains(
        bind_cols(swims,
                  tibble(english = c("US", "US", "UK", "US", "UK"))) |>
          localize_beach() |> names(),
        "english.x"
      )

    })


}

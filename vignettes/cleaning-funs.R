# define functions
#' Localize the name of a beach based on where you are.
#'
#' @param dat data.frame containing a column "where"
#'
#' @returns data.frame dat with column "english" added.
#'
#' @examples tibble(where = "coast") |> localize_beach()
localize_beach <- function(dat) {
  lookup_table <- tribble(
    ~where, ~english,
    "beach",     "US",
    "coast",     "US",
    "seashore",     "UK",
    "seaside",     "UK"
  )
  left_join(dat, lookup_table, by = "where")
}

#' Convert Fahrenheit to Celsius
#'
#' @param x numeric value or vector in Fahrenheit
#'
#' @returns numeric value or vector in Celsius
#'
#' @examples f_to_c(32)
f_to_c <- function(x) {(x - 32) * 5/9}

#' Convert temperature to C if in F
#'
#' @param dat data frame with temp  and english column
#'
#' @returns data frame dat with all values of temp in C
#'
#' @examples tibble(temp = c(0, 32), english = c("UK", "US")) |> celsify_temp()
celsify_temp <- function(dat) {
  mutate(dat, temp = if_else(english == "US", f_to_c(temp), temp))
}

#' Create output file from input filename
#'
#' Addes date and time to start of the filename
#'
#' @param infile string input file path
#'
#' @returns string of output file path with date time added.
#'
#' @examples outfile_path("inputfile.txt")
outfile_path <- function(infile) {
  now <- Sys.time()
  timestamp <- format(now, "%Y-%B-%d_%H-%M-%S")
  paste0(timestamp, "_", str_replace(infile, "(.*)([.]csv$)", "\\1_clean\\2"))
}

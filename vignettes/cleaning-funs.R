# define functions
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

f_to_c <- function(x) (x - 32) * 5/9

celsify_temp <- function(dat) {
  mutate(dat, temp = if_else(english == "US", f_to_c(temp), temp))
}

outfile_path <- function(infile) {
  now <- Sys.time()
  timestamp <- format(now, "%Y-%B-%d_%H-%M-%S")
  paste0(timestamp, "_", str_replace(infile, "(.*)([.]csv$)", "\\1_clean\\2"))
}

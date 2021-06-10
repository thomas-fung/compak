## code to prepare `days` dataset goes here
days <- c(
  rep(25, 5), rep(26, 5), rep(27, 7), rep(28, 8),
  rep(29, 11), rep(30, 2), rep(31, 1), rep(32, 4),
  rep(33, 4), rep(34, 2), rep(35, 2)
)
usethis::use_data(days, overwrite = TRUE)

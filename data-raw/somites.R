## code to prepare `somites` dataset goes here
somites <- c(
  79, 89, 89, 93, 95, 99, 99, 100, 102, 102, rep(104, 3), 105,
  106, 107, 107, 113, rep(114, 3),
  rep(115, 3), 118, 118, rep(120, 5),
  122, 123, 123, 124, 124, rep(125, 5),
  126, 126, 127, rep(128, 5), rep(129, 4),
  130, rep(131, 4), rep(132, 7),
  rep(133, 4), rep(134, 3), rep(135, 5),
  rep(136, 6), rep(137, 9), rep(138, 10),
  rep(139, 6), rep(140, 5), rep(141, 17),
  rep(142, 27), rep(143, 21), rep(144, 24),
  rep(145, 37), rep(146, 32), rep(147, 31),
  rep(148, 29), rep(149, 26), rep(150, 51),
  rep(151, 15), rep(152, 15), rep(153, 12),
  rep(154, 9), rep(155, 9), rep(156, 10),
  rep(157, 4), rep(159, 2), 162, 162, 164
)
usethis::use_data(somites, overwrite = TRUE)

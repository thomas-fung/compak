## code to prepare `pc_data` dataset goes here
##
pc_data <- list(logZ = logZ, logLambda = logLambda)
usethis::use_data(pc_data, overwrite = TRUE, internal = TRUE)

testNc <- function (a) {
  listFiles <- list.files(a, pattern = ".nc",ignore.case = TRUE)
  if (is.na(listFiles[1])) FALSE else TRUE
}
png2 <- function (filename = "Rplot%03d.png", width = 1000, ratio = 1, ...) {
  png(filename, width = 1000, height = width * ratio, ...)
}

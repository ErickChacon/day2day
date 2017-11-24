
# loess_pred <- function(y, x, length.out) {
#   time_seq <- seq(min(x), max(x), length.out = length.out + 2)
#   time_seq <- time_seq[1:length.out + 1]
#   mod <- loess(y ~ as.numeric(x))
#   return(predict(mod, time_seq))
# }

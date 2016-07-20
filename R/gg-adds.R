
wrapit <- function(text, width = 20) {
  wtext <- paste(strwrap(text, width = width), collapse=" \n ")
  return(wtext)
}

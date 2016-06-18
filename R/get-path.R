#' @title Get the folder path containing file.
#' 
#' @description
#' \code{get_path} returns the folder path that contains a specified file.
#' 
#' @details
#' When the file name is not specified, \code{get_path} will look for the
#' file "doctoral-research.ecm".
#' 
#' @param file file name to look for.
#' 
#' @return A character indicating the folder path.
#' 
#' @author Erick Albacharro Chacon-Montalvan
get_path <- function(file = "doctoral-research.ecm") {
  full_path <- system(paste('find ~ -name', file, '2>/dev/null'), intern = TRUE)
  path <- gsub(paste0("." ,file, "$"), "", full_path)
  return(path)
}


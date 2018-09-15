#' @title Get the Folder Path Containing File
#'
#' @description
#' \code{get_path} returns the folder path that contains a specified file.
#'
#' @details
#' When the file name is not specified, \code{get_path} will look for the
#' file "doctoral-research.ecm".
#'
#' @param file File name to look for.
#' @param where Folder name where to look for.
#'
#' @return A character indicating the folder path.
#'
#' @author Erick Albacharro Chacon-Montalvan
#'
#' @examples
#'
#' # Create a file to look for
#' data_path <- file.path(tempdir(), "data")
#' dir.create(data_path)
#' write("this is a test", file.path(data_path, "data.RData"))
#'
#' # Find the path of the file
#' get_path(file = "data.RData", where = tempdir())
#'
#' @export
get_path <- function(file = "repos.ecm", where = "~") {
  full_path <- system(paste('find', where, '-name', file, '2>/dev/null'), intern = TRUE)
  file <- gsub("\\.", "\\\\.", file)
  path <- sub(paste0("." ,file, "$"), "", full_path)
  return(path)
}

#' @title Cut Current Working Directory Filepath
#'
#' @description
#' \code{getwd_short} finds the absolute filepath of a folder that contains the
#' current working directory.
#'
#' @details
#' \code{getwd_short} gets the filepath of the current working directory and shortens
#' it given a folder name inside this path.
#'
#' @param foldername Folder name that contains current working directory.
#'
#' @return A character indicating the folder path.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' \dontrun{
#' # Set working directory to tempdir
#' project_path <- file.path(tempdir(), "my-project")
#' dir.create(project_path)
#' code_path <- file.path(project_path, "analysis")
#' dir.create(code_path)
#' setwd(code_path)
#'
#' # Find the path of the file
#' getwd()
#' getwd_short(foldername = "my-project")
#' }
#'
#' @export
getwd_short <- function(foldername) {
  full_path <- getwd()
  # path <- sub(paste0("^(.*", foldername, ".*)/.*"), "\\1", full_path)
  path <- sub(paste0("^(.*", foldername, "[^/]*)/.*"), "\\1", full_path)
  return(path)
}

#' @title Get Path of Current Git Repository
#'
#' @description
#' \code{git_path} finds the path of the current git repository.
#'
#' @details
#' Git is required to use \code{git_path} and it should be only used when working
#' under a git repository.
#'
#' @return A character indicating the git repository path.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
git_path <- function() {
  system('git rev-parse --show-toplevel', intern = TRUE)
}


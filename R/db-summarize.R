#' @title Summarize the databases inside a folder.
#'
#' @description
#' \code{db_summary} write the summary of databases, founded inside a provided
#' folder, into a file.
#'
#' @details
#' When the filename is not specified, \code{db_summary} will create the file
#' "summary-databases.txt".
#'
#' @param path_data The path where to look for .Rdata files.
#' @param filename File name where to write the summary.
#'
#' @return A character indicating the folder path.
#'
#' @author Erick Albacharro Chacon-Montalvan
#'
#' @export
db_summarize <- function(path_data, filename = "summary-databases.txt"){
  databases <- list.files(path_data,".RData$")
  write("------------------------------------------------", filename)
  write("Summary of databases inside the provided folder.", filename, append = T)
  write("------------------------------------------------", filename, append = T)
  for(i in 1:length(databases)){
    write(paste0("\n", i, ") RData file: ", databases[i]), filename, append = T)
    obj_names <- load(paste0(path_data, databases[i]))
    for(j in 1:length(obj_names)){
      var_names <- eval(parse(text = paste("names(", obj_names[j],")")))
      eval(parse(text = paste0("rm(list = '", obj_names[j], "')")))
      if(!is.null(var_names)){
      write(paste0(i, ".", j, ") Object: ", obj_names[j]), filename, append = T)
      write(paste(var_names, collapse = " "), filename, append = T)
      }
    }
  }
}

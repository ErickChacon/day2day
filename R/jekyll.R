#' @title Prepare md post to jekyll.
#'
#' @description
#' \code{post_jekyll} modify the markdown file created with \code{knit} and
#' exports it to the folder of your jekyll webpage. Basically, this function
#' modifies the path of images to match adequately with your jekyll structure
#' end save it on the _posts folder of your jekyll web repositoory.
#'
#' @details
#' details.
#'
#' @param filename Name of the post you want to export to jekyll.
#' @param path_jekyll Path to your _posts folder.
#' @param fig_jekyll String that needs to be added to the figures link.
#'
#' @return Generates a markdown file on your _posts folder (you must specify
#' this path adequately).
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @export
post_jekyll <- function (filename,
  path_jekyll = "/home/chaconmo/Documents/Repositories/erickchacon/_posts/",
  fig_jekyll = "{{site.baseurl}}/assets/images/") {

  content <- readLines(filename)
  new_content <- gsub("(!\\[.+\\]\\()(.+\\))",
                      paste0("\\1", fig_jekyll, "\\2"),
                      content)
  filename <- file.path(path_jekyll, filename)
  write(new_content, file = filename)
  cat(paste(filename, "successfully exported to jekyll.\n"))
}



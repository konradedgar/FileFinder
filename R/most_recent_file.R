#' @title Most Recent File
#'
#' @param path Valid folder path
#' @param n Number of files to return defaults to 1; as in \code{\link[utils]{head}}.
#' @param ... As in \code{\link[base]{list.files}}.
#'
#' @return A vector with file paths of length \code{n}.
#'
#' @export
#'
#' @importFrom checkmate assertDirectoryExists
#' @importFrom utils head
#'
#' @examples
#' most_recent_file(tempdir())
most_recent_file <- function(path,
                             n = 1,
                             ...) {
    # Check whether path is valid directory
    assertDirectoryExists(x = path, access = "r")
    # Create files info data frame
    dta_files <-
        file.info(list.files(path = path, ...))
    # Provide basename
    dta_files$basename <- basename(rownames(dta_files))
    # Sort data frame by date
    dta_files[with(data = dta_files,
                   expr = {
                       order(mtime, decreasing = TRUE)
                   }),] -> dta_files

    # Get selected number of rows
    dta_match <- head(dta_files, n = 1)

    # Get file names
    res <- rownames(dta_match)

    # Return result
    return(res)
}

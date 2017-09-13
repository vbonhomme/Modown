#' Write to a single coo file a single shape from Coo Momocs' objects
#'
#' @param x any \code{Coo} object from \code{Momocs}
#' @param id \code{numeric}, which shape to export
#' @param folder \code{character} (optionnal) to write
#' within a folder, existing or not
#' @param file \code{character} the name of the file to be created
#' @family write
#' @export
write_Coo1 <- function(x, id, folder="", file){
  # if folder is provided but doesn't exist, create it
  if (!missing(folder) && !dir.exists(folder))
    dir.create(folder, recursive=TRUE)
  # if file is missing, inherits x[id] name
  if (missing(file))
    file <- paste0(names(x)[id], ".coo")
  # if writing to root
  if (folder != "")
    file <- paste0(folder, "/", file)
  # export and write .coo file
  x %>%
    export_Coo1(id) %>%
    writeLines(con=file)
}

#' Write to a single or separate coo file a whole Coo Momocs' object
#'

#' @param x any \code{Coo} object from \code{Momocs}
#' @param folder \code{character} (optionnal) to write
#' within a folder, existing or not
#' @param file \code{character} the name of the file(s) to be created
#' @param separate \code{logical} whether to create separate coo files
#' @param force \code{logical} whether to replace files without notice
#' @param zip \code{logical} whether to zip the created file
#' @family write
#' @export
# write a whole Coo to a single or separate coo file(s)
write_Coo <- function(x, folder="", file,
                      separate=FALSE, force=FALSE, zip=FALSE){
  if (separate){
    shut_up <- lapply(seq_along(x),
                      function(.) write_Coo1(x=x, id=., folder=folder))
  } else {
    # if folder is provided but doesn't exist, create it
    # after checking it exists
    if (!missing(folder)){
      if (dir.exists(folder)){
        if (force){
          dir.create(folder, recursive=TRUE)
        } else {
          ans <- readline(paste(folder, "already exists; replace it anyway (y/n)?"))
          if (ans=="y")
            dir.create(folder, recursive=TRUE)
          else
            stop("Pick another name or remove existing folder")
        }
      } else {
        dir.create(folder, recursive=TRUE)
      }
    }
    # if file is missing, inherits x name
    if (missing(file))
      file <- paste0(substitute(x), ".coo")
    # if writing to root
    if (folder != "")
      file <- paste0(folder, "/", file)
    # checks if file already exists, with checks
    if (file.exists(file)){
      if (!force){
        ans <- readline(paste(file, "already exists; replace it anyway (y/n)?"))
        if (ans!="y")
          stop("Pick another name or remove existing file")
      }
    }
    # export and write .coo file
    x %>%
      export_Coo() %>%
      writeLines(con=file)
    if (zip){
      shut_up <- suppressMessages(zip(gsub("coo", "zip", file), file, flags = "-r9Xq"))
      shut_up <- file.remove(file)
    }
  }
}

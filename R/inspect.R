#' Inspect coo file for possible problems
#'
#' Not guaranteed.
#' @param x simple or multiple coo(s), as path or character vector
#' @family inspect
#' @export
inspect1 <- function(x){
  # prints a custom message
  .msg <- function(msg, id, what="coo"){
    message("! ", msg, ";\n\tcheck ", what, " #", paste(id, collapse=", "))
  }

  # read raw lines
  if (.is.path(x))
    x <- x %>% readLines(warn = FALSE)

  # check that all names are present
  xl <- .cuts_into_list(x, "^~")
  tildes <- sapply(xl, `[`, 1)
  ### names
  # all are filled

  coo_N <- ifelse(length(tildes>=0), length(tildes), 1) # nb of coo
  coo_names <- gsub("~| ", "", tildes)
  if (any(nchar(coo_names)==0)){
    .msg("all names should be filled",
         which(nchar(coo_names)==0))
  }

  # no duplicate
  coo_names_table <- table(coo_names)>1
  if (any(coo_names_table)) {
    .msg("names should not be duplicated",
         which(coo_names %in% names(which(coo_names_table))))
  }
  ### HANDLE NO COV CASE
  ### covariates
  covl <- lapply(xl,
                 function(.) grep("[[:alpha:]] [[:alnum:]]", ., value=TRUE) %>%
                   .str2df %>% colnames)
  # number of covariates per coo
  covl_length <- sapply(covl, length)
  # missing cov
  if (any(covl_length==0)){
    .msg("cov must be present everywhere",
         which(covl_length==0))
  }
  # homogeneous number of cov components
  if (length(unique(covl_length))>1){
    .msg("cov should have the same length",
         names(which(covl_length != as.numeric(names(table(covl_length)[1])))))
  }
  # homogeneous names for cov components
  cov_table <- sort(table(unlist(covl)), decreasing = TRUE)
  if (length(unique(cov_table))>1){
    .msg("cov names should be present everywhere (and check for typos)",
         names(which(cov_table != cov_table[1])),
         what="cov names")
  }

  ### coordinates
  # leading spaces
  leading_spaces <- grep("^ ", x)
  if (length(leading_spaces)>0){
    .msg("leading spaces should be avoided", leading_spaces, "lines")
  }
  # trimming spaces
  # leading spaces
  trailing_spaces <- grep(" $", x)
  if (length(trailing_spaces)>0){
    .msg("trailing spaces should be avoided", trailing_spaces, "lines")
  }
  # multiple spaces
  multiple_spaces <- grep("^.+ {2,}.+$", x)
  if (length(trailing_spaces)>0){
    .msg("multiple spaces should be avoided", multiple_spaces, "lines")
  }

}

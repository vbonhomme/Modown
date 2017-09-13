
# removes on a string all non valid lines
.shave <- function(x){
  x %>%
    # 2+ spaces are converted to a single space
    gsub(" {2, }", " ", .) %>%
    # remove leading and trailing spaces
    gsub("^ | $", "", .) %>%
    # every lines not containing (one or more) (word or number)
    grep("[[:alnum:]]+", ., value=TRUE)
}

# turns a list with <cov_name value> into a data.frame
.str2df <- function(x){
  x <- strsplit(x, " ")
  df <- lapply(x, function(.) paste(.[-1], collapse=" ")) %>% as.data.frame
  colnames(df) <- sapply(x, `[`, 1)
  df
}

# turns a list with coordinates as a vector of characters into a matrix
.str2mtx <- function(x){
  x %>%
    strsplit(" ") %>%
    lapply(as.numeric) %>%
    do.call("rbind", .)
}

# detects valid paths
.is.path <- function(x){
  file.exists(x) && length(x)==1
}

# turns a vector with NAs and non-NAs
# replace NAs with the last non-NA
# borrowed from user2100721@stackoverflow
.replace_na_with_last<-function(x){
  y <- !is.na(x)
  x[which(y)[c(1, 1:sum(y))][cumsum(y)+1]]
}

# turns a vector of characters,
# with some elements beginning with a pattern,
# split it into a list
.cuts_into_list <- function(x, pattern){
  ids <- grep(pattern, x)
  f <- rep(NA, length(x))
  f[ids] <- seq_len(length(ids))
  split(x, .replace_na_with_last(f))
}

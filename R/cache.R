
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Nest list elements of the same name under a single list entry
#'
#' @param ll list
#'
#' @return nested list
#'
#' @importFrom stats setNames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nest_list <- function(ll) {
  nn <- names(ll)
  if (is.null(nn) || any(nn == '')) {
    stop("All list elements must be named")
  }

  unique_names <- unique(nn)

  res <- lapply(unique_names, function(name) {
    ii <- which(nn == name)
    if (length(ii) > 1) {
      unname(ll[ii])
    } else {
      ll[[ii]]
    }
  })

  setNames(res, unique_names)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is the internal cache for storing data during the stat debug call
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cache <- new.env()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the cached data
#'
#' @param nested Nest elements from the same call under a single name. Default: TRUE.
#' Set to FALSE for a flatter list structure.
#'
#' @return list of values from the last run
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_data_cache <- function(nested = TRUE) {
  if (nested) {
    nest_list(cache$data)
  } else {
    cache$data
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Reset the data cache
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset_data_cache <- function() {
  cache$data <- list()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set value in the data cache
#'
#' @param value value
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_data_to_cache <- function(value) {
  if (!'data' %in% names(cache)) cache$data <- list()
  if (!is.list(value)) {
    print(value)
    stop("value must be list, but is: ", class(value))
  }
  cache$data <- c(cache$data, value)
}


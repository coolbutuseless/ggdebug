

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print value compactly. Unnest lists, and only show head of data.frames
#'
#' @param x value
#' @param head_n head n parameter
#'
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vprint <- function(x, head_n) {
  if (is.null(x)) {
    # do nothing
  } else if (is.data.frame(x)) {
    print(head(x, head_n))
  } else if (is.list(x)) {
    print(unlist(x))
  } else if (is.atomic(x)) {
    print(x)
  } else {
    # Silently ignore other classes. e.g. ggproto objects
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add debugging to a particular function within a Stat
#'
#' @param DebugStat the actual Stat
#' @param stat_original_name full name of original stat e.g. 'StatIdentity'. Used
#' for debugging output
#' @param method the method name within this stat to debug
#' @param reset_cache If TRUE then the cache of captured data is
#' reset when this Stat$method() is invoked. Default: FALSE.
#' @param verbose Print data information as the methods execute. Default: FALSE
#' @param head_n Argument to \code{head()} when printing data.frames in verbose mode
#'
#' @importFrom utils head
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enable_caching_in_stat_function <- function(DebugStat,
                                            stat_original_name,
                                            method,
                                            reset_cache = FALSE,
                                            verbose = FALSE,
                                            head_n) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prevent the user from trying to wrap a Stat$function that doesn't exist
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(DebugStat[[method]])) {
    stop("No such method: ", stat_original_name, "$", method, "()")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Retrieve the original defintion of the Stat$method
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inner_function <- environment(DebugStat[[method]])$f
  inner_formals  <- formals(inner_function)
  inner_env      <- environment(inner_function)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Character arg names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arg_names <- names(inner_formals)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rewrite the function body for this Stat and function name
  #  - print all the arguments
  #  - call the original body
  #  - print the original return value
  #  - return the original return value
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_body <- bquote({

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define the helper function here to avoid namespace issues
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # vprint <- .(vprint)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Verbose printing of the StatName$method()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (.(verbose)) {
      caller <- paste0(.(stat_original_name), '$', .(method), '()')
      message(sprintf("\n=================== %-30s ===================", caller))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Verbose printing of the input arguments. This is a bit dodgy as it
    # assumes the called argument order matches the order of the formals
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    arg_names <- .(arg_names)
    args_list <- list()

    if ('...' %in% arg_names) {
      args_list$dots <- list(...)
      arg_names <- setdiff(arg_names, '...')
    }

    arg_names <- setdiff(arg_names, 'self')

    for (arg_name in arg_names) {
      arg_value <- get(arg_name, inherits = FALSE)
      args_list <- c(args_list, list(arg_value))
      if (.(verbose)) {
        message(sprintf("------------------- %-30s -------------------", paste("Input:", arg_name)))
        vprint(arg_value, .(head_n))
      }
    }

    args_list <- setNames(args_list, arg_names)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate original result
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x <- .({body(environment(DebugStat[[method]])$f)})

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Verbose printing of the return value
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (.(verbose)) {
      message(sprintf("------------------- %-30s -------------------", "Return"))
      vprint(x, .(head_n))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Store values in the cache.
    # If this is the first method being augmented with cacheing, then
    # clear out the cache before we start.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (.(reset_cache)) {
      reset_data_cache()
    }

    args_and_return <- list(
      list(
        args   = args_list,
        return = x
      )
    )
    args_and_return <- setNames(args_and_return, .(method))
    add_data_to_cache(args_and_return)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return result from the original stat call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the new verbose function and insert it into the Stat
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  verbose_function <- eval(
    call("function", inner_formals, new_body),
    envir = inner_env, enclos = inner_env
  )

  DebugStat[[method]] <- verbose_function


  DebugStat
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a debug version of the given Stat
#'
#' Create a debug version of the given Stat by creating a new Stat which directly
#' inherits from the original Stat.  Then for each named method, rewrite the
#' method body to capture arguments and return values for each call.
#'
#' @param OriginalStat Ggproto Stat object to debug. e.g. StatIdentity
#' @param methods Methods within ggproto object to debug. Default: all methods
#' @param verbose Print arguments and return values during ggplot build and render process. Default: FALSE
#' @param head_n Argument to \code{head()} when printing data.frames. Default: 6
#'
#' @return A subclass of the given Stat with each method modified to capture
#' the argument list and return value.
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stat_with_caching <- function(OriginalStat,
                                     methods = c(
                                       'parameters'   , 'aesthetics',
                                       'setup_params' , 'setup_data',
                                       'compute_layer', 'compute_panel', 'compute_group',
                                       'finish_layer'),
                                     verbose = FALSE,
                                     head_n = 6) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # It's important to order the methods that we want to debug.
  # The order should be the order in which they are called during the plot rendering.
  # This is done because we want to reset the cache on the first possible method call.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ordered_methods <- c('parameters'   , 'aesthetics',
                       'setup_params' , 'setup_data',
                       'compute_layer', 'compute_panel', 'compute_group',
                       'finish_layer')
  methods <- ordered_methods[ordered_methods %in% methods]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a subclass of the given OriginalStat and get the stat name
  # from its class
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stat_original_name <- class(OriginalStat)[1]
  DebugStat          <- ggproto("DebugStat", OriginalStat)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each method specified by the user, overwrite its current body with
  # one which does data capture and printing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (method in methods) {
    DebugStat <- enable_caching_in_stat_function(
      DebugStat,
      stat_original_name,
      method,
      reset_cache = identical(method, methods[1]),
      verbose     = verbose,
      head_n      = head_n
    )
  }


  DebugStat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (interactive()) {

  suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(ggdebug)
  })

  StatIdentityDebug <- create_stat_with_caching(StatIdentity, verbose = TRUE)

  ggplot(mtcars) +
    geom_point(aes(mpg, wt, colour = as.factor(cyl)), stat = StatIdentityDebug) +
    theme_bw() +
    facet_wrap(~cyl)

  cdata <-  ggdebug::get_data_cache()
  cdata$parameters
}



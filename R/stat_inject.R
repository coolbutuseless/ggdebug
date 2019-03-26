

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Inject an argument or override the return value of a method within a Stat
#'
#' @param OriginalStat the actual Stat
#' @param method the method name to inject data into
#' @param args named list of values to insert at the beginning of the method.
#'             Usually to override arguments. Default: NULL (i.e. no injection)
#' @param return_value the value to return from the method (overriding the actual value).
#'             Default: NULL (i.e. no injection)
#'
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inject_data_into_stat_function <- function(OriginalStat, method, args=NULL, return_value=NULL) {

  stat_original_name <- class(OriginalStat)[1]
  InjectedStat       <- ggproto("InjectedStat", OriginalStat)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prevent the user from trying to wrap a Stat$method() that doesn't exist
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(InjectedStat[[method]])) {
    stop("No such method: ", stat_original_name, "$", method, "()")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Retrieve the defintion of the Stat$method
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inner_function <- environment(InjectedStat[[method]])$f
  inner_formals  <- formals(inner_function)
  inner_env      <- environment(inner_function)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Character arg names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arg_names <- names(inner_formals)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rewrite the body for the method in this Stat
  #  - print all the arguments
  #  - call the original body
  #  - print the original return value
  #  - return the original return value
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_body <- bquote({

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Stomp hard all over existing vars
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    args <- .(args)
    if (!is.null(args)) {
      for (i in seq_along(args)) {
        assign(names(args)[i], args[[i]])
      }
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate original result
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    x <- .({body(environment(InjectedStat[[method]])$f)})

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Either return the injected return value (if it is not NULL) or
    # return result from the original call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return_value <- .(return_value)
    if (is.null(return_value)) {
      x
    } else {
      .(return_value)
    }
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the new verbose function and insert it into the Stat
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  verbose_function <- eval(
    call("function", inner_formals, new_body),
    envir = inner_env, enclos = inner_env
  )

  InjectedStat[[method]] <- verbose_function


  InjectedStat
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

  StatIdentityDebug <- create_stat_with_caching(StatIdentity)

  p <- ggplot(plot_df) +
    geom_point(aes(mpg, wt, colour = as.factor(cyl)), stat = StatIdentityDebug) +
    theme_bw() +
    facet_wrap(~cyl)


  pp <- ggplot_build(p)

  cdata <- get_data_cache()

  compute_layer_return <- cdata$compute_layer$return %>%
    mutate(
      y = ifelse(group == 1, y + 10, y)
    )


  StatIdentityInject <- inject_data_into_stat_function(
    StatIdentity,
    method = 'compute_layer',
    return_value = compute_layer_return
  )

  ggplot(plot_df) +
    geom_point(aes(mpg, wt, colour = as.factor(cyl)), stat = StatIdentityInject) +
    theme_bw() +
    facet_wrap(~cyl)

}


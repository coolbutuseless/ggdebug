

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the name of the 'geom' which is using this stat.
#'
#' This needs to be called from within a Stat method.  It interrogates the
#' frame stack to figure out the layer which called the Stat, and then
#' gets the Geom from that layer.
#'
#' Written with the help of Brodie Gaslam
#'
#' @return name of geom, otherwise NA_character_
#' @export
#'
#' @examples
#' \dontrun{
#'   setup_params = function(data, params) {
#'    message("StatOathBreaker being used with: ", get_geom())
#'    params
#'  }
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_geom <- function() {

  sys_frames <- sys.frames()

  geom <- NA_character_
  for (f in rev(sys_frames)) {
    if (!is.null(f$self)) {
      geom <- head(class(f$self$geom), 1)
      break
    }
  }

  geom
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing zone. Create a clone of 'identity' stat with some debugging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (interactive()) {

  stat_oath_breaker <- function(mapping = NULL, data = NULL,
                             geom     = "point",
                             position = "identity",
                             ...,
                             show.legend = NA,
                             inherit.aes = TRUE) {
    layer(
      data        = data,
      mapping     = mapping,
      stat        = StatOathBreaker,
      geom        = geom,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = FALSE,
        ...
      )
    )
  }

  StatOathBreaker <- ggproto(
    "StatOathBreaker", Stat,

    setup_params = function(data, params) {
      message("StatOathBreaker being used with: ", get_geom())
      params
    },

    compute_layer = function(data, scales, params) {
      data
    }
  )


  ggplot(mtcars) +
    geom_line(aes(mpg, wt), stat = 'oath_breaker')
}





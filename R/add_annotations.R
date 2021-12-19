#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fig
add_annotations <- function(fig, cols, ymax = NULL, xmax = NULL) {

  stopifnot(length(cols)==3)

  fig_ymax <- if(is.null(ymax)) layer_scales(fig)$y$range$range[2] else ymax
  fig_xmax <- if(is.null(xmax)) layer_scales(fig)$x$range$range[2] else xmax

  fig +
    annotate(geom = 'rect',
             xmin = 0,
             ymin = 0,
             xmax = 2.54,
             ymax = fig_ymax,
             fill = cols[1],
             alpha = 0.1) +
    annotate(geom = 'rect',
             xmin = 2.54,
             ymin = 0,
             xmax = 4.48,
             ymax = fig_ymax,
             fill = cols[2],
             alpha = 0.1) +
    annotate(geom = 'rect',
             xmin = 4.48,
             ymin = 0,
             xmax = fig_xmax,
             ymax = fig_ymax,
             fill = cols[3],
             alpha = 0.1) +
    annotate(geom = 'text',
             x = 1.27,
             y = fig_ymax * 0.90,
             size = 5,
             label = 'Trial phase') +
    annotate(geom = 'text',
             x = 3.51,
             y = fig_ymax * 0.90,
             size = 5,
             label = 'Trial And\nCohort\nPhase') +
    annotate(geom = 'text',
             x = 5.85,
             y = fig_ymax * 0.90,
             size = 5,
             label = 'Cohort Phase')

}

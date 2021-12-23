#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fig
add_annotations <- function(fig,
                            cols,
                            ymax = NULL,
                            ymin = NULL,
                            xmax = NULL,
                            xmin = NULL,
                            ymult = 0.90,
                            x_cohort_phase = 6,
                            textsize = 4.5) {

  stopifnot(length(cols)==3)

  fig_ymax <- if(is.null(ymax)) layer_scales(fig)$y$range$range[2] else ymax
  fig_xmax <- if(is.null(xmax)) layer_scales(fig)$x$range$range[2] else xmax

  fig_ymin <- if(is.null(ymin)) 0 else ymin
  fig_xmin <- if(is.null(xmin)) 0 else xmin

  fig +
    annotate(geom = 'rect',
             xmin = fig_xmin,
             ymin = fig_ymin,
             xmax = 2.54,
             ymax = fig_ymax,
             fill = cols[1],
             alpha = 0.1) +
    annotate(geom = 'rect',
             xmin = 2.54,
             ymin = fig_ymin,
             xmax = 4.48,
             ymax = fig_ymax,
             fill = cols[2],
             alpha = 0.1) +
    annotate(geom = 'rect',
             xmin = 4.48,
             ymin = fig_ymin,
             xmax = fig_xmax,
             ymax = fig_ymax,
             fill = cols[3],
             alpha = 0.1) +
    annotate(geom = 'text',
             x = 1.27,
             y = fig_ymax * ymult,
             size = textsize,
             label = 'Trial phase') +
    annotate(geom = 'text',
             x = 3.51,
             y = fig_ymax * ymult,
             size = textsize,
             label = 'Trial And\nCohort\nPhase') +
    annotate(geom = 'text',
             x = x_cohort_phase,
             y = fig_ymax * ymult,
             size = textsize,
             label = 'Cohort Phase')

}

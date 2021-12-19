

theme_fig <- function(x, ...){
  theme_bw() +
    theme(text = element_text(family = 'serif', size = 12),
          axis.text = element_text(family = 'serif', color = 'black', size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          ...)
}

theme_tbl <- function(x, ...) {
  theme_void() +
    theme(text = element_text(family = 'serif', size = 12),
          plot.caption = element_text(hjust = 0, size = 2),
          plot.title.position = "plot",
          plot.caption.position =  "plot",
          ...)
}

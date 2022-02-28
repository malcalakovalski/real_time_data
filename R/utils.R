brookings_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(brookings_colors)
  }

  brookings_colors[cols]
}

mutate_where <- function (.data, .where, ...)
{
  rows_lgl <- as.logical(rlang::eval_tidy(enquo(.where), .data,
                                          parent.frame()))
  .data[rows_lgl, ] <- dplyr::mutate(.data[rows_lgl, ], ...)
  .data
}

rename_dates <- function(data){
  if("dates" %in% colnames(data) )
  {
    data %>%
      mutate(dates = as_date(dates)) %>%
      dplyr::rename(date = dates)
  }

}

#Function to shift x-axis to 0
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))),
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax,
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())

}


#' Transforms correspondence table into a format suitable for plotly sankey plot
#'
#'
#'
#' @param x a correspondence table in long format (data.table)
#' @param source location (numeric) or name (character) of column of source classification
#' @param target location (numeric) or name (character) of column of target classification
#' @param value location (numeric) or name (character) of value column
#'
#' @return
#' @export
#'
#' @examples
make_CT_sankeystyle <- function(x, source = 1, target = 2, value = 3) {
  x <- copy(x)
  setcolorder(x, c(source, target, value))
  setnames(x, c('source', 'target', 'value'))

  labels <- c(unique(x$source), unique(x$target))
  id <- 0:(length(labels)-1)

  data <- list()

  data$nodes <- data.table(
    label = labels,
    id = id
  )

  x <- merge(x, data$nodes, by.x = 'source', by.y = 'label')
  x <- merge(x, data$nodes, by.x = 'target', by.y = 'label')
  x[, target := NULL]
  x[, source := NULL]
  setnames(x, c('id.x', 'id.y'), c('source', 'target'))
  setcolorder(x, c('source', 'target', 'value'))

  data$edges <- x

  return(data)
}




#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_CT_sankey <- function(x, ...) {
  # plot
  plot_ly(
    type = "sankey",
    orientation = "h",

    node = list(
      label = x$nodes$label,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    link = list(
      source = x$edges$source,
      target = x$edges$target,
      value =  x$edges$value
    )

  )


}

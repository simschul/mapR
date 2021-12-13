#' Title
#'
#' @param x
#' @param source
#' @param target
#' @param value
#' @param return
#'
#' @return
#' @export
#'
#' @examples

CT_to_wide <- function(x,
                       source = 1,
                       target = 2,
                       value = 3,
                       return = 'data.table') {
  if (is.numeric(source)) source <- colnames(x)[source]
  if (is.numeric(target)) target <- colnames(x)[target]
  if (is.numeric(value)) value <- colnames(x)[value]

  x_wide <- dcast(x, get(source) ~ get(target),
                  value.var = value,
                  fill = 0)

  setnames(x_wide, 'source', source)

  if (return == 'matrix') x_wide <- as.matrix(x_wide, rownames = 1)

  return(x_wide)
}


#' Title
#'
#' @param x
#' @param source_name
#' @param target_name
#' @param sort_by
#'
#' @return
#' @export
#'
#' @examples

CT_to_long <- function(x, source_name = 'source',
                       target_name = 'target',
                       sort_by = 'source') {
  if (is.matrix(x)) x <- as.data.table(x, keep.rownames = source_name)
  source_name <- colnames(x)[1]
  x <- melt(x, id.var = source_name,
            value.name = 'share',
            variable.factor = FALSE,
            variable.name = target_name)
  x <- x[share > 0]
  setorderv(x, cols = get(paste0(sort_by, '_name')))
  return(x)
}



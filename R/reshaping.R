#' Reshapes a correspondence table from long to wide format.
#' Wrapper around `data.table::dcast`
#'
#' @param x a correspondence table in long format. Must have at least 3 columns: source and target classification, and mapping infromation (numeric/boolean)
#' @param source location (numeric) or name (character) of column of source classification
#' @param target location (numeric) or name (character) of column of target classification
#' @param value location (numeric) or name (character) of value column
#' @param return
#'
#' @return
#' @import data.table
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

  x_wide <- dcast(x,
                  paste0(source, ' ~ ', target), # formular as character string, see: https://stackoverflow.com/questions/31295376/in-r-dcast-in-function-pass-column-names-again
                  value.var = value,
                  fill = 0)
  setkey(x_wide, NULL)
  if (return == 'matrix') x_wide <- as.matrix(x_wide, rownames = 1)

  return(x_wide)
}


#' Reshapes correspondence table from wide to long format.
#' Wrapper around `data.table::melt`
#'
#' @param x a CT in wide format. Either matrix with rownames (source classification) and column names (target classfication), or data.table with the source classifaction as the first column
#' @param source_name name of the source classication. Defaults to 'source' (if x is matrix), or the name of the first column (if x is data.table)
#' @param target_name name of the target classifaction. Defaults to 'target'
#' @param sort_by either 'source' (default) or 'target'
#'
#' @return a CT in long format (=data.table with 3 columns: source and target classification, and mapping infromation (numeric/boolean))
#' @import data.table
#' @export
#'
#' @examples

CT_to_long <- function(x, source_name = 'source',
                       target_name = 'target',
                       sort_by = 'source') {
  if (is.matrix(x)) x <- data.table::as.data.table(x, keep.rownames = source_name)
  source_name <- colnames(x)[1]
  x <- data.table::melt(x, id.var = source_name,
            value.name = 'share',
            variable.factor = FALSE,
            variable.name = target_name)
  x <- x[share > 0]
  data.table::setorderv(x, cols = get(paste0(sort_by, '_name')))
  return(x)
}



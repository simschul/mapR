
#' Create correspondence table object
#'
#' @param x either in long format (data.table/frame with three cols: source, target, value), or in wide format (matrix) with rownames (source) and colnames (target)
#' @param source numeric: column of x where to find the source classification (only if x is data.table)
#' @param target numeric: column of x where to find the target classification (only if x is data.table)
#' @param value numeric: column of x where to find the value classification (only if x is data.table)
#' @param source_name character: name of source classification (only if x is matrix)
#' @param target_name character: name of target classification (only if x is matrix)
#'
#' @return an CT object (inherits from data.table and data.frame)
#' @export
#'
#' @examples
CT <- function(x, source = 1, target = 2, value = 3,
               source_name = 'source', target_name = 'target') {
  if (is.data.frame(x)) {
    if (!is.data.table(x)) x <- as.data.table(x)
    else x <- copy(x)

    if (ncol(x) == 2 | is.na(value) | is.null(value)) {
      x[, value := NA]
      value <- 3
    }
  }

  if (is.matrix(x)) {
    stopifnot({
      !is.null(colnames(x))
      !is.null(rownames(x))
    })

    x <- CT_to_long(x,
                    source_name = source_name,
                    target_name = target_name)

  }

  obj <- structure(
    class = c('CT', 'data.table', 'data.frame'),
    x,
    source.name = colnames(x)[source],
    target.name = colnames(x)[target],
    value.name = ifelse(is.null(value), NA, colnames(x)[value])
  )
  setcolorder(obj, c(source, target, value))
  setnames(obj, c('source', 'target', 'value'))
  return(obj)
}


#' Print a CT
#' see: `?CT`
#' @param x an object of type 'CT'
#'
#' @return
#' @export
#'
#' @examples
print.CT <- function(x) {
  cat(
    'Correspondence table between source classification "',
    attr(x, 'source.name'),
    '" and target classification "',
    attr(x, 'target.name'),
    '"\n',
    sep = ''
  )
  print(as.data.table(x), class = TRUE)
}

#' Title
#'
#' @param x
#' @param tol
#' @param return_incoherent_sectors
#'
#' @return
#' @export
#'
#' @examples

is_coherent <- function(x, tol, return_incoherent_sectors) {
  UseMethod('is_coherent')
}

#' Title
#'
#' @param x
#' @param tol
#' @param return_incoherent_sectors
#'
#' @return
#' @export
#'
#' @examples
is_coherent.CT <- function(x, tol = 1E-6, return_incoherent_sectors = FALSE) {
  results <- x[, list(sum = sum(value)), by = source]
  class(results) <- setdiff(class(results), 'CT')
  if (my.utils::zero_range(results$sum)) return(TRUE)
  else {
    warning('Incoherence found for the following sectors:\n',
            paste(results[sum < (1 - tol) | sum > (1 + tol)]$source, collapse = ', '))
    if (isTRUE(return_incoherent_sectors)) return(results[sum < (1 - tol) | sum > (1 + tol)])
    else return(FALSE)
  }
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
write_CT <- function(x, file, ...) {
  UseMethod('write_CT')
}

#' Title
#'
#' @param x
#' @param file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_CT.CT <- function(x, file = '', ...) {
  data.table::setnames(x, c(attr(x, 'source.name'),
                attr(x, 'target.name'),
                attr(x, 'value.name')))
  rio::export(x, file = file, ... = ...)
}


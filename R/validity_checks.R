

#
# is_CT_wide <- function(x) {
#
#   # type matrix
#   if (is.matrix(x)) {
#     isCT <- colnames(x) != NULL & rownames(x) != NULL & (is.numeric(x) | is.logical(x))
#   }
#
#   # type data.table
#   if (is.data.table(x)) {
#     isCT <- is.character(x[[1]])
#   }
#
# }
#
# is_CT_long <- function(x) {
#
# }
#
# is_coherent_CT <- function(x) {
#
# }

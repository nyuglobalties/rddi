ddi_name <- function(x, ...) {
  UseMethod("ddi_name")
}

ddi_name.ddi_node <- function(x, ...) {
  names(x)
}

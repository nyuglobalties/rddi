`%||%` <- function(x, y) if (is.null(x)) y else x
`%if_empty%` <- function(x, y) if (length(x) == 0) y else x
`%if_empty_string%` <- function(x, y) if (identical(x, "")) y else x

rddi_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::abort(.subclass = "rddi_error", message = msg)
}

dots_partition <- function(...) {
  dots <- dots_list(...)

  if (is.null(names(dots))) {
    is_named <- rep(FALSE, length(dots))
  } else {
    is_named <- names(dots) != ""
  }

  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}

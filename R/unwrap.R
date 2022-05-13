#' Allow text and nodes in ddi_node content
#'
#' Normally, ddi_node objects can only either have text
#' content or ddi_node children. This adheres to the XML
#' schema. However, some elements in DDI have a less strict
#' content schema, more like HTML, which allow text and XML
#' tags both as content. This utility is for DDI node authors
#' who would like to use this feature. Rather than wrap the
#' content in XML structure, this function marks its input
#' content as "unwrapped": when passed to `as_xml()`, the content
#' will be directly converted to text, not wrapped in XML.
#'
#' @param content A content list, most likely from `dots_to_xml_components()`
#' @return `content` contained in an "unwrapping" context
#'
#' @noRd
#' @examples
#' if (FALSE) {
#'   # Building a custom "universe" tag
#'   # which accepts <txt> and <concept>
#'   # alongside text
#'   ddi_universe <- function(...) {
#'     components <- dots_to_xml_components(...)
#'
#'     build_branch_node(
#'       "universe",
#'       content = unwrap_content(components$content),
#'       attribs = components$attribs
#'     )
#'   }
#' }
unwrap_content <- function(content) {
  list(do.call(unwrap, content))
}

unwrap <- function(...) {
  components <- dots_to_xml_components(...)
  # Do not include attributes as unwrap converts
  # `content` into text
  content <- components$content

  if (length(content) < 1) {
    rddi_err("Cannot create unwrapped ddi node of nothing")
  }

  structure(
    list(
      content = content
    ),
    class = c("ddi_unwrapped", "ddi_node")
  )
}

is_unwrapped <- function(x) {
  inherits(x, "ddi_unwrapped")
}

#' @export
print.ddi_unwrapped <- function(x, ...) {
  cat_line("<Unwrapped DDI nodes>")

  for (cont in x$content) {
    print(cont)
  }

  invisible(x)
}
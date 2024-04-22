#' Pipe operator
#'
#' Import of [magrittr::%>%()]. See magrittr package documentation for detailed description.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @keywords internal
#' @returns The type of return from this function depends on the left hand side and right hand side arguments supplied to it. In principle, any type of returned object is feasible.
#' @seealso [magrittr::%>%()]
NULL

#' Deprecated function
#'
#' Import of [lifecycle::deprecated()]. See lifecycle package documentation for detailed description.
#'
#' @importFrom lifecycle deprecated
#' @name deprecated
#' @rdname deprecated
#' @export
#' @keywords internal
#' @returns Does not return a value as the function is used to signal that a function argument has been deprecated.
#' @seealso [lifecycle::deprecated()]
NULL

#' Non standard evaluation assignment function
#'
#' Import of := function, implementing [rlang::dyn-dots()]. See rlang package documentation for detailed description.
#'
#' @importFrom rlang :=
#' @name :=
#' @rdname nseequals
#' @export
#' @keywords internal
#' @returns Does not return a value. Used to implement non standard evaluation within a dynamic dots context.
#' @seealso [rlang::dyn-dots()]
NULL

#' Dot Data function
#'
#' Import of [rlang::.data()]. See rlang package documentation for detailed description.
#'
#' @importFrom rlang .data
#' @name .data
#' @rdname dotdata
#' @export
#' @keywords internal
#' @returns Does not return a value. A pronoun for use in data-masked functions.
#' @seealso [rlang::.data()]
NULL

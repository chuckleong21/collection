box::use(
  assertthat[`on_failure<-`, assert_that]
)

is_scalar_numeric <- function(x) length(x) == 1 && is.numeric(x)

#' @export
is_dialog_type <- function(x) {
  is_scalar_numeric <- function(x) length(x) == 1 && is.numeric(x)
  is_scalar_numeric(x) && x %in% 0:2
}
on_failure(is_dialog_type) <- function(call, env) {
  sprintf("DialogType is either 0, 1, 2, not %s", deparse(call$x))
}

#' @export
is_valid_widths <- function(x) is.numeric(x) & (length(x) >=1 & length(x) <=2)
on_failure(is_valid_widths) <- function(call, env) {
  sprintf("Maximum length of 'widths' is %g instead of 2", length(call$x))
}
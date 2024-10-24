box::use(
  purrr[map]
)

# branch method
#' @export
branch <- function(diff) {
  UseMethod("branch")
}

branch.collection_diff <- function(x) {
  map(x$diff, ~.x$branch)
}
local(envir = e, {
  branch.collection_diff
  .S3method("branch", "collection_diff")
})
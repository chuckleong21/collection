box::use(
  assertthat[assert_that, `on_failure<-`],
  purrr[map, map2, map_vec], 
  dplyr[rows_upsert, filter, select, arrange, mutate, row_number]
)

box::use(
  app/logic/branch[branch]
)

# merge method ------------------------------------------------------------


#' @export
merge <- function(x, y, ...) {
  UseMethod("merge")
}
merge.collection_diff <- function(diff, database = NULL) {
  on.exit(message(sprintf("updated %g records", updates)), add = TRUE)
  is_database <- function(x) {
    assert_that(inherits(x, "collection"))
    x$source == "database"
  }
  on_failure(is_database) <- function(call, env) {
    paste0(deparse(call$x), " is not a database collection")
  }
  database <- database %||% collection("database")
  assert_that(is_database(database))
  
  out <- map2(branch(diff), database$data, \(x, y) {
    rows_upsert(y, 
                filter(x, diff == "behind") |> 
                  select(-diff), 
                by = "subject_id")
  }) 
  updates <- out |>
    map_vec(~(length(which(is.na(.x$id))))) |> 
    sum()
  out <- out |>
    map(~(arrange(.x, created_at) |> 
            mutate(id = row_number())))
  structure(list(source = "database", data = out), class = "collection")
}
merge.data.frame <- function(x, y, ...) {
  merge.data.frame(x, y, ...)
}
merge.default <- function(x, y, ...) {
  merge.default(x, y, ...)
}

e <- new.env()
local(envir = e, {
  merge.collection_diff 
  .S3method("merge", "collection_diff")
})

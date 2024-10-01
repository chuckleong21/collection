box::use(
  DBI[dbConnect], 
  duckdb[duckdb],
  methods[setGeneric, setMethod],
  tools[file_ext],
  openxlsx2[read_xlsx],
  readxl[excel_sheets],
  stringr[str_detect], 
  purrr[map, list_rbind], 
  dplyr[as_tibble, mutate]
)


# "con" is an S4 object
#' @export
con <- dbConnect(duckdb(dbdir = "app/logic/database.duckdb"))

setGeneric("duckdb_connection", standardGeneric("import"))
setMethod("import", function(x, file) {
  if(!grepl("xlsx?", file_ext(file))) {
    stop(sprintf('Expects a "xlsx" or "xls" file but got %s file', file_ext(file)))
  }
  
  
})

movies |> 
  list_rbind() |> 
  tidyr::separate_wider_delim(简介, delim = " / ", 
                              names = c("年份", "地区", "类型", "导演", "主演"), 
                              too_few = "align_end") |> 
  View()

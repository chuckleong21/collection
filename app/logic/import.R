box::use(
  purrr[map, map2, map_chr, imap, map_vec, pmap,
        set_names, reduce, list_rbind], 
  readxl[excel_sheets], 
  stringr[str_which, str_match, str_count, str_c,
          str_extract_all, str_extract, str_split,
          str_replace_all, str_replace], 
  openxlsx2[read_xlsx], 
  dplyr[mutate, as_tibble, filter, arrange, row_number, transmute,
        rename, case_when, select, relocate, group_by, ungroup, 
        setequal, tbl, collect, rows_upsert, case_match],
  tidyr[drop_na, unnest_wider], 
  lubridate[ymd_hms, as_date],
  stats[na.omit], tools[file_ext],
  utils[head, tail, zip], 
  DBI[dbConnect, dbDisconnect, dbWriteTable], duckdb[duckdb]
)

box::use(
  app/logic/import_helpers[games_regex], 
  app/logic/collection[collection, write_collection, diff.collection],
  app/logic/request[request],
  app/logic/merge[merge],
  app/logic/fill[fill]
)

#' Import collection data
#'
#' @param file Scalar string. Path of the import file. Must be an .xlsx file
#'
#' @return A tibble
read_import <- function(file) {
  stopifnot("INVALID FILE TYPE: expects xlsx file" = file_ext(file) == "xlsx")
  sheet_regex <- c("^看.$|^.看$", "听", "读", "玩")
  meta_regex <- c("^https\\:\\/\\/([a-z]+)\\.douban\\.com\\/subject\\/(\\d+).$", 
                  "^https\\:\\/\\/www\\.douban\\.com\\/([a-z]+)\\/(\\d+).$")
  meta_types <- c("标题" = 0, "简介" = 0, "豆瓣评分" = 1, "链接" = 0, 
                  "创建时间" = 0, "我的评分" = 1, "标签" = 0, 
                  "评论" = 0, "可见性" = 0
  )
  rename_vec <- list(
    title      = "标题",
    intro      = "简介",
    rating     = "豆瓣评分",
    url        = "链接",
    created_at = "创建时间",
    my_rating  = "我的评分",
    tag        = "标签",
    comment    = "评论",
    privacy    = "可见性"
  )
  map(sheet_regex, \(x) {
    sheets <- excel_sheets(file)
    idx <- str_which(sheets, x)
    set_names(idx, sheets[idx])
  }) |> 
    reduce(c) |>
    imap(\(x, idx) {
      suppressWarnings(read_xlsx(file, sheet = x, type = meta_types)) |> 
        mutate(status = idx) |>
        as_tibble() |> 
        rename(!!!rename_vec)
    }) |> 
    list_rbind() |> 
    mutate(
      url_extract = str_match(url, meta_regex[1]), 
      url_extract = case_when(
        is.na(url_extract[, 1]) ~ str_match(url, meta_regex[2]),
        .default = url_extract
      ), 
      type = url_extract[, 2],
      subject_id = url_extract[, 3],
      created_at = ymd_hms(created_at) 
    ) |> 
    group_by(type) |>
    arrange(created_at) |>
    mutate(id = row_number()) |>
    select(-url_extract) |> 
    relocate(c(subject_id, type), .before = title) |>
    relocate(id, .before = subject_id) |> 
    ungroup()
}

clean_import <- function(data, which = NULL) {
  nm <- switch(
    which, 
    "movie" = c("year", "region", "genre", "director", "starring"),
    "music" = c("performer", "year"), 
    "book" = c("author", "year", "publisher")
  )
  if(which %in% c("movie", "music", "book")) {
    data <- data |> 
      filter(type == which) |> 
      drop_na(intro) |>
      mutate(
        r = map_chr(intro, \(s) paste0(paste0(rep(c("(.+)", "\\s\\/\\s"), str_count(s, "/")), 
                                              collapse = ""), "(.+)", collapse = "")),
        e = map2(intro, r, \(s, r) str_match(s, r)[, -1]), 
        e = map(e, `length<-`, max(lengths(e))), 
        true_length = map_vec(e, \(x) length(na.omit(x))),
        e = pmap(list(x = e, y = true_length, z = type), intro_inspect), 
        e = map(e, set_names, nm)
      ) |> 
      unnest_wider(e)
    
    data <- switch(
      which, 
      "movie" = data |> 
        transmute(id, subject_id = as.integer(subject_id), type, title, year = as.integer(year), 
                  region = map(region, str_split, pattern = "\\s"), 
                  region = reduce(region, c), 
                  region = map(region, \(x) case_match(x, 
                                                       "中国大陆" ~ "中国", "中國大陸" ~ "中国",
                                                       "中国香港" ~ "香港", "中國香港" ~ "香港",
                                                       "中国台湾" ~ "台湾", "中國台灣" ~ "台湾", 
                                                       .default = x)),
                  region = map_vec(region, str_c, collapse = " "),
                  genre, director, 
               starring, status, rating, my_rating, 
               url, created_at, cover = NA_character_, site = NA_character_) |>
        relocate(cover, .after = title) |> 
        relocate(site, .after = starring),
      "music" = data |> 
        transmute(id, subject_id = as.integer(subject_id), type, title, year = as.integer(year), performer, status, rating,
               my_rating, url, created_at, cover = NA_character_) |>
        relocate(cover, .after = title), 
      "book" = data |> 
        transmute(id, subject_id = as.integer(subject_id), type, title, year = as.integer(year), author, publisher, 
               status, rating, my_rating, url, created_at, cover = NA_character_) |>
        relocate(cover, .after = title)
    )
    return(data)
  }
  
  if(which == "game") {
    data <- data |> 
      filter(type == "game") |>
      drop_na(intro) |>
      mutate(
        category = str_extract_all(intro, games_regex("type")), 
        category = map_chr(category, ~paste0(.x, collapse = " ")),
        platform = str_extract_all(intro, games_regex("platform")), 
        platform = map_chr(platform, ~paste0(.x, collapse = " ")), 
        release = str_extract(intro, "\\d{4}?\\-?\\d{2}\\-\\d{2}$"), 
        intro = str_replace_all(intro, games_regex("type"), ""), 
        intro = str_replace_all(intro, games_regex("platform"), ""), 
        intro = str_replace_all(intro, "\\d{4}?\\-?\\d{2}?\\-\\d{2}$", ""), 
        intro = str_replace_all(intro, "^((?:[^/])*/){5}", ""),
        intro = str_replace(intro, "/?$", ""), 
        developer = str_extract(intro, "[^/]+$")
      ) |> 
      transmute(id, subject_id = as.integer(subject_id), type, title, category, developer, 
             release = as_date(release), status, rating, my_rating, url, created_at, cover = NA_character_) |>
      relocate(cover, .after = title)
    return(data)
  }
  
  stop('invalid "which" argument: one of c("movie", "book", "music", "game")', call. = FALSE)
}

intro_inspect <- function(x, y, z) {
  stopifnot("z should be a single character" = is.character(z) && length(z) == 1)
  stopifnot("y should be a single integer" = is.integer(y) && length(y) == 1)
  if(z == "book") {
    if(y > 3) {
      tail(x, 3)
    } else head(x, 3)
  } else if(z %in% c("movie", "music")) {
    x
  }
}

#' @export
from_import <- function(file) {
  collections <- c(book = "book", game = "game", 
                   movie = "movie", music = "music")
  imports <- read_import(file = file)
  
  collections <- imap(collections, ~clean_import(imports, .x))
  collections$book$status <- factor(collections$book$status, levels = c("想读", "在读","读过"), ordered = TRUE)
  collections$movie$status <- factor(collections$movie$status, levels = c("想看", "在看","看过"), ordered = TRUE)
  collections$music$status <- factor(collections$music$status, levels = c("想听", "在听","听过"), ordered = TRUE)
  collections$game$status <- factor(collections$game$status, levels = c("想玩", "在玩","玩过"), ordered = TRUE)
  structure(collections, class = "collection")
}

#' @export
import_to_database <- function(file, dbdir = NULL) {
  dbdir <- dbdir %||% "app/static/database.duckdb"
  has_changed <- function(x, y) {
    bool <- map2(x$data, y$data, setequal) |>
      reduce(c)
    all(!bool)
  }
  database <- collection("database")
  message(sprintf('reading file "%s"...', file))
  imported <- suppressWarnings(collection("import", file = file))
  message("checking diff between import and database...")
  d <- diff(database, imported)
  rd <- request(d)
  if(nrow(rd) != 0) {
    con <- dbConnect(duckdb("app/static/requests.duckdb"))
    requests <- con |>
      tbl("requests") |>
      collect()
    rows_upsert(requests, rd, "url") |>
      dbWriteTable(con, "requests", "url", overwrite = TRUE)
    dbDisconnect(con)
    zip("requests.zip", "app/static/requests.duckdb")
  }
  
  message("merging import with database...")
  m <- merge(d, database)
  empty_cover <- any(map_vec(m$data, \(d) any(is.na(d$cover))))
  if(has_changed(database, m) | empty_cover) {
    message("completing collection...")
    f <- fill(m)
    message("writing collection to database...")
    write_collection(f, "database", dbdir = dbdir)
    message(sprintf("updated database %s", dbdir))
  }
  database <- collection("database")
  database
}

box::use(
  cli[combine_ansi_styles, style_italic],
  purrr[map, map_vec, list_rbind, map2, iwalk],
  duckdb[duckdb], 
  DBI[dbConnect, dbDisconnect, dbWriteTable],
  dplyr[tbl, collect, select, filter, case_when, mutate, setequal, 
        arrange, rows_upsert, row_number],
  waldo[compare],
  assertthat[assert_that, `on_failure<-`],
  openxlsx2[write_xlsx]
)

box::use(
  app/logic/read_import[...]
)

# explicit register S3 method for box module

# collection class -------------------------------------------------------

collection_get <- function(source = c("database", "import"), 
                           file = NULL, dbdir = NULL) {
  on.exit(dbDisconnect(con), add = TRUE)
  if(!all(source %in% c("import", "database"))) {
    stop('One or more elements in argument "source" is invalid')
  }
  collection_names <- c(book = "book", game = "game", 
                        movie = "movie", music = "music")
  
  dbdir <- dbdir %||% "app/static/database.duckdb"
  con <- dbConnect(duckdb(dbdir = dbdir))
  database <- map(collection_names, \(x) {
    tbl(con, x) |> collect()
  })
  database$book$status <- factor(database$book$status, levels = c("想读", "在读","读过"), ordered = TRUE)
  database$movie$status <- factor(database$movie$status, levels = c("想看", "在看","看过"), ordered = TRUE)
  database$music$status <- factor(database$music$status, levels = c("想听", "在听","听过"), ordered = TRUE)
  database$game$status <- factor(database$game$status, levels = c("想玩", "在玩","玩过"), ordered = TRUE)
  database$book$cover <- database$movie$cover <- database$music$cover <- database$game$cover <- NA_character_
  
  if("import" %in% source) {
    if(!is.null(file)) {
      import <- from_import(file)
      if(length(source) == 1) {
        return(
          structure(list(source = source, data = import), class = "collection")
        )
      } else {
        return(
          structure(
            list(
              source = source,
              data = list(
                get(source[1], environment()),
                get(source[2], environment())
              ) |> 
                set_names(source)
            ), 
            class = "collection"
          )
        )
      }
    }
    stop('argument "file" must not be NULL')
  } else if(source == "database") {
    return(structure(list(source = source, data = database), class = "collection"))
  } else stop('invalid "source" argument: either "database" or "import"')
}
#' @title Get collection
#' @description
#' A \code{collection} object is created by either the data 
#' stored in a database, or an external file exported through
#' web extension called \emph{豆伴}. For a collection created
#' by database, it uses a default local file: \code{app/logic/database.duckdb}.
#' Pay attention when you have to alter the file location.
#' 
#' @param source where the collection should be obtained from, 
#' either \code{"source"}, \code{"import"}, 
#' or \code{c("source", "import")}.
#'
#' @param file A xlsx file path that should not be be \code{NULL} when
#' \code{source} contains import.
#' @param dbdir A duckdb file path with a local path in \code{app/logic}.
#' See description.
#'
#' @return A \code{collection} object
#' @export
#' 
#' @examples
#' collection("database")
#' 
collection <- function(source = c("database", "import"), 
                       file = NULL, dbdir = NULL) {
  collection_get(source = source, file = file, dbdir = dbdir)
}

print.collection <- function(x, ...) {
    desc_na <- function(x) {
      ifelse(!is.na(x), x, 0)
    }
    col_steelblue4 <- combine_ansi_styles("steelblue4")
    col_movie <- combine_ansi_styles("#47c5f1")
    col_music <- combine_ansi_styles("#ed764d")
    col_book <- combine_ansi_styles("#65c272")
    col_game <- combine_ansi_styles("#f9af3f")
    
    if(length(x$source) == 1) {
      x_desc <- map(x$data, \(i) {
        split(i, i$status) |> 
          map_vec(\(d) nrow(d))
      })
    } else {
      x_desc <- map(x$data, \(i) {
        map(i, \(j) split(j, j$status) |> map_vec(\(d) nrow(d)))
      })
    }
    if(length(x$source) == 1) {
      cat(col_steelblue4(sprintf("<Collection>:%s\n", style_italic(x$source))), 
          col_movie("Movie"), "\t\t    |",
          col_music("Music"), "\t   |", 
          col_book("Books"), "     |", 
          col_game("Games"), "\n",
          paste0("看过", col_movie(x_desc$movie["看过"]), "部电影电视剧"), " |", 
          paste0("听过", col_music(x_desc$music["听过"]), "张专辑"), "|",
          paste0("读过", col_book(x_desc$book["读过"]), "本书"),  " |",
          paste0("玩过", col_game(x_desc$game["玩过"]), "款游戏"), "\n",
          paste0("在看", col_movie(desc_na(x_desc$movie["在看"])), "部电影电视剧"), "   |", 
          paste0("在听", col_music(desc_na(x_desc$music["在听"])), "张专辑"), "  |",
          paste0("在读", col_book(desc_na(x_desc$book["在读"])), "本书"),  "  |",
          paste0("在玩", col_game(desc_na(x_desc$game["在玩"])), "款游戏"), "\n",
          paste0("想看", col_movie(desc_na(x_desc$movie["想看"])), "部电影电视剧"), "  |", 
          paste0("想听", col_music(desc_na(x_desc$music["想听"])), "张专辑"), "  |",
          paste0("想读", col_book(desc_na(x_desc$book["想读"])), "本书"),  "  |",
          paste0("想玩", col_game(desc_na(x_desc$game["想玩"])), "款游戏"), "\n"
      )
    } else {
      walk2(x_desc, x$source, \(u, v) {
        cat(col_steelblue4(sprintf("<Collection>:%s\n", style_italic(v))), 
            col_movie("Movie"), "\t\t    |",
            col_music("Music"), "\t   |", 
            col_book("Books"), "     |", 
            col_game("Games"), "\n",
            paste0("看过", col_movie(u$movie["看过"]), "部电影电视剧"), " |", 
            paste0("听过", col_music(u$music["听过"]), "张专辑"), "|",
            paste0("读过", col_book(u$book["读过"]), "本书"),  " |",
            paste0("玩过", col_game(u$game["玩过"]), "款游戏"), "\n",
            paste0("在看", col_movie(desc_na(u$movie["在看"])), "部电影电视剧"), "   |", 
            paste0("在听", col_music(desc_na(u$music["在听"])), "张专辑"), "  |",
            paste0("在读", col_book(desc_na(u$book["在读"])), "本书"),  "  |",
            paste0("在玩", col_game(desc_na(u$game["在玩"])), "款游戏"), "\n",
            paste0("想看", col_movie(desc_na(u$movie["想看"])), "部电影电视剧"), "  |", 
            paste0("想听", col_music(desc_na(u$music["想听"])), "张专辑"), "  |",
            paste0("想读", col_book(desc_na(u$book["想读"])), "本书"),  "  |",
            paste0("想玩", col_game(desc_na(u$game["想玩"])), "款游戏"), "\n"
        )
      })
    }
  }

#' Compare Collections
#'
#' @param x Collection get from database.
#' @param y Collection get from an external file
#' @param compare Whether to compare x and y
#'
#' @return A \code{collection_diff} object
#' @export
diff.collection <- function(x, y, compare = TRUE) {
  stopifnot('sources in "x" is not unique' = length(x$source) == 1)
  stopifnot('sources in "y" is not unique' = length(y$source) == 1)
  stopifnot('source in "x" is not from "database"' = x$source == "database")
  stopifnot('source in "y" is not from "import"' = y$source == "import")
  diff_switch <- function(x, y) {
    i <- nrow(x); j <- nrow(y)
    if(i == 0) {
      s <- "behind"
    } else {
      if(j > 0) {
        if(x$status == y$status & x$created_at == y$created_at) {
          s <- "identical"
        } else {
          if(y$status > x$status) {
            s <- "behind"
          }
          if(y$status < x$status) {
            s <- "ahead"
          }
          if(y$status == x$status) {
            if(x$created_at > y$created_at) {
              s <- "ahead"
            } else {
              s <- "behind"
            }
          }
        }
      } else {
        s <- "ahead"
      }
    }
    return(s)
  }
  
  diff_sid <- function(x, y) {
    sid <- unique(c(x$subject_id, y$subject_id))
    compare_sid <- map_vec(seq_along(sid), \(i) {
      u <- filter(x, subject_id == sid[i]) |> select(-id)
      v <- filter(y, subject_id == sid[i]) |> select(-id)
      diff_switch(x = u, y = v)
    })
    branch <- map(c("behind", "ahead"), \(k) {
      filter(y, subject_id %in% sid[which(compare_sid == k)]) |> 
        mutate(diff = k) |>
        select(-id)
    }) |> list_rbind()
    main <- filter(x, subject_id %in% branch$subject_id) |> 
      select(-id)
    list(main = main, branch = branch)
  }
  out <- map2(x$data, y$data, \(x, y) {
    diff_sid(x, y)
  })
  # out <- out[which(!map(out, \(x) map_vec(x, nrow)) |> map_vec(sum) == 0)]
  structure(list(compare = compare, diff = out), class = "collection_diff")
}



write_collection_xlsx <- function(collection, file) {
  assert_that(inherits(collection, "collection"), 
              length(file) == 1,
              tools::file_ext(file) %in% c("xls", "xlsx"))
  iwalk(collection$data, \(x, idx) {
    write_xlsx(x, file = file, sheet = idx)
  })
}
write_collection_duckdb <- function(collection, dbdir = NULL) {
  on.exit(dbDisconnect(con), add = TRUE)
  assert_that(inherits(collection, "collection"))
  dbdir <- dbdir %||% "app/logic/database.duckdb"
  assert_that(
    length(dbdir) == 1,
    "duckdb" == tools::file_ext(dbdir)
  )
  con <- dbConnect(duckdb(dbdir = dbdir))
  iwalk(collection$data, \(x, idx) {
    dbWriteTable(con, idx, x, overwrite = TRUE)
  })
}

#' @export
write_collection <- function(collection, 
                             to = c("worksheet", "database"), 
                             ...) {
  call <- as.list(match.call())
  if(length(to) != 1) {
    stop(paste0('argument "to" is either "worksheet" or "database",', 
                " not ", deparse(call$to)))
  } else if(!to %in% c("worksheet", "database")) {
    stop(paste0('argument "to" is either "worksheet" or "database",', 
                " not ", deparse(call$to)))
  }
  if(to == "worksheet") {
    write_collection_xlsx(collection = collection, ...)
  }
  if(to == "database") {
    write_collection_duckdb(collection = collection, ...)
  }
}


# collection_diff class --------------------------------------------------

print.collection_diff <- function(x) {
  if(x$compare) {
    is_identical <- all(map(x$diff, \(x) map_vec(x, nrow)) |> map_vec(sum) == 0)
    if(is_identical) {
      branch_no_diff <- map(x$diff, \(xx) xx$branch[, -charmatch("diff", names(xx$branch))])
      x$diff <- map(x$diff, \(x) x$main) |>
        map2(branch_no_diff, \(x, y) list(main = x, branch = y))
    }
    print(map(x$diff, \(diff) {
      compare(diff$main, diff$branch)
    }))
  } else {
    print(x$diff)
  }
  
}


e <- new.env()
local(envir = e, {
  print.collection_diff
  .S3method("print", "collection_diff")
})
local(envir = e, {
  diff.collection
  .S3method("diff", "collection")
})
local(envir = e, {
  print.collection
  .S3method("print", "collection")
})

local(envir = e, {
  diff.collection
  .S3method("diff", "collection")
})
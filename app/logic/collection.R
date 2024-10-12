box::use(
  cli[combine_ansi_styles, style_italic],
  purrr[imap, map, set_names, map_vec, map2, list_rbind],
  duckdb[duckdb], 
  DBI[dbConnect, dbDisconnect],
  dplyr[tbl, collect, select, filter, case_when, mutate, setequal],
  waldo[compare]
)

box::use(
  app/logic/read_import[...]
)

# explicit register S3 method for box module

# collection method -------------------------------------------------------

e <- new.env()
local(envir = e, {
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
      x_desc <- purrr::map(x$data, \(i) {
        purrr::map(i, \(j) split(j, j$status) |> map_vec(\(d) nrow(d)))
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
      purrr::walk2(x_desc, x$source, \(u, v) {
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
  .S3method("print", "collection")
})
#' Compare Collections
#'
#' @param x Collection get from database.
#' @param y Collection get from an external file
#' @param compare Whether to compare x and y
#'
#' @return A \code{collection_diff} object
diff.collection <- function(x, y, compare = FALSE) {
  stopifnot('sources in "x" is not unique' = length(x$source) == 1)
  stopifnot('sources in "y" is not unique' = length(y$source) == 1)
  stopifnot('source in "x" is not from "database"' = x$source == "database")
  stopifnot('source in "y" is not from "import"' = y$source == "import")
  
  diff_sid <- function(x, y) {
    args <- get("args", environment())
    sid <- unique(c(x$subject_id, y$subject_id))
    compare_sid <- map_vec(seq_along(sid), \(i) {
      u <- filter(x, subject_id == sid[i]) |> select(-id)
      v <- filter(y, subject_id == sid[i]) |> select(-id)
      case_when(nrow(u) != 0 & nrow(v) == 0 ~ "behind", 
                nrow(u) != 0 & nrow(v) != 0 & !setequal(u, v) ~ "behind",
                nrow(u) == 0 & nrow(v) != 0 ~ "ahead", 
                .default = "identical") 
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
  out <- out[which(!map(out, \(x) map_vec(x, nrow)) |> map_vec(sum) == 0)]
  structure(list(compare = compare, diff = out), class = "collection_diff")
}
local(envir = e, {
  diff.collection
  .S3method("diff", "collection")
})

collection_get <- function(source = c("database", "import"), 
                           file = NULL, dbdir = NULL) {
  on.exit(dbDisconnect(con))
  if(!all(source %in% c("import", "database"))) {
    stop('One or more elements in argument "source" is invalid')
  }
  collection_names <- c(book = "book", game = "game", 
                        movie = "movie", music = "music")
  
  dbdir <- dbdir %||% "app/logic/database.duckdb"
  con <- dbConnect(duckdb(dbdir = dbdir))
  database <- map(collection_names, \(x) {
    tbl(con, x) |> collect()
  })
  
  if("import" %in% source) {
    if(!is.null(file)) {
      imports <- read_import(file = file)
      import <- imap(collection_names, ~clean_import(imports, .x))
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


# collection_diff method --------------------------------------------------

#' @export
branch <- function(diff) {
  UseMethod("branch")
}
local(envir = e, {
  branch.collection_diff <- function(x) {
    map(x$diff, ~.x$branch)
  }
  .S3method("branch", "collection_diff")
})
local(envir = e, {
  print.collection_diff <- function(x) {
    if(x$compare) {
      print(map(x$diff, \(diff) {
        compare(diff$main, diff$branch)
      }))
    } else {
      print(x$diff)
    }
  }
  .S3method("print", "collection_diff")
})
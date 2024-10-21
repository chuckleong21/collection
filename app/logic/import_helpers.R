#' @export
games_regex <- function(type, regex = NULL) {
  regex <- switch(type, 
                  "type" = regex %||% c("第一人称射击", "体育", "冒险", 
                                        "角色扮演", "动作", "卡牌", 
                                        "大型多人在线", "横版过关", 
                                        "策略", "益智", "模拟", 
                                        "射击", "格斗", "乱斗", "清版"), 
                  "platform" = regex %||% c("PC", "iPhone", "iPad", "Mac", 
                                            "Android", 
                                            "PlayStation\\s?\\d?", "PSV", 
                                            "(PSV/PS Vita)", "Windows Phone",
                                            "Xbox.+?/", "Linux", "Steam VR",
                                            "Browser", "Nintendo Switch")
  )
  regex <- paste0(paste0(regex[-length(regex)], "|", collapse = ""), 
                  regex[length(regex)], collapse = "")
  sprintf("(?=.*%s)(%s)", regex , regex)
}

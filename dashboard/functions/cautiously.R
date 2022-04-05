library(purrr)
library(glue)
library(htmltools)


#' Cautiously
#'
#' Combines \link{purrr}'s safely and cautiously, returning a function that calls ` .f`,
#' and returns a list containing `result`, `error`, `warning` , `message`, and `output`.
#'
#' If an error occurs while calling `.f`, `result` will be `NULL`, otherwise `error` will be `NULL`
#' @param .f function to wrap
cautiously <- function(.f) {
  f <- quietly(safely(.f))

  function(...) {
    result <- f(...)
    result$error <- result$result$error
    result$warnings <- result$warnings %>% discard(~ !is.null(.x) && .x != "")
    result$messages <- result$messages %>% discard(~ !is.null(.x) && .x != "")
    result$result <- result$result$result
    structure(result, class = c("list", "cautious"))
  }
}

as.character.cautious <- function(.x, html = FALSE) {
  errors <- character()
  if (!is.null(.x$error)) {
    errors <- .format_status_part(.x$error$message, "danger", html)
  }
  warnings <- .format_status_part(.x$warnings, "warning", html)
  messages <- .format_status_part(.x$messages, "info", html)
  .collapse(c(errors, warnings, messages))
}

print.cautious <- function(.x, ...) {
  if (!is.null(.x$result)) {
    print(.x$result, ...)
  }
  print(as.character(.x), html = FALSE)
}

.format_status_part <- function(message, status, html = FALSE, container = div) {
  if (length(message) > 1) {
    message %>% map_chr(.format_status_part, status = status, html = html)
  } else if (length(message) == 0) {
    return("")
  } else {
    if (html) {
      div(class = paste("box", status), message) %>% as.character()
    } else {
      glue("{status}: {message}")
    }
  }
}

.collapse <- function(messages) {
  paste0(messages %>% discard(~ is.null(.x) || .x == ""), sep = "", collapse = "\n")
}

#' Parse a webhook body into a tabular structure
#'
#' @param response a POST body from a typeform webhook that is converted to a list.
#' @param attempt_table create a table from the response object?
#' @param wide_table Should the table be wide, with one var per column
#'
#' @return
#' @export
#'
#' @examples
parse_webhook <- function (response, attempt_table = TRUE, wide_table = TRUE)
{
  response.l <- response
  if (attempt_table) {
    response <- {
      function(x) data.table::setcolorder(x, c("submitted_at", names(x)[which(!names(x) %in%
                                                                                c("submitted_at"))]))
    }(data.table::rbindlist(lapply(seq_along(response$form_response$submitted_at),
                                   function(i) {
                                     x = data.table::as.data.table(response$form_response$answers[i])
                                     x[, `:=`(submitted_at, response$form_response$submitted_at[i])]
                                   })))
    values <- as.vector(sapply(response.l["form_response"][[1]][["answers"]],
                               function(i) {
                                 sapply(seq_along(i[["type"]]), function(j) {
                                   f_extract <- val_extractor(i[["type"]][j])
                                   f_extract(i, j)
                                 })
                               }))
    refs <- as.vector(sapply(response.l["form_response"][[1]][["answers"]],
                             function(i) {
                               i[[1]][["ref"]]
                             }))
    response[, `:=`(value, values)]
    response[, `:=`(ref, refs)]
    if (!is.null(response.l[["form_response"]][["hidden"]])) {
      x = data.table::as.data.table(response.l[["form_response"]][["hidden"]])
      x[, `:=`(submitted_at, response.l$form_response$submitted_at)]
      x <- data.table::melt(x, id.vars = c("submitted_at"), value.name = "value", variable.name = "ref")
      response <- rbind(x, response, fill = TRUE)[order(submitted_at)]
    }
    if (wide_table)
      response <- response[, data.table::dcast(.SD, submitted_at ~ ref)]
  }
  response
}

host_api <- function(endpoint, handler, ..., get_test = FALSE) {
  plumber::pr() |>
    {
      \(x) if (get_test) {
        plumber::pr_get(x, "/",
                        \(res) {
                          res$status <- 200
                          "endpoint running"
                        })
      } else {
        x
      }
    }() |>
    plumber::pr_post(endpoint,
                     handler) |>
    plumber::pr_run(...)
}

host_api("/form",
         \(req, res) {
           res$status <- 200
           },
         host = "0.0.0.0",
         port = 8000,
         docs = FALSE
         )




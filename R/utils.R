# Internal helper: coerce input to data.frame
.as_df <- function(data) {
  tryCatch(
    as.data.frame(data),
    error = function(e) {
      stop("`data` must be coercible to a data.frame.", call. = FALSE)
    }
  )
}


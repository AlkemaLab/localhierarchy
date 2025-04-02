
#' @import tibble
hierarchical_model_matrix <- function(columns, data) {
  mat <- matrix(NA, nrow = nrow(data), ncol = 0)
  assign <- c()

  index <- tibble::tibble(i = numeric(0), column = character(0), level = character(0))

  i <- 1
  if("intercept" %in% columns) {
    mat <- cbind(mat, rep(1, nrow(mat)))
    assign <- c(assign, 0)

    index[i, ] <- tibble(i = i, column = "intercept", level = "intercept")

    i <- i + 1
  }

  for(column in columns) {
    for(l in levels(factor(data[[column]]))) {
      mat <- cbind(mat, as.numeric(data[[column]] == l))
      assign <- c(assign, which(column == columns))
      index[i, ] <- tibble::tibble(i = i, column = column, level = l)

      i <- i + 1
    }
  }

  list(
    assign = assign,
    matrix = mat,
    index = index
  )
}


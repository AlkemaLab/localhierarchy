

is_there_a_comma <- function(variable){
  grepl(
    pattern = ",",
    x = variable,
    fixed = TRUE)
}
# is_there_a_comma("a[1,1]")

# get_dimension <- function(variable){
#   comma_matches <- gregexpr(
#     pattern = ",",
#     text = variable,
#     fixed = TRUE)[[1]]
#   if (length(comma_matches) == 1 && comma_matches[[1]] == -1) {
#     num_inds <- 1
#   } else {
#     num_inds <- length(comma_matches) + 1
#   }
#   return(num_inds)
# }
#
# #get_dimension(c("a[1,1]", "a[2,1]"))
# #get_dimension(c("a[1]", "a[2]"))
# #get_dimension(c("a[1,1]", "a[1,2]"))


#' Create a matrix
#'
#' This function takes a postsumm object and returns a matrix with the
#' relevant information
#'
#' @param post_summ tibble with columns `variable` and `postmean`,
#' variable examples are 'a[1,1]', 'a[2,1]', etc.
#'
#' @returns matrix with rows and columns corresponding to the indices in the variable
#' @export
#'
create_a_matrix <- function(post_summ){
  res <- post_summ %>%
    mutate(
    row = gsub(pattern = ".*\\[(.*),.*", replacement = "\\1", x = variable),
    col = gsub(pattern = ".*\\[.*,(.*)\\]", replacement = "\\1", x = variable)
    ) %>%
    select(-variable) %>%
    # make integers
    tidyr::pivot_wider(names_from = col, values_from = postmean) %>%
    tidyr::pivot_longer(cols = -row, names_to = "col", values_to = "postmean") %>%
    mutate(row = as.integer(row),
           col = as.integer(col))
  m <- matrix(NA, max(res$row), max(res$col))
  for (rowi in unique(res$row)){
    for (coli in unique(res$col)){
      m[rowi, coli] <- res %>%
        filter(row == rowi, col == coli) %>%
        pull(postmean)
    }
  }
  m
}
#create_a_matrix( tibble(variable = c("a[1,1]", "a[2,1]"), postmean = seq(1,2)) )
#robust to ordering of the variables
#create_a_matrix( tibble(variable = c("a[2,1]", "a[1,1]"), postmean = seq(1,2)) )
#create_a_matrix( tibble(variable = c("a[1,1]", "a[2,1]", "a[1,2]", "a[2,2]"), postmean = seq(1,4)) )
#create_a_matrix( tibble(variable = c("a[1,1]",  "a[1,2]", "a[2,1]","a[2,2]"), postmean = seq(1,4)) )

#' Determine whether or not a variable exists in draws from a fit
#' This is similar to the function posterior:::check_existing_variables,
#' but note that function is not exported from posterior
#'
#' @param fit fpemplus fit object
#' @param var_name name of variable as character string
#'
#' @return TRUE or FALSE indicating whether or not the variable exists in the
#' fit.
variable_exists_in_draws <- function(fit, var_name) {
  all_variables <- posterior::variables(fit$samples$draws())
  all_variables_no_index <- stringr::str_split_i(string = all_variables,
                                                 pattern = fixed("["),
                                                 i = 1)
  return(any(all_variables_no_index == var_name))
}



#' split_var_name_and_index
#' For a data frame of estimates with a column that contains combined info
#' about a variable name and row and/or column indices, add columns containing
#' just the variable name and the indices.
#'
#' @param estimate_df data frame with a column called `variable` that contains parname[i] or parname[i,j] entries
#' @param num_inds number of indices in the variable name (used for 1 or 2)
#'
#' @returns data frame with the same columns as `estimate_df` plus columns containing
#' just the variable name and the indices.
#' @export
#'
#' @examples
#' split_var_name_and_index(
#' estimate_df = tibble(variable = c("a[1]", "a[2]")), num_inds = 1)
#' split_var_name_and_index(
#' estimate_df = tibble(variable = c("a[1,1]", "a[2,1]")), num_inds = 2)
#'
split_var_name_and_index <- function(estimate_df, num_inds) {
  # column names for indices, starting from "i"
  ind_names <- letters[seq(from = 9, length = num_inds)]

  estimate_df <- estimate_df |>
    dplyr::mutate(
      variable_mod = substr(variable, 1, nchar(variable) - 1)
    ) |>
    tidyr::separate_wider_delim(
      cols = "variable_mod",
      delim = "[",
      names = c("var_name", "inds")
    ) |>
    tidyr::separate_wider_delim(
      cols = "inds",
      delim = ",",
      names = ind_names
    ) |>
    dplyr::mutate_at(all_of(ind_names), as.integer)

  return(estimate_df)
}
# # if we get it wrong
# split_var_name_and_index(
#   estimate_df = tibble(variable = c("a[1,1]", "a[2,1]")),
#   num_inds = 1)
# split_var_name_and_index(
#   estimate_df = tibble(variable = c("a[1]", "a[2]")),
#   num_inds = 2)


#' Starting from an array of parameter values (of dimension 1 or 2),
#' create a data frame with columns `variable` with the combined parameter name
#' and indices, and `median` with the parameter values
param_array_to_indexed_df <- function(
    param_values,
    num_inds,
    n,
    m,
    param_name) {
  if (num_inds == 1) {
    param_summary <- data.frame(
      var_name = param_name,
      i = seq_len(n),
      mean = param_values,
      #median = param_values,
      variable = paste0(param_name, "[", seq_len(n), "]")
    )
  } else {
    param_summary <- param_values |>
      as.data.frame() |>
      `colnames<-`(seq_len(m)) |>
      dplyr::mutate(i = seq_len(n)) |>
      tidyr::pivot_longer(
        cols = as.character(seq_len(m)),
        names_to = "j",
        names_transform = as.integer,
       values_to = "mean"
      #  values_to = "median"
      ) |>
      dplyr::mutate(
      #  median = mean,
        variable = paste0(param_name, "[", i, ",", j, "]")
      )
  }

  return(param_summary)
}

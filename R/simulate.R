#' Simulate nested multilevel data and etas
#'
#' Simulates a nested multilevel data structure with etas at each level.
#' Lowest level can be used as observational error.
#'
#' @param n_levels Number of hierarchical levels (default: 3).
#' @param n_units_perlevel A vector specifying the number of units at each level (default: rep(25, n_levels)).
#' @param sigma_perlevel A vector specifying the standard deviation of etas at each level (default: rep(1, n_levels)).
#' @param mu_global Global mean for the outcome variable (default: 0).
#' @param seed Random seed for reproducibility (default: 1234).
#'
#' @returns A tibble with columns for each level, etas, and the outcome variable.
#' @export
#'
#' @examples
simulate_etas_nested_multilevel_data <- function(n_levels = 3,
                                            n_units_perlevel = rep(25, n_levels),
                                            sigma_perlevel = rep(1, n_levels),
                                            mu_global = 0,
                                            seed = 1234) {
  set.seed(seed)

  if (length(n_units_perlevel) != n_levels || length(sigma_perlevel) != n_levels) {
    stop("Lengths of `n_units_perlevel` and `sigma_perlevel` must match `n_levels`.")
  }

  # Step 1: Generate full nested structure
  generate_ids <- function(n_units_perlevel) {
    # Start from top level
    id_list <- expand.grid(rev(lapply(n_units_perlevel, function(n) seq_len(n))))
    colnames(id_list) <- paste0("level", rev(seq_along(n_units_perlevel)))

    # Reverse columns to make level1 highest
    id_list <- id_list[, rev(seq_len(ncol(id_list)))]

    # Create unique labels for each level
    for (lvl in seq_along(n_units_perlevel)) {
      lvl_name <- paste0("level", lvl)
      if (lvl == 1) {
        id_list[[lvl_name]] <- factor(id_list[[lvl_name]])
      } else {
        #parent <- do.call(paste, c(id_list[1:(lvl - 1)], sep = "_"))
        # previous level is already uniquely labeled
        parent <- do.call(paste, c(id_list[lvl - 1], sep = "_"))
        id_list[[lvl_name]] <- factor(paste0(parent, "_", id_list[[lvl_name]]))
      }
    }
    return(id_list)
  }

  df <- tibble(generate_ids(n_units_perlevel))

  # Step 2: Simulate etas per level
  total_effect <- rep(0, nrow(df))
  for (lvl in seq_len(n_levels)) {
    lvl_col <- paste0("level", lvl)
    u <- rnorm(length(levels(df[[lvl_col]])), mean = 0, sd = sigma_perlevel[lvl])
    names(u) <- levels(df[[lvl_col]])
    df[[paste0(lvl_col, "_eta")]] <- u[as.character(df[[lvl_col]])]
    total_effect <- total_effect + u[as.character(df[[lvl_col]])]
  }

  df <- df %>%
    mutate(mu_global = mu_global,
           total_effect = mu_global + total_effect)
  return(df)
}

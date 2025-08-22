#' Simulate multilevel data
#'
#' Simulates a nested multilevel data structure with etas at each level.
#' Lowest level can be used as observational error.
#'
#' @param n_levels Number of hierarchical levels (default: 3).
#' @param n_units_perlevel A vector specifying the number of units at each level (default: rep(25, n_levels)).
#' @param sigma_perlevel A vector specifying the standard deviation of etas at each level (default: rep(1, n_levels)).
#' @param mu_global Global mean for the outcome variable (default: 0).
#' @param add_data Logical indicating whether to add observations (default: TRUE). If TRUE, adds column y.
#' @param n_data Number of observations to add at the lowest level (default: 10).
#' @param sigma_y Standard deviation of the added observations (default: 0.05)
#' @param add_data1levelup Logical indicating whether to add observations one level up
#' e.g. at national level if lowest level is subnational (default: FALSE).
#' @param seed Random seed for reproducibility (default: 1234).
#'
#' @returns A tibble with columns for each level, mu_global, etas, and their total labeled y
#' @export
#'
simulate_multilevel_data <- function(n_levels = 3,
                                      n_units_perlevel = rep(25, n_levels),
                                      sigma_perlevel = rep(1, n_levels),
                                      mu_global = 0,
                                      add_data = TRUE,
                                      n_data = 10,
                                      sigma_y = 0.05,
                                      add_data1levelup = FALSE,
                                      seed = 12345
                                     ) {
  set.seed(seed)

  if (length(n_units_perlevel) != n_levels || length(sigma_perlevel) != n_levels) {
    stop("Lengths of `n_units_perlevel` and `sigma_perlevel` must match `n_levels`.")
  }

  # Step 1: Generate full nested structure
  # add data here if requested, as it's just another level
  if (add_data){
    n_levels <- n_levels + 1
    n_units_perlevel <- c(n_units_perlevel, n_data) # Add one unit at the top level for global mean
    sigma_perlevel <- c(sigma_perlevel, sigma_y)
  }

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
  y <- rep(0, nrow(df))
  for (lvl in seq_len(n_levels)) {
    lvl_col <- paste0("level", lvl)
    u <- rnorm(length(levels(df[[lvl_col]])), mean = 0, sd = sigma_perlevel[lvl])
    names(u) <- levels(df[[lvl_col]])
    df[[paste0(lvl_col, "_eta")]] <- u[as.character(df[[lvl_col]])]
    y <- y + u[as.character(df[[lvl_col]])]
  }

  df <- df %>%
    mutate(mu_global = mu_global,
           y = mu_global + y)
  if (!add_data1levelup | ! add_data)
    return(df)

  # if add_data1levelup, to get data one level up
  # only done if add_data is TRUE
  # take y and remove the 2 next levels (noise and the level itself)
  # and add new noise
  data_1levelup <- df %>%
    rename(obslevel =  paste0("level", n_levels, "_eta"),
           levelup = paste0("level", n_levels - 1, "_eta")) %>%
    mutate(y_new = y - obslevel - levelup +
             # add noise to the new level
            rnorm(nrow(df), sd = sigma_y)) %>%
    select(-y) %>%
    rename(y = y_new)

  return(
    list(
      data = df,
      data_1levelup = data_1levelup)
      )
}


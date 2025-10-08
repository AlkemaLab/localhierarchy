
#' Plot summaries of hierarchical parameters
#'
#' Display outputs from posterior_summary_hierparam_localhierarchy
#'
#' @param res output from posterior_summary_hierparam_localhierarchy
#' @param hierarchy_select optional, what hierarchical level to show? if NULL, all levels are shown
#' @param areas_select optional: specific areas in a level to filter by, allowed only if one hierarchical level is selected
#' @param res2 optional, output from posterior_summary_hierparam_localhierarchy for comparison
#' @param modelname1 label for res
#' @param modelname2 label for res2
#' @param k_select optional, if res contains k, which k values to show? if NULL, all k values are shown
#' @param dodge used for offsetting plots, default is 0.5
#'
#' @returns ggplot object
#'
plot_posterior_summaries_localhierarchy <- function(
    res,
    hierarchy_select = NULL,
    areas_select = NULL,
    res2 = NULL,
    modelname1 = "model 1",
    modelname2 = "model 2",
    k_select = NULL,
    dodge = position_dodge(width=0.5)  # for offsetting plots
) {
  if (!is.null(areas_select) & is.null(hierarchy_select)) {
    stop("You must select a hierarchy to filter the areas. Please provide a hierarchy_select argument.")
  } else {
    if (!is.null(areas_select)){
      res[[hierarchy_select]] <-
        res[[hierarchy_select]] %>%
        filter(name %in% areas_select)
    }
  }
  if (is.null(hierarchy_select)) {
    hierarchy_select <- names(res)
  }
  p <- list()
  for (hierarchy in hierarchy_select){
    res_all <- NULL
    # check if we need to plot res2
    if (!is.null(res2)){
      if (!is.null(res2[[hierarchy]])) {
        res_all <-
          res[[hierarchy]] %>%
          mutate(model = modelname1) %>%
          bind_rows(
            res2[[hierarchy]] %>%
              mutate(model = modelname2) %>%
              filter(name %in% res[[hierarchy]]$name)
          )
    }}
    if (is.null(res_all)){
      res_all <- res[[hierarchy]] %>%
        mutate(model = modelname1)
    }
    if (!is.element("k", names(res_all))) {
      p[[hierarchy]] <-
        res_all %>%
        group_by(name, model) %>%
        reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
        ggplot(aes(y = y, x = name, color = model)) +
        geom_linerange(aes(ymin = ymin, ymax = ymax), position=dodge) +
        geom_point(position = dodge) +
        coord_flip() +
        ylab("Parameter estimates")
    } else {
      if (!is.null(k_select)) {
        res_all <- res_all %>%
          filter(k %in% k_select)
      }
      p[[hierarchy]] <-
        res_all %>%
        group_by(name, k, model) %>%
        reframe(y = val[quant == 0.5], ymin = val[quant == 0.025], ymax = val[quant == 0.975]) %>%
        ggplot(aes(y = y, x = name, color = model)) +
        geom_linerange(aes(ymin = ymin, ymax = ymax), position=dodge) +
        geom_point(position = dodge) +
        coord_flip() +
        facet_wrap(~k, ncol = 1) +
        ylab("Parameter estimates")
    }
  }
  return(p)
}

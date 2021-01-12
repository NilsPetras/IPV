#' Item Overview
#'
#' Shows all (squared) factor loadings of all items in all models in a plot grid
#' of bar plots.
#'
#' @param data raw SEM estimates in the appropriate format, given by the input
#'   functions.
#' @param squared logical; should factor loadings be squared?; defaults to TRUE
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg"; defaults to
#'   "none".
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#'
#' @return gg / ggplot object; plot grid with one bar plot per item showing
#'   (squared) factor loadings of that item in all IPV models, arranged by
#'   facets and tests
#'
#' @details
#'   File output produces much more reliable results than display within R.
#'
#'   Unlike the IPV chart functions, this function is not meant to
#'   produce print-ready output under all circumstances. Suggestions via github
#'   are welcome.
#'
#' @export
#'
#' @examples
#' # Honesty/Humility and Agreeableness items
#' # the use of file output is recommended to prevent irregular placement of plot labels
#' res <- ipv_est(HEXACO[ ,c(2:41, 122:161)], "HA")
#' item_overview(res$est_raw)
#'
#'
item_overview <- function(
  data,
  squared = TRUE,
  file_name = "none",
  # size = 1,
  dpi = 500) {


  # size parameter currently out of use, not yet properly implemented

  # data preparation -----------------------------------------------------------

  # collect factor loadings
  loads <- data$global$fls
  names(loads) <- c(
    "construct",
    "test",
    "item",
    "construct_loading",
    "test_loading")
  loads$facet <- NA
  loads$facet_loading <- NA
  loads <- loads[ ,c(1, 2, 6, 3, 4, 5, 7)]
  temp <- lapply(data$tests, '[[', 1)
  temp <- lapply(temp, function(x){ # rename items uniquely
    x$item <- as.character(paste(
      x$factor,
      sep = ".",
      x$item))
    return(x)
  })
  temp <- do.call("rbind", temp)
  facets <- temp$subfactor
  names(facets) <- temp$item
  facet_loadings <- temp$subfactor_loading
  names(facet_loadings) <- temp$item
  loads$facet <- facets[as.character(loads$item)]
  loads$facet_loading <- facet_loadings[as.character(loads$item)]

  # set loadings < 0 to NA and throw warning if so
  loads[ ,5:7][apply(loads[,5:7], 2, function(x) x < 0)] <- NA
  if (any(is.na(loads))) {
    warning(paste(
      sum(is.na(loads)),
      " negative factor loadings set to 0 for display.",
      sep = ""))
  }
  loads[is.na(loads)] <- 0

  if (squared) {
    loads[ ,5:7] <- apply(loads[ ,5:7], 2, function(x){
      x <- x ^ 2
    })
  }

  # factor loadings in long format
  long <- reshape2::melt(loads, id.vars = 4, measure.vars = 5:7)
  long$variable <- as.character(long$variable)
  long$variable[long$variable == "construct_loading"] <-
    as.character(loads$construct)
  long$variable[long$variable == "test_loading"] <-
    as.character(loads$test)
  long$variable[long$variable == "facet_loading"] <-
    as.character(loads$facet)
  long$variable <- factor(
    long$variable,
    levels = unique(long$variable)) # conserves level order
  long$test <- rep(
    long[(1 / 3 * nrow(long) + 1) : (2 / 3 * nrow(long)), "variable"],
    3)

  # hierarchical data structure as list to sort plots
  chunks <- long[(2 / 3 * nrow(long) + 1) : nrow(long), ]
  chunks <- split(
    chunks,
    f = chunks[ ,"test"],
    drop = T)
  for (i in 1:length(chunks)) {
    chunks[[i]] <- split(chunks[[i]], f = droplevels(chunks[[i]][ ,"variable"]))
    chunks[[i]] <- lapply(chunks[[i]], '[[', 1)
  }


  # plot creation --------------------------------------------------------------

  plots <- chunks
  for (i in 1:length(plots)) {
    plots[[i]] <- lapply(plots[[i]], as.list)
    for (j in 1:length(plots[[i]])) {
      names(plots[[i]][[j]]) <- chunks[[i]][[j]]
    }}

    ## individual item plot for each item

  for (i in 1:length(chunks)) { # i = test
    for (j in 1:length(chunks[[i]])) { # j = facet
      # is_last <- as.numeric(j == length(chunks[[i]]))
      for (k in 1:length(chunks[[i]][[j]])) { # k = item

        plots[[i]][[j]][[k]] <- ggplot2::ggplot(
          long[which(long$item == chunks[[i]][[j]][k]), ]) +

          # initialize
          ggplot2::scale_fill_brewer(palette = "Greens") +
          ggplot2::theme_minimal() +
          ggplot2::ylim(0,1) +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
              color = "gray", size = .25, linetype = "dashed"),
            panel.grid.minor.y = ggplot2::element_blank(),
            legend.position    = "none",
            axis.title.x       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            plot.margin        = grid::unit(c(0, 0, .5, 0), "in"),
            axis.text          = ggplot2::element_text(size = 6),
            axis.text.y        = ggplot2::element_text(color = "gray")) +

          # bar plot
          ggplot2::geom_col(
            ggplot2::aes(
              x = long[which(long$item == chunks[[i]][[j]][k]), ]$variable,
              y = long[which(long$item == chunks[[i]][[j]][k]), ]$value,
              fill = long[which(long$item == chunks[[i]][[j]][k]), ]$variable))

        if (k > 1) {
          plots[[i]][[j]][[k]] <-  plots[[i]][[j]][[k]] +
            ggplot2::theme(
              axis.text.y = ggplot2::element_blank())
        }
      }
    }
  }

    ## plot grid --------------------

  nrows <- unlist(lapply(plots, length))
  width <- max(unlist(lapply(plots, function(x) lapply(x, length))))
  facets <- chunks
  tests <- chunks

  for (i in 1:length(facets)) {
    for (j in 1:length(facets[[i]])) {
      labels <- unlist(lapply(strsplit(names(plots[[i]][[j]]), split = "\\."), "[[", 2))
      hjust <- -10 / nchar(labels) # adjust labels based on string length

  # plot grid of item plots for each facet
      facets[[i]][[j]] <- cowplot::plot_grid(
        plotlist = plots[[i]][[j]][1:width], # adds empty plots at the rear
        labels = labels,
        hjust = hjust,
        vjust = 3,
        nrow = 1,
        label_size = 10)
    }

  # plot grid of facet plot grids for each test
    tests[[i]] <- cowplot::plot_grid(
      plotlist = facets[[i]],
      labels = names(plots[[i]]),
      ncol = 1,
      vjust = -.5)
  }

  # overall plot grid of test plot grids
  grd <- cowplot::plot_grid(
    plotlist = tests,
    labels = names(plots),
    ncol = 1,
    vjust = 5.5 * nrows + 1,
    rel_heights = nrows,
    scale = .8)


  # optional file save ---------------------------------------------------------

  ## .pdf ---------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "pdf") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi)
  }


  ## .png ---------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "png") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi)
  }


  ## .jpeg --------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "peg") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi)
  }


  # return ---------------------------------------------------------------------

  return(grd)

}

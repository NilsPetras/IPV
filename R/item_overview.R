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
#' @param color character; vector of hex codes for colors; defaults to the
#'   colors "#DAD8D8" (gray), "#11C1FF" (light blue), and "#007AD6" (blue)
#' @param font character; font of the plot labels; defaults to "mono"
#'
#' @return gg / ggplot object; plot grid with one bar plot per item showing
#'   (squared) factor loadings of that item in all IPV models, arranged by
#'   facets and tests
#'
#' @details File output produces much more reliable results than display within
#'   R. Display within R may scatter elements of the chart.
#'
#'
#' @export
#'
#' @examples
#' # Honesty/Humility and Agreeableness items
#' # the use of file output is recommended
#' # to prevent irregular placement of plot labels
#' res <- ipv_est(HEXACO[ ,c(2:41, 122:161)], "HA")
#' item_overview(res$est_raw)
#'
#'
item_overview <- function(
  data,
  squared = TRUE,
  file_name = "none",
  dpi = 500,
  color = NULL,
  font = "mono") {



  # helper variables -----------------------------------------------------------

  nested <- !is.null(data$global)
  if (is.null(color)) {
    if (nested) {color <- c("#DAD8D8", "#11C1FF", "#007AD6")}
    else {color <- c("#11C1FF", "#007AD6")}
  }


  # data preparation -----------------------------------------------------------

  # nested case (multiple tests) ----

  if (nested) {
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
    temp[!is.na(temp)] <- lapply(temp[!is.na(temp)], function(x){ # rename items uniquely
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

    # squared loadings are variance portions (as in formula of center distances)
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

  # simple case (one test) ----------

  } else {

    # collect factor loadings
    loads <- data$fls
    names(loads) <- c(
      "test",
      "facet",
      "item",
      "test_loading",
      "facet_loading")

    # set loadings < 0 to NA and throw warning if so
    loads[ ,4:5][apply(loads[,4:5], 2, function(x) x < 0)] <- NA
    if (any(is.na(loads))) {
      warning(paste(
        sum(is.na(loads)),
        " negative factor loadings set to 0 for display.",
        sep = ""))
    }
    loads[is.na(loads)] <- 0

    # squared loadings are variance portions (as in formula of center distances)
    if (squared) {
      loads[ ,4:5] <- apply(loads[ ,4:5], 2, function(x){
        x <- x ^ 2
      })
    }

    # factor loadings in long format
    long <- reshape2::melt(loads, id.vars = 3, measure.vars = 4:5)
    long$variable <- as.character(long$variable)
    long$variable[long$variable == "test_loading"] <-
      as.character(loads$test)
    long$variable[long$variable == "facet_loading"] <-
      as.character(loads$facet)
    long$variable <- factor(
      long$variable,
      levels = unique(long$variable)) # conserves level order

    long$test <- rep(
      long[1 : (1 / 2 * nrow(long)), "variable"],
      2)

    # hierarchical data structure as list to sort plots
    chunks <- long[(1 / 2 * nrow(long) + 1) : nrow(long), ]

    # code below as in nested for convenience
    chunks <- split(
      chunks,
      f = chunks[ ,"test"],
      drop = T)

    for (i in 1:length(chunks)) {
      chunks[[i]] <- split(chunks[[i]], f = droplevels(chunks[[i]][ ,"variable"]))
      chunks[[i]] <- lapply(chunks[[i]], '[[', 1)
    }
  }


  # plot creation --------------------------------------------------------------

  plots <- list()

    ## individual item plot for each item

  for (i in names(chunks)) { # i = test, only one in simple case
    for (j in names(chunks[[i]])) { # j = facet
      # for loop doesn't work properly with ggplot, so lapply instead
      # see: https://stackoverflow.com/questions/31993704/
      plots[[i]][[j]] <- lapply(
        as.list(chunks[[i]][[j]]),
        function(x) {
          p <- ggplot2::ggplot(
            long[which(long$item == x), ]) +

            # initialize
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
                x = long[which(long$item == x), ]$variable,
                y = long[which(long$item == x), ]$value),
              width = .99,
              fill = color)

          # one y-axis text per row looks cleaner
          if (which(chunks[[i]][[j]] == x) > 1) {
            p <-  p +
              ggplot2::theme(
                axis.text.y = ggplot2::element_blank())
          }
          return(p)
        }
      )
      names(plots[[i]][[j]]) <- chunks[[i]][[j]]
    }
  }

    ## plot grid --------------------

  nrows <- unlist(lapply(plots, length))
  width <- max(unlist(lapply(plots, function(x) lapply(x, length))))
  facets <- chunks
  tests <- chunks

  for (i in 1:length(chunks)) {
    for (j in 1:length(chunks[[i]])) {
      if(nested) {
        labels <- unlist(lapply(strsplit(names(plots[[i]][[j]]), split = "\\."), "[[", 2))
      } else {
        labels <- names(plots[[i]][[j]])
      }
      # adjust labels based on string length to center in plot
      hjust <- -8 / nchar(labels)

  # plot grid of item plots for each facet
      facets[[i]][[j]] <- cowplot::plot_grid(
        plotlist = plots[[i]][[j]][1:width], # adds empty plots at the rear
        labels = labels,
        hjust = hjust,
        vjust = 3,
        nrow = 1,
        label_size = 10,
        label_fontfamily = font)
    }

  # plot grid of facet plot grids for each test
    tests[[i]] <- cowplot::plot_grid(
      plotlist = facets[[i]],
      labels = names(plots[[i]]),
      ncol = 1,
      vjust = -.5,
      label_fontfamily = font)
  }

  if(!is.null(data$global)) {
    # overall plot grid of test plot grids
    grd <- cowplot::plot_grid(
      plotlist = tests,
      labels = names(plots),
      ncol = 1,
      vjust = 5.5 * nrows + 1,
      rel_heights = nrows,
      scale = .8,
      label_fontfamily = font) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(l = 1.5, unit = "cm")
      )
  } else {
    grd <- tests[[1]] +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 1.5, r = 1.5, l = 1.5, unit = "cm")
      )
  }


  # optional file save ---------------------------------------------------------

  ## .pdf ---------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "pdf") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi,
                    limitsize = FALSE)
  }


  ## .png ---------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "png") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi,
                    limitsize = FALSE)
  }


  ## .jpeg --------------------------

  if (substring(file_name, nchar(file_name) - 3 + 1) == "peg") {
    ggplot2::ggsave(file_name,
                    grd,
                    width = 2 * width, # * size,
                    height = 2 * (sum(nrows) + length(nrows) - 1), # * size,
                    units = "in",
                    dpi = dpi,
                    limitsize = FALSE)
  }


  # return ---------------------------------------------------------------------

  return(grd)

}

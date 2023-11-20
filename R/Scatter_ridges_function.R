#' Scatter Plot Combine with Ridgelines
#'
#' @description `ggScatRidges` is a simple function combining a scatter plot generated in 'ggplot2' to a ridgeline plot from 'ggridges' to visualise the disparities of the data points. This helps visualising the distribution of different groups in the data.
#'
#' @param x As input data. If a dataframe was provided, the dataframe should contain no less than three columns. If no dataframe was supplied, a x vector should be set as an input. The vector should #' contain numerical values.
#' @param y As input data. If no dataframe was provided, a y vector should be set as an input along with a x vector. The vector should contain numerical values.
#' @param xlab To give a title for the xlab can be given here.
#' @param ylab To give a title for the ylab can be given here.
#' @param title To give a title for the plot can be given here.
#' @param xlim To set scale limits on the xaxis.
#' @param ylim To set scale limits on the yaxis.
#' @param group The user should provide here the grouping of the rows if a dataframe was provided, otherwise a vector.
#' @param color The user can choose from `ggpubr::get_palette`. Default = "lancet".
#' @param ridges The user can choose to plot, or not, the ridgelines. Default = TRUE.
#' @param size The overall size of the text in the plot. Default = 15.
#' @param draw If the user wants to directly draw the plot. Default = TRUE.
#'
#' @return A ggplot object if draw set to 'TRUE' otherwise a grob table is returned but set to invisible.
#'
#' @examples
#' # The following example is based on the iris dataset:
#'
#' ## Example 1
#' ggScatRidges(x = iris$Sepal.Length, y = iris$Sepal.Width, group= iris$Species,
#'              color = "lancet", ridges = T, title = "plot iris",
#'              xlab = "Sepal.Length", ylab = "Sepal.Width", size = 15, draw = T)
#'
#' ## Example 2
#' iris2 <- iris[,c(1,2,5)] ## The first column will be used as 'x', the second as 'y' and the third as group for plotting.
#' ggScatRidges(x = iris2, 
#'              color = "lancet", ridges = T, title = "plot iris",
#'              xlab = "Sepal.Length", ylab = "Sepal.Width", size = 15, draw = T) 
#'
#' @import ggplot2
#' @import ggpubr
#' @importFrom cowplot axis_canvas insert_xaxis_grob insert_yaxis_grob ggdraw
#' @import ggridges
#' @import viridis
#' @import hrbrthemes
#'
#' @export


ggScatRidges <- function(x,
                         y = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         title = NULL,
                         xlim = NULL,
                         ylim = NULL,
                         group = NULL,
                         color = "lancet",
                         ridges = TRUE,
                         size = 15,
                         draw = TRUE){
  

  if(!inherits(x, "data.frame")){
    # then your x is vector and you need to get y
    if(is.null(y)){
      stop("Please provide 'y'")
    }
    if(is.null(group)){
      stop("Please provide 'group'")
    }
    if(!inherits(x, c("double", "numeric"))){
      stop("The 'x' vector to be plotted should only contain numeric values")
    }
    if(!inherits(y, c("double", "numeric"))){
      stop("The 'y' vector to be plotted should only contain numeric values")
    }
    }else if(ncol(x) != 3L){
      stop("if 'x' is a tabular data, it should have three columns, first will be used as 'x' axis and the second column will be used as the 'y' axis. The third column will be used for grouping.")
    }else{
      y <-     x[[2]]
      group <- x[[3]]
      x <-     x[[1]]
    }
  
    if(!inherits(x[[1]], c("double", "numeric"))){
      stop("The 'x' column to be plotted should only contain numeric values")
    }
    if(!inherits(y, c("double", "numeric"))){
      stop("The 'y' column to be plotted should only contain numeric values")
    }
    if(length(x[[3]]) != length(x[[1]])){
      stop("The value of the 'group' argument should have the same length as value of 'x' argument.")
    }
    if(length(x[[1]]) != length(x[[2]])){
      stop("The value of the 'x' argument should have the same length as value of 'y' argument.")
    }
  
# Scatter plot ------------------------------------------------------------
  if(ridges){
    main_plot <- ggplot(mapping = aes(x = x, y = y, col = group)) +
      geom_point() +
      xlab(xlab) +
      ylab(ylab) +
      theme_minimal(base_size = size) +
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
      ggpubr::color_palette(color) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      ggtitle(title) +
      geom_density2d()

# Add x axis ridges -------------------------------------------------------
    xridges <- suppressMessages({
      axis_canvas(main_plot, axis = "x") +
      geom_density_ridges(mapping = aes(x = x, y = group, fill = group),
                          alpha = 0.7, size = 0.2) +
      ggpubr::fill_palette(color) +
      ggplot2::scale_y_discrete(expand = c(0, 0))
      })

# Add x axis ridges -------------------------------------------------------
    yridges <- suppressMessages({
      axis_canvas(main_plot, axis = "y", coord_flip = TRUE)+
      geom_density_ridges(mapping = aes(x = y, y = group, fill = group),
                          alpha = 0.7, size = 0.2) +
      ggpubr::fill_palette(color) +
      scale_y_discrete(expand = c(0, 0)) +
      coord_flip()
    })

# Combine all plots -------------------------------------------------------
    final <- cowplot::insert_xaxis_grob(plot = main_plot, grob = xridges, height = grid::unit(.2, "null"), position = "top") |>
      cowplot::insert_yaxis_grob(grob = yridges, width = grid::unit(.2, "null"), position = "right")

# Scatter plot only if Ridges = F -----------------------------------------
  }else{
    final <- ggplot(mapping = aes(x = x, y = y, col = group)) +
      geom_point() +
      theme_minimal(base_size = size) +
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
      ggpubr::color_palette(color) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      ggtitle(title) +
      geom_density2d()
  }

# Return objects ----------------------------------------------------------
  if(draw){
    ggdraw(final)
  }else{
    return(invisible(final))
  }
}

#' Scatter plot combine with ridge lines
#'
#' @param DF The input data. A dataframe can be used for this function, the dataframe should contain two columns used as input data and an other column describing the grouping.
#' @param X The input data. If a dataframe was provided, the user can indicate here  If no dataframe was supplied, a X vector should be set as an input. The vector should contain numerical values.
#' @param Y The input data. If no dataframe was supplied, a Y vector should be set as an input along with a X vector. The vector should contain numerical values.
#' @param Y The input data. If no dataframe was supplied, a Y vector should be set as an input along with a X vector. The vector should contain numerical values.
#' @param xlab. A title for the xlab can be given here.
#' @param ylab. A title for the ylab can be given here.
#' @param title. A title for the plot can be given here.
#' @param xlim. Set scale limits on the xaxis. 
#' @param ylim. Set scale limits on the yaxis.
#' @param group
#' @param color The user can choose from 
#' @param ridges. The user can choose to plot or not the ridgelines. Default = FALSE.
#' @param size. The overall size of the text in the plot.
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @import ggridges
#' @import viridis
#' @import hrbrthemes
#' @export 


library("ggplot2")
library("cowplot")
library("ggpubr")
library("ggridges")
library("viridis")
library("hrbrthemes")

# Main plot
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
                         size = NULL,
                         draw = TRUE){
  

# Check user input --------------------------------------------------------
  if(!inherits(x, c("data.frame", "matrix"))){
    # then your x is vector and you need to get y
    if(is.null(y)){
      stop("provide y")
    }
    if(!inherits(x, c("double", "numeric"))){
      stop("The x vector to be plotted should contain only numeric values")
    }
    if(!inherits(y, c("double", "numeric"))){
      stop("The y vector to be plotted should contain only numeric values")
    }
    }else if(ncol(x) != 2){
      stop("if 'x' is a tabular data, it should have two columns, first will be used as x axis and second columns will be used as the y axis.")
    }else{
      y <- x[[2]]
      x <- x[[1]]
      group <- x[[3]]
    }
  
    if(!inherits(x[[1]], c("double", "numeric"))){
      stop("The x column to be plotted should contain only numeric values")
    }
    if(!inherits(y, c("double", "numeric"))){
      stop("The y column to be plotted should contain only numeric values")
    }
    if(length(x[[3]]) != length(x[[1]])){
      stop("The value of the 'group' argument should have the same length as value of 'x' argument.")
    }
    if(length(x[[1]]) != length(x[[2]])){
      stop("The value of the 'x' argument should have the same length as value of 'y' argument.")
    }
  
  ### main 
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
    
    xridges <- suppressMessages({
      axis_canvas(main_plot, axis = "x") +
      geom_density_ridges(mapping = aes(x = x, y = group, fill = group),
                          alpha = 0.7, size = 0.2) +
      ggpubr::fill_palette(color) +
      ggplot2::scale_y_discrete(expand = c(0, 0))
      })
    
    yridges <- suppressMessages({
      axis_canvas(main_plot, axis = "y", coord_flip = TRUE)+
      geom_density_ridges(mapping = aes(x = y, y = group, fill = group),
                          alpha = 0.7, size = 0.2) +
      scale_y_discrete(expand = c(0, 0)) +
      coord_flip() +
      ggpubr::fill_palette(color)
    })
    
    p1 <- cowplot::insert_xaxis_grob(plot = main_plot, grob = xridges, height = grid::unit(.2, "null"), position = "top")
    final <- cowplot::insert_yaxis_grob(plot = p1, grob = yridges, width = grid::unit(.2, "null"), position = "right")

  } else {
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
  
  if(draw){
    ggdraw(final)
  }else{
    return(invisible(final))
  }
}








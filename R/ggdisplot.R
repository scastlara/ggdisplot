#' Plot scatterplot of two variables along with two density plots.
#'
#' @param df dataframe to work with.
#' @param var1 x variable to plot (string).
#' @param var2 y variable to plot (string).
#' @param color factor variable for ggplot (string).
#' @param xlim limits for x variable eg: c(1, 10).
#' @param ylim limits for y variable eg: c(1, 10).
#' @param xlab label for x variable.
#' @param ylab label for y variable.
#' @param ... additional ggplot parameters.
#' @return plot as returned by cowplot::plot_grid()
#' @export
#' @examples
#' ggdisplot(iris, "Sepal.Length", "Sepal.Width", "Species")
#' ggdisplot(iris, "Sepal.Length", "Sepal.Width", "Species", xlim=c(1,10), ylim=c(1,10))
#' ggdisplot(iris, "Sepal.Length", "Sepal.Width", "Species", theme_bw())


ggdisplot <- function(df, var1, var2, color, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, relative_space = 0.2, ...) {
	library(ggplot2)
	library(cowplot)
	scatt <- ggplot(df, aes_string(x=var1, y=var2, color=color)) + 
		  geom_point() + 
		  theme_light() + 
		  theme(legend.position="none");
	ds_top <- ggplot(df, aes_string(x=var1, fill=color)) + 
		  geom_density(alpha=0.6, color="transparent") + 
		  theme_light() + 
		  xlab("") +
	    theme(axis.line.x = element_blank(),
	          axis.text.x = element_blank(),
	          axis.title.x = element_blank(),
	          axis.ticks.x = element_blank());
	ds_bottom <- ggplot(df, aes_string(x=var2, fill=color)) + 
		  geom_density(alpha=0.6, color="transparent") + 
		  theme_light() + 
		  coord_flip() + 
		  theme(legend.position="none") + 
		  xlab("") + 
	  theme(axis.line.y = element_blank(),
	        axis.text.y = element_blank(),
	        axis.title.y = element_blank(),
	        axis.ticks.y = element_blank());
	legend <- get_legend(ds_top)
	ds_top <- ds_top + theme(legend.position="none");

	# X + Y limits
	if (!is.null(xlim)) {
		ds_top <- ds_top + xlim(xlim);
		scatt  <- scatt + xlim(xlim);
	}
	if (!is.null(ylim)) {
		ds_bottom <- ds_bottom + xlim(ylim);
		scatt     <- scatt + ylim(ylim);
	}

	# X + Y labels
	if (!is.null(xlab)) {
		scatt  <- scatt + xlab(xlab);
	}
	if (!is.null(ylab)) {
		scatt     <- scatt + ylab(ylab);
	}

	# Additional parameters
	for (var in list(...)) {
		scatt     <- scatt + var;
		ds_top    <- ds_top + var;
		ds_bottom <- ds_bottom + var;
	}
	displot <- plot_grid(ds_top,
	                     legend,
	                     scatt,
	                     ds_bottom,
	                     ncol=2,
	                     axis = "left",
	                     align = "hv",
	                     rel_widths = c(1-relative_space,relative_space),
	                     rel_heights = c(relative_space,1-relative_space))
	return(displot)
}


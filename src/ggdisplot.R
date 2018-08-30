library(ggplot2)
library(cowplot)

ggdisplot <- function(df, var1, var2, color) {
	p1 <- ggplot(df, aes_string(x=var1, y=var2, color=color)) + 
		  geom_point() + 
		  theme_light() + 
		  theme(legend.position="none");
	p2 <- ggplot(df, aes_string(x=var1, fill=color)) + 
		  geom_density(alpha=0.6, color="transparent") + 
		  theme_light() + 
		  xlab("");
	p3 <- ggplot(df, aes_string(x=var2, fill=color)) + 
		  geom_density(alpha=0.6, color="transparent") + 
		  theme_light() + 
		  coord_flip() + 
		  theme(legend.position="none") + 
		  xlab("");
	legend <- get_legend(p2)
	p2 <- p2 + theme(legend.position="none");
	displot <- plot_grid(p2, legend, p1, p3, ncol=2)
	return(displot)
}


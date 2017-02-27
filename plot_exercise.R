source("Iteration.R")

plot_host <- function(init_host_matrix, antimicrobe_host_matrix, final_host_matrix, j)
	{
	my_margins <- c(1,3)
	par(mfrow=my_margins,oma=c(0,0,0,0),pty="s")
	lbls <- paste("Microbe", 1:5)
	pie(init_host_matrix[,j], labels=lbls, col=colorplot, main=paste("Host",j,"before treatment"))
	pie(antimicrobe_host_matrix[,j], labels=lbls, col=colorplot, main=paste("Host",j,"during treatment"))
	pie(final_host_matrix[,j], labels=lbls, col=colorplot, main=paste("Host",j,"after treatment"))
	}

plot_exercise <- function(init_host_matrix, antimicrobe_host_matrix, final_host_matrix)
	{
	for (j in 1:ncol(init_host_matrix))
		{
		if (j > 1)
			dev.new()
		plot_host(init_host_matrix, antimicrobe_host_matrix, final_host_matrix, j)
		}
	}

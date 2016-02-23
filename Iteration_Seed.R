source("Iteration.R")

COLORS <- c("GREEN", "ORANGE", "YELLOW", "RED", "WHITE") 

simulate_drift_with_new_seed <- function(num_its, seed=F)
	{
	if(seed)
		{
		set.seed(seed)
		}
	new_colors <- COLORS
	cat("First generation:", rep(new_colors, each=new_population_size/number_of_colors), "\n")
	# Store the output of the simulation:
	result <- matrix(nrow=num_its,ncol=new_population_size)
	for (i in 1:num_its)
		{
		otcm <- sample_3(rep(new_colors,each=new_population_size/number_of_colors), round(runif(1,0,1e6)), print_result=F)
		new_colors <- rep(otcm, each=new_population_size/length(COLORS))
		result[i,] <- new_colors
		cat("New generation:", new_colors,"\n")
		}

	color_counts <- matrix(nrow=num_its, ncol=number_of_colors)
	for (i in 1:num_its)
		{
		for (j in 1:number_of_colors)
			{
			color_counts[i,j] <- sum(result[i,]==COLORS[j])
			}
		}

	viable_colors <- COLORS[which(color_counts[1,]>0)]
	plotcols <- colorplot[which(color_counts[1,]>0)]

	plot(color_counts[,which(color_counts[1,]>0)[1]]/new_population_size,xlab="Generation #", ylab="Frequency of randomly chosen colors",ylim=c(0,1),main="",xlim=c(0,num_its),col=plotcols[1])
	par(xpd=TRUE)
	legend(num_its/2, 1.21, viable_colors,lwd=c(2,2,2),pch=c(1,19,8),lty=c(1,2,3), seg.len=7.5, col=plotcols)
	lines(color_counts[,which(color_counts[1,]>0)[1]]/new_population_size,lwd=2,col=plotcols[1])

	points(color_counts[,which(color_counts[1,]>0)[2]]/new_population_size,lwd=2,pch=19,col=plotcols[2])
	lines(color_counts[,which(color_counts[1,]>0)[2]]/new_population_size,lwd=2,lty=2, col=plotcols[2])
	if (length(viable_colors) > 2)
		{
		points(color_counts[,which(color_counts[1,]>0)[3]]/new_population_size,lwd=2,pch=8,col=plotcols[3])
		lines(color_counts[,which(color_counts[1,]>0)[3]]/new_population_size,lwd=2,lty=3, col=plotcols[3])
		}
	}

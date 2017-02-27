# Simulate microbiome dynamics after antimicrobial perturbation. The inputs are the number of hosts, the starting proportion of bugs, and the output is a series of plots about the effects of the antmicrobial community dynamics.

COLORS <- c("GREEN", "ORANGE", "YELLOW", "RED", "WHITE")
colorplot <- c("green", "orange", "yellow", "red", "grey")

simulate_setup <- function(N_hosts, num_bugs, starting_proportion)
	{
	# Make proportions into true probabilities so they sum to one:
	starting_proportion <- starting_proportion/sum(starting_proportion) 
	return(rmultinom(n=N_hosts, size=num_bugs, prob=starting_proportion))
	}

kill_bugs <- function(bug_densities, bug_susceptibilities)
	{
	surviving_bug_densities <- numeric()
	for (i in 1:length(bug_densities))
		{
		surviving_bug_densities[i] <- rbinom(1, bug_densities[i], bug_susceptibilities[i])
		}
	return(surviving_bug_densities)
	}

simulate_antimicrobial <- function(host_matrix, bug_susceptibilities)
	{
	ans_matrix <- host_matrix
	if (length(bug_susceptibilities) != nrow(ans_matrix))
		{
		print("You have different numbers of susceptibilities and microbe types!")
		return(0)
		}
	for (j in 1:ncol(host_matrix))
		{
		ans_matrix[,j] <- kill_bugs(ans_matrix[,j], bug_susceptibilities)
		}
	return(ans_matrix)
	}

#repopulate_bugs <- function(bug_densities, bug_growth_rates, num_bugs, starting_proportion)
repopulate_bugs <- function(bug_densities, bug_growth_rates, num_bugs)
	{
	# For now, assume that the fastest growing bug crowds out the other bugs, rather than direct competition effects (e.g., something like bacterial toxins where bug A kills bug B cells); these could potentially be incorporated into an alternative repopulate_bugs function
	survivors <- ifelse(bug_densities > 0, 1, 0)
#	if (sum(survivors)==0)
#		{
#		# If microbiome is extinct, recover stochastically to initial densities; skip this step to have the recolonization occur in proportion to the relative growth rates
#		return(rmultinom(1, num_bugs, starting_proportion))
#		}
#	else
#		{
		# Assume that the relative proportion of new bugs is proportional to their relative recovery rates
		new_bug_recovery_proportions <- (survivors*bug_growth_rates)/sum(survivors*bug_growth_rates)
		bug_densities <- bug_densities + rmultinom(1, size = num_bugs-sum(bug_densities),  new_bug_recovery_proportions)
#		}
	return(bug_densities)
	}

simulate_recolonization <- function(host_matrix,  bug_growth_rates, num_bugs)
	{
	ans_matrix <- host_matrix
	for (j in 1:ncol(ans_matrix))
		{
		ans_matrix[,j] <- repopulate_bugs(ans_matrix[,j], bug_growth_rates, num_bugs)
		}
	return(ans_matrix)
	}

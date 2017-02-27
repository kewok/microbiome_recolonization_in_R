source("plot_exercise.R")

N_hosts <- 10
num_bugs <- 25
init_proportions <- c(0.1, 0.2, 0.1, 0.3, 0.95) # note this doesn't have to add up to one.
bug_susceptibilities <- c(0.01, 0.9, 0.8, 0.5, 0.1) # how likely the antibiotic is to work against a given bug
bug_R0 <- c(520, 100, 390, 10, 1) # the microbial growth rates, arbitrary units

initial_microbiomes <- simulate_setup(N_hosts, num_bugs, init_proportions)
microbiomes_after_antibiotic <- simulate_antimicrobial(initial_microbiomes, bug_susceptibilities)
final_microbiomes <- simulate_recolonization(microbiomes_after_antibiotic, bug_R0, num_bugs)

plot_exercise(initial_microbiomes, microbiomes_after_antibiotic, final_microbiomes)

# To get rid of plots:
graphics.off()



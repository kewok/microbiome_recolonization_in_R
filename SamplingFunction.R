# A function that takes your input (value) and prints the input to the console
print_input <- function(value)
	{
	options(warn=-1) # suppress warnings in case value is not equal to length LETTERS
	if (value==length(LETTERS))
		{
		cat("LETTERS =", LETTERS,"\n")
		}
	else
		{
		cat(value,"\n")
		}
	}

# A function that samples 3 values from your input; the default seed is set to 5.
sample_3 <- function(input, seed=1, print_result=T)
	{
	set.seed(seed)
	if (print_result)
		print(sample(input, 3))
	else
		return(sample(input,3))
	}





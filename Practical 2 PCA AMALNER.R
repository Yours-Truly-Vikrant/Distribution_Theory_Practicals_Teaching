#1
#1. Generate random samples from the finite population using:
#(a) Simple Random Sampling With Replacement (SRSWR).
#(b) Simple Random Sampling Without Replacement (SRSWOR).
#(c) Sampling with Unequal Probabilities for Population Elements.



set.seed(123)
random_numbers=sample(1:50,size=20,replace=TRUE)
random_numbers

set.seed(123)
rn=sample(1:50,size=10,replace=FALSE)
rn



prob=runif(10,min=0,max=1)
samp=sample(rn,5,replace=TRUE,prob=prob)
samp

#Q.2
# Generate a random sample of size 10,000 from Poisson(λ = 3). Treat this generated sample data as characteristic values of study variables for a population of size 10,000. Now, draw 5 random samples of size 100 each from this population using SRSWOR. Obtain the estimate of the population mean based on each sample data using the sample mean as an estimator for the population mean. Also, find the amount of sampling error from your estimates.
set.seed(1)
population=rpois(1000,lambda = 3)
size=100
num_sample=5
sample_mean=numeric(num_sample)


for(i in 1:num_sample){
  sample=sample(population,size=size,replace=FALSE)
  sample_mean[i]=mean(sample)
  
}
print(sample_mean)

population_mean=mean(population)
population_mean


for(i in 1:5){
  sampling_error=sample_mean-population_mean
}
print(sampling_error)



#Q.3
#Draw 10,000 samples of size 25 from N(100, 9) and study the sampling distribution of the following statistics for n = 5, 10, 25:
#i)Ybar (Sample Mean).
#(b)(Sample Variance).
#(c) min(Y1, Y2, . . . , Yn) (Minimum).
#(d) max(Y1, Y2, . . . , Yn) (Maximum).
#(e) R = Y(n) − Y(1) (Range).

set.seed(123)  # For reproducibility

# Parameters
n_samples = 10000
sample_size = 25
mean_population = 100
sd_population = sqrt(9)  # Standard deviation is sqrt(variance), so sqrt(9) = 3

# Generate samples
samples = matrix(rnorm(n_samples * sample_size, mean = mean_population, sd = sd_population), 
                 nrow = n_samples, ncol = sample_size)

# Initialize vectors to store statistics
sample_means = numeric(n_samples)
sample_variances = numeric(n_samples)
sample_mins = numeric(n_samples)
sample_maxs = numeric(n_samples)
sample_ranges = numeric(n_samples)

# Compute statistics for each sample
for (i in 1:n_samples) {
  sample = samples[i, ]
  sample_means[i] = mean(sample)
  sample_variances[i] = var(sample)
  sample_mins[i] = min(sample)
  sample_maxs[i] = max(sample)
  sample_ranges[i] = sample_maxs[i] - sample_mins[i]
}

hist(sample_means)
qqnorm(sample_means)



hist(sample_variances,breaks=100)
qqnorm(sample_variances)


hist(sample_mins)
qqnorm(sample_mins)


hist(sample_maxs)
qqnorm(sample_maxs)



hist(sample_ranges)
      
qqnorm(sample_ranges)



#Q.4
#Consider a population of size N=5, with 'y' values as (138,142,145,155,143). Write down all possible samples of size n=3 using SRSWOR. For each sample, obtain the values of :
#a) T_1=Ybar(sample mean)
#b) T_2=(Y1+Y2)/2
#c) T_3= Sample Median

# Define the population
population = c(138, 142, 145, 155, 143)

# Generate all possible samples of size 3 without replacement
#install.packages("combinat")
library(combinat)  # For combinations
samples = combn(population, 3, simplify = FALSE)

# Initialize vectors to store the statistics
sample_means = numeric(length(samples))
mean_first_two = numeric(length(samples))
sample_medians = numeric(length(samples))

# Calculate statistics for each sample
for (i in 1:length(samples)) {
  sample = samples[[i]]
  sample_means[i] = mean(sample)
  mean_first_two[i] = mean(sample[1:2])
  sample_medians[i] = median(sample)
}

# Combine results into a data frame for easy viewing
results = data.frame(
  Sample = I(samples),
  Sample_Mean = sample_means,
  Mean_First_Two = mean_first_two,
  Sample_Median = sample_medians
)

# Print the results
print(results)

mean(sample_means)
mean(population)
mean(mean_first_two)
mean(c(138,142))
mean(sample_medians)
median(population)










Q.1# We wish to compare two different types of eye drops A & B that are intended to prevent redness in people with hay fever. we find that for randomly selected people with an equal amount of redness in each eye at basline. after 10 minutes the drug A eye is less red than the drug B eye for 2 people. The drug B eye is less red than the drug A eye for 8 people, and the eyes are eually red for 5 people. Assess which eye drop performance is better using the appropriate nonparametric test at 5% significance level. 

binom.test(8, 10, alternative="greater")


Q.2#
table = matrix(c(16,160,784,40),nrow=2, ncol=2)
chisq.test(table, correct=FALSE)

Q.3#: Perform a nonparametric test and resample base test between the variable Weight and Diet, assuming that chicken weights are not normally distributed.

data(ChickWeight)
chick_data <- ChickWeight[ChickWeight$Diet %in% c(1, 2), ]
wilcox.test(weight ~ Diet, data = chick_data)

install.packages("nptest")
library(nptest)
x = ChickWeight$weight
y = ChickWeight$Diet
np.loc.test(x, y=NULL, mu = 4, alternative = "greater", median.test = TRUE)



Q.4#: The following data records Richter Magnitude of 10 randomly selected earthquack in the country x. Richter Magnitude: 4.3, 6.1, 5.5,3.2,5.0,3.8,4.1,3.9,6.5
Find the bootstrap mean and standard error. Also find the bootstrap 95% percentile confidence interval. what conclusion can you draw about the average richter magnitude of the country X.


x <- c(4.3, 6.1, 5.5, 3.2, 5.0, 3.8, 4.1, 3.9, 6.5)
bootstrap_mean <- function(data, n_boot) {
  means <- replicate(n_boot, mean(sample(data, replace = TRUE)))
  return(means)
}
set.seed(100)
bootstrap_means <- bootstrap_mean(x, 10000)
bootstrap_se <- sd(bootstrap_means)
bootstrap_ci <- quantile(bootstrap_means, c(0.025, 0.975))
cat("Bootstrap mean: ", mean(bootstrap_means), "\n")
cat("Bootstrap standard error: ", bootstrap_se, "\n")
cat("Bootstrap 95% confidence interval: [", bootstrap_ci[1], ", ", bootstrap_ci[2], "]\n")
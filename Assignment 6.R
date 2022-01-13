# Assignment 6

rm(list=ls())
cat("\014")
graphics.off()

setwd("~/Desktop/bayesian_assignments/assignment_6")

#
# define data
#

# reaction times (msec):
y = c(607, 583, 521, 494, 369, 782, 570, 678, 467, 620, 425, 395, 346, 361, 310, 300, 382, 294, 315, 250, 320, 335, 297, 315, 316, 319, 326, 280, 330, 272, 317, 311, 336, 308, 330, 311, 304, 327, 367, 414, 411, 391, 333, 425, 416, 379, 368, 284, 260, 294, 265, 275, 270, 270, 282, 281, 283, 307, 344, 318, 326, 308, 306, 294, 342, 323, 325, 402, 219, 285, 277, 283, 271, 280, 289, 288, 199, 267, 354, 234, 270, 320, 214, 252, 234, 223, 332, 268, 286, 292, 295, 292, 275, 300, 256, 282, 319, 288, 316, 265, 306, 347, 374, 328, 305, 327, 344, 275, 218, 263, 282, 386, 307, 267, 282, 314, 328, 332, 386, 462, 368, 354, 283, 335, 264, 304, 248, 239, 288, 239, 249, 272, 289, 274, 281, 232, 279, 308, 260, 309, 312, 307, 296, 280, 331, 298, 342, 297, 297, 305, 308, 510, 490, 458, 425, 522, 927, 555, 550, 516, 548, 560, 545, 633, 496, 498, 555, 387, 317, 365, 357, 390, 320, 316, 297, 354, 266, 279, 327)

data <- list(
  J = 179, # number of reaction time samples
  child_c = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1), # child or adult
  P = 23,  # number of participants
  y = log(y), # to be able to use normal.
  # participant ID by sample
  g = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23)
)

library(rstan)

# model 5
fit5 <- stan(
  file="assign5_v3.stan",
  data=data,
  chains=4,
  warmup=5000,
  iter=10000,
  cores=6,
  refresh=1,
  control = list(adapt_delta = 0.8, max_treedepth = 15)
)


# model 6
fit6 <- stan(
  file="assign6.stan",
  data=data,
  chains=4,
  warmup=5000,
  iter=10000,
  cores=6,
  refresh=1,
  control = list(adapt_delta = 0.8, max_treedepth = 15)
)

#shinystan model 5
library(shinystan)
my_shinystan_5 <- as.shinystan(fit5)
launch_shinystan(my_shinystan_5)

#shinystan model 6
my_shinystan_6 <- as.shinystan(fit6)
launch_shinystan(my_shinystan_6)


#
# PLOT the histograms for child and adults (beta distributions)
#

samples_ss6 <- extract(fit6, permuted = TRUE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue") #alpha makes transparent
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(128,255,128, max = 255, alpha = 80, names = "lt.green")

h_adult = exp(samples_ss6$theta_predicted_adult)
h_child = exp(samples_ss6$theta_predicted_child)

histSampleAdult <- hist(h_adult, breaks=30, plot = FALSE) # Save histogram of log(y)
histSampleChild <- hist(h_child, breaks=50, plot = FALSE) # Save histogram of a sample of mu.
# ?plot.histogram
plot(histSampleAdult, freq = FALSE, col = c1, ylegend=NULL, xlim=c(100, 900), ylim=c(0,0.01), main="Posterior for Adults (blue), Children (salmon) and Together (green)", xlab="Reaction time (msec)") # Plot 1st histogram using a transparent color
plot(histSampleChild, freq = FALSE, col = c2, ytick=NULL, add = TRUE)     # Add 2nd histogram using different color

#
# PLOT difference between 5 and 6 model
#
samples_ss5 <- extract(fit5, permuted = TRUE)
h_together = exp(samples_ss5$beta_predicted)
histSampleTogether <- hist(h_together, breaks=60, plot = FALSE) # Save histogram of a sample of mu.
plot(histSampleTogether, freq = FALSE, col = c3, ytick=NULL, add = TRUE)     # Add 2nd histogram using different color

#Add lines for central tendency and CI for adults
abline(v = mean(h_adult), col = "blue", lwd = 2)
ci_95_adult <- quantile(h_adult, probs = c(0.025, 0.975))
abline(v=ci_95_adult, col ="blue", lwd = 1)

#Add lines for central tendency and CI for children
abline(v = mean(h_child), col = "red", lwd = 2)
ci_95_child <- quantile(h_child, probs = c(0.025, 0.975))
abline(v=ci_95_child, col ="red", lwd = 1)

#Add lines for central tendency and CI for children
abline(v = mean(h_together), col = "green", lwd = 2)
ci_95_together <- quantile(h_together, probs = c(0.025, 0.975))
abline(v=ci_95_together, col ="green", lwd = 1)

#Now plotting the priors
hist_prior_6 <- hist(exp(samples_ss6$theta), breaks=200, plot=FALSE)
hist_prior_5 <- hist(exp(samples_ss5$beta), breaks=200, plot=FALSE) #theta is confusingly called beta in my model 5.
plot(hist_prior_6, freq = FALSE, col = c1, ylegend=NULL, xlim=c(100, 900), main="Priors: blue=Assignment 6, pink=Assignment 5", xlab="Reaction time (msec)") 
plot(hist_prior_5, freq = FALSE, col = c2, ytick=NULL, add = TRUE)  

#Now plotting a child only posterior predictive distribution
hist_predict_child <- hist(exp(samples_ss6$logy_predicted_child), breaks=200, plot=FALSE)
plot(hist_predict_child, freq = FALSE, col = c1, ylegend=NULL, xlim=c(100, 900), main="Predicted child")

#Now plotting an adult only posterior predictive distribution
hist_predict_adult <- hist(exp(samples_ss6$logy_predicted_adult), breaks=200, plot=FALSE)
plot(hist_predict_adult, freq = FALSE, col = c2, ylegend=NULL, xlim=c(100, 900), main="Predicted adult")

#Randomly picking if it is a child or adult:
library(purrr)

personSample <- rbernoulli(1, p = 0.26) #TRUE is child and FALSE is adult.

#Allocating the sampled individual to either child or adult pool.
if (personSample=TRUE) {
  sample(exp(samples_ss6$logy_predicted_child), size  = 1) #sampling from the child pool.
} else {
  sample(exp(samples_ss6$logy_predicted_adult), size = 1) #sampling from the adult pool.
}

#
# Question: what is a good prior for a zero-inflated model?
#

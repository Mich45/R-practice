nhanes_data <- read.csv("nhanes_vit_d.csv")
head(nhanes_data)


mean_vit_d <- mean(nhanes_data$vit_d_lab)
mean_vit_d

num_of_sample <- nrow(nhanes_data)
num_of_sample

standard_d <- sd(nhanes_data$vit_d_lab)
standard_d

#t-alpha 95% CI: alpha = 0.05, alpha/2 = 0.0025

t_95 <- qt(1-0.025, num_of_sample)
t_95

# COmpute lower and upper CI bounds

ci_lower <- mean_vit_d - (t_95 * standard_d) / sqrt(num_of_sample)
ci_lower

ci_upper <- mean_vit_d + (t_95 * standard_d) / sqrt(num_of_sample)
ci_upper

# Using T test R function

t.test(nhanes_data$vit_d_lab)


#t-alpha 90% CI: alpha = 0.1, alpha/2 = 0.05

t_90 <- qt(1-0.05, num_of_sample)
t_90

# Compute lower and upper CI bounds

ci_lower_90 <- mean_vit_d - (t_90 * standard_d) / sqrt(num_of_sample)
ci_lower_90

ci_upper_90 <- mean_vit_d + (t_90 * standard_d) / sqrt(num_of_sample)
ci_upper_90

# T test with 90% confidence interval
t.test(nhanes_data$vit_d_lab, conf.level = 0.9)

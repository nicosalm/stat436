library(tidyverse)

data <- read.csv("data.csv", comment.char = "#", header = TRUE)

pre_1950 <- subset(data, Year < 1950)
mean_pre_1950 <- mean(abs(pre_1950$Anomaly))
sd_pre_1950 <- sd(pre_1950$Anomaly)
cv_pre_1950 <- sd_pre_1950 / mean_pre_1950

post_1990 <- subset(data, Year >= 1990)
mean_post_1990 <- mean(abs(post_1990$Anomaly))
sd_post_1990 <- sd(post_1990$Anomaly)
cv_post_1990 <- sd_post_1990 / mean_post_1990

# results
cat("Pre-1950 Period:\n")
cat("  Mean of absolute anomalies:", round(mean_pre_1950, 3), "\n")
cat("  Standard deviation:", round(sd_pre_1950, 3), "\n")
cat("  Coefficient of variation:", round(cv_pre_1950, 3), "\n\n")

cat("Post-1990 Period:\n")
cat("  Mean of absolute anomalies:", round(mean_post_1990, 3), "\n")
cat("  Standard deviation:", round(sd_post_1990, 3), "\n")
cat("  Coefficient of variation:", round(cv_post_1990, 3), "\n")

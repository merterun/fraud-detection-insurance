fraud <- read.csv("fraud_oracle.csv")

library(tidyverse)

head(fraud)

str(fraud)

summary(fraud)

dim(fraud)

# View unique values for each column
lapply(fraud, unique)

# Check for missing values
colSums(is.na(fraud))

# Calculate the total number of claims and frauds
total_claims <- nrow(fraud)
total_frauds <- sum(fraud$FraudFound_P)

# Bar plot of frauds vs total claims
totalclaims_fraud <- ggplot(fraud, aes(x = "Total Claims", fill = "Fraud")) +
  geom_bar(aes(y = total_claims), stat = "identity", width = 0.3, fill = "#CCDCB8") +
  geom_bar(aes(y = total_frauds), stat = "identity", width = 0.3, fill = "#CA1265") +
  scale_fill_manual(name = "Fraud") +
  ggtitle("Total Claims vs. Total Frauds") +
  labs(x = "", y = "Number of Claims") +
  theme(plot.title = element_text(size = 18))
totalclaims_fraud

# Pie chart of frauds vs non-frauds
fraud_pie <- ggplot(fraud, aes(x = "", fill = factor(FraudFound_P))) +
  geom_bar(width = 1, position = "fill") +
  coord_polar(theta = "y") +
  ggtitle("Fraudulent Claims Proportion") +
  scale_fill_manual(values = c("#FF8F00", "#CA1265")) +
  theme(plot.title = element_text(size = 18))

# Box plot of age of policy holder by fraud status
age_dist <- ggplot(fraud, aes(x = factor(FraudFound_P), y = AgeOfPolicyHolder, fill = factor(FraudFound_P))) +
  geom_boxplot() +
  ggtitle("Age of Policy Holder by Fraud Status") +
  labs(x = "Fraud Status", y = "Age of Policy Holder") +
  theme(plot.title = element_text(size = 18))

# Scatter plot of vehicle price vs age of vehicle
vehicle_age <- ggplot(fraud, aes(x = VehiclePrice, y = AgeOfVehicle)) +
  geom_point(aes(color = factor(FraudFound_P)), size = 2) +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle("Vehicle Price vs. Age of Vehicle") +
  labs(x = "Vehicle Price", y = "Age of Vehicle") +
  theme(plot.title = element_text(size = 18))

install.packages("gridExtra")
library(gridExtra)
grid.arrange(totalclaims_fraud, fraud_pie, age_dist, vehicle_age, ncol = 2)


# Fault vs FraudFound_P
fault_fraud <- ggplot(fraud, aes(x = Fault, fill = factor(FraudFound_P))) +
  geom_bar(position = "dodge") +
  xlab("Fault") +
  ylab("Count") +
  ggtitle("Fault vs FraudFound_P") +
  theme_bw()

# WitnessPresent vs FraudFound_P
witness_fraud <- ggplot(fraud, aes(x = WitnessPresent, fill = factor(FraudFound_P))) +
  geom_bar(position = "dodge") +
  xlab("WitnessPresent") +
  ylab("Count") +
  ggtitle("WitnessPresent vs FraudFound_P") +
  theme_bw()

# PastNumberOfClaims vs FraudFound_P
claims_fraud <- ggplot(fraud, aes(x = PastNumberOfClaims, fill = factor(FraudFound_P))) +
  geom_bar(position = "dodge") +
  xlab("PastNumberOfClaims") +
  ylab("Count") +
  ggtitle("PastNumberOfClaims vs FraudFound_P") +
  theme_bw()

# DriverRating vs FraudFound_P
rating_fraud <- ggplot(fraud, aes(x = DriverRating, fill = factor(FraudFound_P))) +
  geom_bar(position = "dodge") +
  xlab("DriverRating") +
  ylab("Count") +
  ggtitle("DriverRating vs FraudFound_P") +
  theme_bw()

grid.arrange(fault_fraud, witness_fraud, claims_fraud, rating_fraud, ncol = 2)


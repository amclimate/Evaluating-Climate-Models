# Evaluating-Climate-Models
Evaluating Climate Models
# Load necessary libraries
library(readr)     # For reading and writing CSV files
library(Metrics)   # For RMSE and other error metrics
library(hydroGOF)  # For NSE metric
library(ggplot2)   # For visualization

# Load the CSV file (Ensure the file is in your working directory)
data <- read_csv("temperature_data.csv")

# Extract Observed (historical) and Simulated (GCM) temperature data
obs <- data$Observed
sim <- data$Simulated

# Compute Sum of Squared Differences (SSD)
SSD <- sum((sim - obs)^2)

# Compute Mean Squared Difference (MSD)
MSD <- mean((sim - obs)^2)

# Compute Root Mean Squared Difference (RMSD)
RMSD <- sqrt(MSD)

# Compute Bias (Mean Bias Error)
Bias <- mean(sim - obs)

# Compute Correlation Coefficient (CC)
CC <- cor(sim, obs)

# Compute R² (Coefficient of Determination)
R2 <- CC^2

# Compute Normalized RMSD (nRMSD)
nRMSD <- RMSD / mean(obs)

# Compute Absolute Normalized Mean Bias Deviation (ANMBD)
ANMBD <- abs(Bias) / mean(obs)

# Compute Absolute Average Relative Deviation (AARD)
AARD <- mean(abs((sim - obs) / obs)) * 100  # Expressed as percentage

# Compute Nash-Sutcliffe Efficiency (NSE)
NSE <- NSE(sim, obs)

# Compute Skill Score (SS)
SS <- 1 - (sum((sim - obs)^2) / sum((obs - mean(obs))^2))

# Store results in a dataframe
results <- data.frame(
  SSD = SSD,
  MSD = MSD,
  RMSD = RMSD,
  Bias = Bias,
  CC = CC,
  R2 = R2,
  nRMSD = nRMSD,
  ANMBD = ANMBD,
  AARD = AARD,
  NSE = NSE,
  SS = SS
)

# Print results
print(results)

# Save results to a CSV file
write_csv(results, "performance_metrics.csv")  # Using readr package

# OR using base R:
write.csv(results, "performance_metrics.csv", row.names = FALSE)

# Print message to confirm export
cat("Performance metrics exported to 'performance_metrics.csv'\n")

# Visualization: Scatter Plot of Observed vs Simulated Data
ggplot(data, aes(x = Observed, y = Simulated)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Observed vs Simulated Temperature") +
  xlab("Observed Temperature (°C)") +
  ylab("Simulated Temperature (°C)") +
  theme_minimal()


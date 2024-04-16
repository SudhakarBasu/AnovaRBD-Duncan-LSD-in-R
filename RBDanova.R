# Install and load the agricolae package
install.packages("agricolae")
library(agricolae)

# Set working directory
setwd("C:/Users/user/Desktop/github/anova rbd")

# Read the CSV file
RBD <- read.csv("RBD.csv")

# Display the first few rows of the dataset
head(RBD)

# Display the entire dataset
RBD

# Check the structure of the dataset
str(RBD)

# Check the dimensions of the dataset
dim(RBD)

# Check the class of the dataset
class(RBD)

# Convert factors to appropriate data types
GEN <- as.factor(RBD$GEN)
RBD$REP <- as.factor(RBD$REP)

# Fit the linear model
model <- lm(RBD$YPP ~ RBD$REP + RBD$GEN)

# Perform ANOVA
anova_result <- anova(model)
anova_result

# For fitted vs Residuals and Normal QQ plots
par(mfrow=c(1,2))
plot(model, which=1)
plot(model, which=2)

# Duncan test
DNMRT <- duncan.test(RBD$YPP, RBD$GEN, 58, 0.6600)
DNMRT

# LSD test
LSD <- LSD.test(RBD$YPP, RBD$GEN, 58, 0.6600)
LSD

# Arrange the data output file for making graphs
library(ggplot2)
msd <- LSD$means[,1:2]
ag <- LSD$groups
ag1 <- ag[order(row.names(ag)),]
dt <- data.frame(Trt = row.names(ag1), Avg = msd$`RBD$YPP`, Sdv = msd$std, gr = ag1$groups)
dt

# ggplot for RBD
ggplot(dt, aes(x = Trt, y = Avg)) +
  geom_bar(stat = "identity", aes(fill = Trt), width = 0.5) +
  geom_errorbar(aes(ymin = Avg - Sdv, ymax = Avg + Sdv), width = 0.1) +
  geom_text(aes(label = gr, y = Avg + Sdv), vjust = -0.5)

# Save the results in a text file
sink("RBD.txt")
print(anova_result)
print("DNMRT Result")
print(DNMRT$statistics)
print(DNMRT$groups)
print("LSD Result")
print(LSD$statistics)
print(LSD$groups)
sink()

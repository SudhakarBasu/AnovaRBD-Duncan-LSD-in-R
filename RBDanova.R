install.packages("agricolae")
library(agricolae)
setwd("C:/Users/user/Desktop/github/anova rbd")
RBD <- read.csv("C:/Users/user/Desktop/github/anova rbd/RBD.csv")
head(RBD)
RBD
str(RBD)
dim(RBD)
class(RBD)
GEN <- as.factor(RBD$GEN)
RBD$REP <- as.factor(RBD$REP)
model <- lm(RBD$YPP~ RBD$REP+RBD$GEN)
# ANOVA TABLE FOR RBD
anova <-anova(model)
anova
#For fitted vs Residuals and Normal QQ plots
par(mfrow=c(1,2))
plot(model, which=1)
plot(model, which=2)
#Duncan test## change below values 58,0.6600 to your data set Df and Mean Sq of residuals of anova table
DNMRT <-duncan.test(RBD$YPP,RBD$GEN,58,0.6600)
DNMRT
#LSD test###  change below values 58,0.6600 to your data set Df and Mean Sq of residuals of anova table
LSD <-LSD.test(RBD$YPP,RBD$GEN,58,0.6600)
LSD
#Save the file in txt
sink("RBD.txt")
print(anova)
print("DNMRT Result")
print(DNMRT$statistics)
print(DNMRT$groups)
print("LSD Result")
print(LSD$statistics)
print(LSD$groups)
sink()

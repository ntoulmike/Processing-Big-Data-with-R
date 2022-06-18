# Loading Libraries -------------------------------------------------------
library(MASS)
library(caTools)
library(leaps)
library(r1071)
# Importing ---------------------------------------------------------------
df <- read.csv("Find smokers by vital signs.csv", sep = ";")
str(df)
# Removing Unnecessary Data -----------------------------------------------
df <- subset(df, select = -oral)
df <- df[, -1]
# Missing values = 0 -------------------------------------------------------
sum(is.na(df))

# Transforming Collums to Appropriate Type --------------------------------
df$gender <- replace(df$gender, df$gender=="M", 1)
df$gender <- replace(df$gender, df$gender=="F", 0)
df$gender <- as.integer(df$gender)

df$tartar <- replace(df$tartar, df$tartar=="Y", 1)
df$tartar <- replace(df$tartar, df$tartar=="N", 0)
df$tartar <- as.integer(df$tartar)

# Reviewing Data Set ------------------------------------------------------
str(df)
# Linear Regression -------------------------------------------------------
lm.fit <- lm(smoking ~ ., data= df)

# Plot of Linear Regression -----------------------------------------------
par(mfrow=c(2,2))
plot(lm.fit)
summary(lm.fit)
par(mfow=c(1,1))

# Optimal fit using reg subset ------------------------------------------
#25 variables as input
regfit.full <- regsubsets(smoking ~ ., df, nvmax = 25)
reg.summary <- summary(regfit.full)

# Plots of optimal LM -----------------------------------------------------

# Plot RSS
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables ", ylab="RSS", type="l")

# Plot Adj R2
plot(reg.summary$adjr2, xlab="Number of Variables ", ylab="Adjusted Rsq", type="l")
# Find index of the maximum value
max_idx <- which.max(reg.summary$adjr2)
# Add a point on the last plot to highlight the max value
points(max_idx, reg.summary$adjr2[max_idx], col="red", cex=2, pch=20)
print(max_idx)

#Plot Cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
# Find index of the maximum value
min_idx <- which.min(reg.summary$cp)
# Add a point on the last plot to highlight the max value
points(min_idx, reg.summary$cp[min_idx], col="red", cex=2, pch=20)

#Plot Bic
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "Bic", type = "l")
min_idx <- which.min(reg.summary$bic)
points(min_idx, reg.summary$bic[min_idx], col="red", cex=2, pch=20)


# Metrics of Lm -----------------------------------------------------------
summary(regfit.full)$rsq
coef(regfit.full, min_idx)
# Best model in terms of Bic , seems to be the one with 16 variables which are:
#gender, height.cm., weight.kg., systolic, relaxation, fasting.blood.sugar, 
#Cholesterol, triglyceride, HDL, hemoglobin, serum.creatinine, AST, ALT, 
# Gtp, dental.caries and tartar 

#The R Squared of that model is 0.3033785, which regarding regression is really bad.

# Logistic Regression -----------------------------------------------------
# Create train and test Data set ------------------------------------------

set.seed(1)

sample <- sample.split(df, SplitRatio = 0.7)
df_train  <- subset(df, sample == TRUE)
df_test   <- subset(df, sample == FALSE)

# Dimensions of Test ------------------------------------------------------
dim(df_test)
#[1] 17821    25

# Train the model ---------------------------------------------------------
#We use the subset of best 16 variables from the previous calculation
glm.fits <- glm(smoking ~  . - age - waist.cm. - eyesight.left. - 
                  eyesight.right. - hearing.left. - hearing.right.
                - LDL - Urine.protein, data = df_train, family = binomial)

# Metricó of GLM -----------------------------------------------------------
summary(glm.fits)

# Prediction and Test -----------------------------------------------------
glm.probs <- predict(glm.fits, df_test, type = "response")
glm.pred <- rep(0,17821)
glm.pred[glm.probs>0.5] <- 1

# Confusion Matrix --------------------------------------------------------
table(glm.pred,df_test$smoking)

# Accuracy and Error ------------------------------------------------------
ac <- mean(glm.pred == df_test$smoking)
# ac is 0.7459177
error <- 1 - ac
# error is 0.2540823

# Plot of optimal GLM -----------------------------------------------------
par(mfrow=c(2,2))
plot(glm.fits)

par(mfrow=c(1,1)) 


# Support Vector Classifier -----------------------------------------------
# New splits --------------------------------------------------------------
set.seed(1)

sample <- sample.split(df_test, SplitRatio = 0.7)
ndf_train  <- subset(df_test, sample == TRUE)
ndf_test   <- subset(df_test, sample == FALSE)

sample <- sample.split(ndf_test, SplitRatio = 0.7)
sndf_train  <- subset(ndf_test, sample == TRUE)
sndf_test   <- subset(ndf_test, sample == FALSE)

sample <- sample.split(sndf_test, SplitRatio = 0.7)
ssndf_train  <- subset(sndf_test, sample == TRUE)
ssndf_test   <- subset(sndf_test, sample == FALSE)

# Selecting best Variables through correlation ----------------------------
cor(df)

# SVC ---------------------------------------------------------------------
set.seed(1)

svmfit <- svm(smoking ~ serum.creatinine + hemoglobin
              + triglyceride + waist.cm. + weight.kg.
              + height.cm. + gender, data = ssndf_train, kernel= "linear", cost =10, scale = FALSE)

# Finding optimal cost ----------------------------------------------------

tune.out <- tune(svm, smoking ~ serum.creatinine + hemoglobin + triglyceride 
                 + waist.cm. + weight.kg. + height.cm. + 
                   gender, data = ssndf_train, kernel="linear", ranges =list(cost=c(0.01, 0.1, 1, 10, 100)))



plot(tune.out$bestmodel, ssndf_test$smoking)
# Not plot aqquired

summary(tune.out$best.model)

# Confusion Matrix --------------------------------------------------------


table(true= ssndf_test, pred=predict(tune.out$bestmodel, newdata=ssndf_test))

# PCA ---------------------------------------------------------------------

# Perfoming PCa -----------------------------------------------------------

pr.out <- prcomp(df, scale. = TRUE)

pr.out
# PLotting the first two principal components -----------------------------

biplot(pr.out, scale=0)


# Computing Variance ------------------------------------------------------
pr.var <- pr.out$sdev^2
pr.var


# Computing Propotion of Variance Explained (PVE) -------------------------
pve <- pr.var/sum(pr.var)
pve

# Plot PVE ----------------------------------------------------------------

plot(pve, xlab = "Principal Component", ylab = "Propotion of Variance Explained", type = "b")

# END ---------------------------------------------------------------------



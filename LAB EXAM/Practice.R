# ==============================
# R Lab Exam Complete Script
# ==============================

# 1️⃣ Load packages
library(psych)      # for describe() and alpha()
library(Hmisc)
library(emmeans)    # for probing interactions

# ==============================
# 2️⃣ Import Data
# ==============================
data <- read.csv("data_fial_v4.csv")

# ==============================
# 3️⃣ Reverse Coding (if needed)
# ==============================
data$CON_1r <- 6 - data$SC_1
data$SC_2r <- 6 - data$SC_2
data$SC_3r <- 6 - data$SC_3
data$SC_6r <- 6 - data$SC_6
data$SC_8r <- 6 - data$SC_8
data$SC_11r <- 6 - data$SC_11

# ==============================
# 4️⃣ Compute Scale Means
# ==============================
### WHY DO THIS? To make it into a single average score for each participant
data$SC_mean <- rowMeans(cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, 
                               data$SC_7, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12))

data$VH_mean <- rowMeans(cbind(data$VH_1, data$VH_2, data$VH_3, data$VH_4, data$VH_5, data$VH_6, data$VH_7, data$VH_8), na.rm = TRUE)

# ==============================
# 5️⃣ Check Reliability
# ==============================
items_SC <- cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_7, 
                  data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12)
alpha(items_SC)

items_VH <- cbind(data$VH_1, data$VH_2, data$VH_3, data$VH_4, data$VH_5, data$VH_6, data$VH_7, data$VH_8)
alpha(items_VH)

# ==============================
# 6️⃣ Descriptive Stats
# ==============================
describe(data$SC_mean)
describe(data$VH_mean)

# ==============================
# 7️⃣ Center / Z-Score Variables
# ==============================
data$zSC_mean <- (data$SC_mean - mean(data$SC_mean, na.rm=TRUE)) / sd(data$SC_mean, na.rm=TRUE)
data$zVH_mean <- (data$VH_mean - mean(data$VH_mean, na.rm=TRUE)) / sd(data$VH_mean, na.rm=TRUE)
data$zExercise <- (data$Exercise - mean(data$Exercise, na.rm=TRUE)) / sd(data$Exercise, na.rm=TRUE)

# ==============================
# 8️⃣ Scatterplots (Main Effects)
# ==============================
plot(data$zExercise ~ data$zSC_mean, data = data)
plot(data$zExercise ~ data$zVH_mean, data = data)

# ==============================
# 9️⃣ Main Effects Regression
# ==============================
model1 <- lm(zExercise ~ zSC_mean + zVH_mean, data=data)
summary(model1)

# ==============================
# 10️⃣ Moderation / Interaction
# ==============================
model2 <- lm(zExercise ~ zSC_mean * zVH_mean, data=data)
summary(model2)
anova(model1, model2) # compare R²

# ==============================
# 11️⃣ Practice with GPA
# ==============================
model3 <- lm(GPA ~ zSC_mean + zVH_mean, data=data) # main effects
summary(model3)

model4 <- lm(GPA ~ zSC_mean * zVH_mean, data=data) # interaction
summary(model4)
anova(model3, model4)

plot(GPA ~ zSC_mean, data=data)
plot(GPA ~ zVH_mean, data=data)

# ==============================
# 12️⃣ Probe Interaction (Simple Slopes)
# ==============================
# Using emtrends (simple slopes at -1 and +1 SD for z-scores)
emtrends(model4, ~ zVH_mean,
         var = "zSC_mean",
         at = list(zVH_mean = c(-1, 1),
                   zSC_mean = c(-1, 1)))

# ==============================
# 13️⃣ Interaction Plot
# ==============================
# Visualize moderation
emmip(model4, zVH_mean ~ zSC_mean,
      at = list(zVH_mean = c(-1, 1)))

####################### IMPORTS ####################### 

library(psych)      
library(Hmisc)
library(emmeans)

# Define data
data <- read.csv("data_final_v4.csv")

# Reverse the score for confidence 
data$CON_1r <- 8 - data$CON_1
data$CON_2r <- 8 - data$CON_2
data$CON_5r <- 8 - data$CON_5

# Reverse the score for autonomy 
data$AUTO_7r <- 8 - data$AUTO_7

# Personal Growth
data$PG_Mean <- rowMeans(cbind(data$PG_1, data$PG_2, data$PG_3, data$PG_4, data$PG_5))

# Confidence Mean
data$CON_Mean <- rowMeans(cbind(data$CON_1r, data$CON_2r, data$CON_3, data$CON_4, data$CON_5r, data$CON_6 ))

# AUTO Mean
data$AUTO_Mean <- rowMeans(cbind(data$AUTO_1, data$AUTO_2, data$AUTO_3, data$AUTO_4, data$AUTO_5, data$AUTO_6, data$AUTO_7r))

### Define items for re-use
items_PG_Mean <- cbind(data$PG_1, data$PG_2, data$PG_3, data$PG_4, data$PG_5)

items_CON_Mean <- cbind(data$CON_1r, data$CON_2r, data$CON_3, data$CON_4, data$CON_5r, data$CON_6)

items_AUTO_Mean <- cbind(data$AUTO_1, data$AUTO_2, data$AUTO_3, data$AUTO_4, data$AUTO_5, data$AUTO_6, data$AUTO_7r)

alpha(items_CON_Mean)
describe(data$CON_Mean)
alpha(items_AUTO_Mean)
describe(data$AUTO_Mean)
alpha(items_PG_Mean)
describe(data$PG_Mean)


# Correlation 

# why did I use print(corr.test(), short=FALSE)?
# I think its limited. it said to print with short false so I could see the CI
# After printing with short=false, I could see the CI? Maybe my screen is too 
# short?


# Confidence & Autonomy 
print(corr.test(data$CON_Mean, data$AUTO_Mean), short=FALSE)

# Autonomy & Personal Growth 
print(corr.test(data$AUTO_Mean, data$PG_Mean), short=FALSE)

# Personal Growth & Confidence 
print(corr.test(data$PG_Mean, data$CON_Mean), short=FALSE) # we expect this to be high

# Center Confidence Mean 
data$zCON_Mean <- (data$CON_Mean - mean(data$CON_Mean, na.rm=TRUE))/sd(data$CON_Mean, na.rm=TRUE)

# Center Autonomy mean
data$zAUTO_Mean <- (data$AUTO_Mean - mean(data$AUTO_Mean, na.rm=TRUE))/sd(data$AUTO_Mean, na.rm=TRUE)

# CONTEXT confidence and autonomy on personal growth

# Main effect 
model1 <- lm(data$PG_Mean ~ data$zCON_Mean + data$zAUTO_Mean, data=data)
summary(model1)

# Interaction effect.
model2 <- lm(PG_Mean ~ zCON_Mean * zAUTO_Mean, data=data)
summary(model2)

# Not really needed ?
anova(model1, model2)

emtrends(model2, ~ zAUTO_Mean,
         var="zCON_Mean",
         at = list(zCON_Mean = c(-1, 1),
                   zAUTO_Mean = c(-1, 1)))
emmip(model2, zAUTO_Mean ~ zCON_Mean,
      at = list(zAUTO_Mean = c(-1, 1)))

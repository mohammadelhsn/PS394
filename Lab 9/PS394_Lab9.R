
#use your code from previous lab to set your working directory and load the lab data

#use your code from previous lab to set your working directory and load the lab data

library(psych)
library(Hmisc)

## read the data
data <- read.csv("PS394 Class Dataset F2025_ME.csv")

###1) Compute your variables ----

##The self-compassion scale includes items SC_1 through SC_12. Items are rated 1 (almost never) through 5 (almost always). Items 1, 2, 3, 6, 8, 11 are reverse coded. The items and labels are below for reference. 
#SC_1: I’m disapproving and judgmental about my own flaws and inadequacies. (R)
#SC_2: When I’m feeling down I tend to obsess and fixate on everything that’s wrong. (R)
#SC_3: When I fail at something important to me I become consumed by feelings of inadequacy.(R)
#SC_4: When something upsets me I try to keep my emotions in balance.
#SC_5: When I feel inadequate in some way, I try to remind myself that feelings of inadequacy are shared by most people.
#SC_6: I’m intolerant and impatient towards those aspects of my personality I don't like. (R)
#SC_7: When I’m going through a very hard time, I give myself the caring and tenderness I need.
#SC_8: When I’m feeling down, I tend to feel like most other people are probably happier than I am. (R)
#SC_9: When something painful happens I try to take a balanced view of the situation.
#SC_10: I try to see my failings as part of the human condition.
#SC_11: When I fail at something that's important to me, I tend to feel alone in my failure. (R)
#SC_12: I try to be understanding and patient towards those aspects of my personality I don't like

##Step 1: Reverse code reversed items

data$SC_1r<- (6-data$SC_1) #create a new variable(column) ‘SC_1r’ with the reverse coded values of column ‘SC_1’. Not be subtract the items from 6 here - but this may differ depending on the scale. This value should be the highest rating on the scale plus 1 (i.e., 5 + 1 = 6)
data$SC_2r<- (6-data$SC_2) #create a new variable(column) ‘SC_2r’
data$SC_3r<- (6-data$SC_3) #create a new variable(column) ‘SC_3r’
data$SC_6r<- (6-data$SC_6) #create a new variable(column) ‘SC_6r’
data$SC_8r<- (6-data$SC_8) #create a new variable(column) ‘SC_8r’
data$SC_11r<- (6-data$SC_11) #create a new variable(column) ‘SC_11r’

##Step 2: Compute the mean
data$SC_mean<- rowMeans(cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_7, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12)) #create a new variable (columm) SC_mean that is equal to the average of the scale items - be sure to use the reverse coded items for reverse items!!!

##Step 3: Check the reliability
items_SC <- cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_7, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12) #create a data subset containing only the items used to compute your scale
alpha(items_SC) #get the alpha for the scale items

####The valuing happiness scale 
#VH_1: How happy I am at any given moment says a lot about how worthwhile my life is.
#VH_2: If I don’t feel happy, maybe there is something wrong with me.
#VH_3: I value things in life only to the extent that they influence my personal happiness.
#VH_4: If I don’t feel happy, I worry about it.
#VH_5: Feeling happy is extremely important to me.
#VH_6: I am concerned about my happiness even when I feel happy.
#VH_7: To have a meaningful life, I need to feel happy most of the time.
#VH_8: I get somewhat distressed if I don’t feel happy.

##Step 1: Reverse code reversed items
#No reverse coded items in this scale

##Step 2: Compute the mean

data$VH_mean <- rowMeans(cbind(data$VH_1, data$VH_2, data$VH_3, data$VH_4, data$VH_5, data$VH_6, data$VH_7, data$VH_8), na.rm = TRUE)

##Step 3: Check the reliability

data_VH <-cbind(data$VH_1, data$VH_2, data$VH_3, data$VH_4, data$VH_5, data$VH_6, data$VH_7, data$VH_8)
alpha(data_VH)


###2) Inspecting the variables ----

describe(data$SC_mean)
describe(data$VH_mean)

###3) Run a multiple regression ----

##step 1: main effects model
#center your variables (if you want/need to) - using z score method here
data$zSC_mean <- ((data$SC_mean - mean(data$SC_mean, na.rm=TRUE))/sd(data$SC_mean, na.rm=TRUE)) #convert you variable to z scores
data$zVH_mean <- ((data$VH_mean - mean(data$VH_mean, na.rm=TRUE))/sd(data$VH_mean, na.rm=TRUE)) #convert you variable to z scores
data$zExercise <- ((data$Exercise - mean(data$Exercise, na.rm=TRUE))/sd(data$Exercise, na.rm=TRUE)) #convert you variable to z scores

#get scatterplots for your variables
plot(data$zExercise ~ data$zSC_mean, data = data) #get a scatterplot with self compassion on the x axis and Exercise on the y axis
plot(data$zExercise ~ data$zVH_mean, data = data) #get a scatterplot with self compassion on the x axis and Exercise on the y axis

#run main effects model regression
model1 <- lm(zExercise ~ zSC_mean + zVH_mean, data = data)
summary(model1) #regression where you interpret the main effects of your predictors on the outcome

##Step 2: moderation model

#run regression
model2 <- lm(zExercise ~ zSC_mean*zVH_mean, data = data)
summary(model2) #regression where you interpret the moderation effect - NOT MAIN EFFECTS

#compare models: Rsquare change = 0.08285 (R2 from model1) - 0.03074 (R2 from model2) = 0.03211 (3.211%)
anova(model1, model2) #compare the models - p value for R square change


##### PRACTICE #########
plot(GPA ~ zSC_mean, data=data)
plot(GPA ~ zVH_mean, data=data)

# zSC_mean on GPA and zVH_mean on GPA
model3 <- lm(GPA ~ zSC_mean + zVH_mean, data=data)
summary(model3)

# zSC_mean x zVH_mean on GPA 
# The format is OUTCOME ~ VAR 1 +/* VAR 2 depending on what you're looking for

model4 <- lm(GPA ~ zSC_mean * zVH_mean, data=data)
summary(model4)

anova(model3,model4)


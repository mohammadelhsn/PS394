###1) Setting working directory ----

getwd() #where is your working directory?

###2) Loading the data ----

data <- read.csv("PS394 Class Dataset F2025.csv") #load the data in r

View(data) #open the data window - see anything weird?
summary(data) #get a summary of the data - notice how everything is a character variable? This is because of those annoying first two rows carried in from Qualtrics

data <- data[-c(1, 2), ] #lets drop the top to rows of our data

View(data) #everything looks okay now!
summary(data) #but it's not - still reading as character variables because R decides that when you download the data

#so let's quickly jump out of R, open the data file in Excel, delete the teo extra rows (row 2 and 3 in excel - keep the variable names). Save the data with your intials at the end (i.e., "_XX")
data <- read.csv("PS394 Class Dataset F2025_ME.csv") #let's load it back in

View(data) #take a look and all looks good
summary(data) #and now the variables aren't all reading as character!!

###3) Inspecting the data ----

data$ID <- seq_len(nrow(data)) #we are quickly going to compute an ID number for each row - 1 through 131
data$ID # if you run the variable date, you can see everyone now has a unique ID. you can also open the data and scroll right to the final column to see it in the dataset

#To clean the data, we need to inspect it... Here is an extensive suite of functions that will allow you to look at any varriable. In our example, we are looking at the GPA variable. 

hist(data$GPA) #histogram of GPA
mean(data$GPA, na.rm=TRUE) #mean of variable in dataset
median(data$GPA, na.rm=TRUE) #median of variable in dataset
range(data$GPA, na.rm=TRUE) #range of variable in dataset
var(data$GPA, na.rm=TRUE) #variance of variable in dataset
sd(data$GPA, na.rm=TRUE) #standard deviation of variable in dataset

#You can also get a birdseye view of the data using the describe() function
#This code uses psych, so that needs to be loaded before it will run

library(psych)
describe(data)

###4) Changing the data ----

# If you were to find any specific values you wanted to recode, you could use this code

data$GPA[data$ID == "70"] <- 8.9 #recode the GPA of person with ID 70 to 8.9
data$GPA[data$ID == "73"] <- 8.9 #recode the GPA of person with ID 85 to 8.9

###5) Correlations ----

cor.test(data$GPA, data$Exercise) #getting a single correlation, between GPA and Exercise

#If you want many correlations between many variables, you can get a matrix, rather than getting them one by one
rcorr(cbind(data$GPA, data$Exercise, data$Pets)) #getting a correlation matrix of all variables listed




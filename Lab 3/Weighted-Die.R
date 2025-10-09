1+1

100:130

5 -
  +
  +
  1

# Returns an error
#3 % 5


# Exercise 2.1

# My chosen number is 20
num <- 20
num # output 
# Add 2 to my number
num <- num + 2
num # output
# Multiply result by 3
num <- num * 3
num # output
# Subtract 6
num <- num - 6
num # output
# Divide by 3 
num <- num / 3
num # output


1:6 # lists all nums from 1-6

a <- 1
a
## 1

a + 2
## 3


die <- 1:6
die


my_number <- 1
my_number 
## 1
my_number <- 999
my_number

ls()

die - 1

die / 2

die * die

1:2

1:4

die

die + 1:2

die + 1:4

die %*% die

die %o% die

round(3.1415) 

factorial(3)

mean(1:6)

mean(die)

round(mean(die))

sample(x=1:4, size=2)

sample(x=die, size=1)
sample(x=die, size=1)
sample(x=die, size=1)

# a random of the sample?

# error round(3.1415, corners=2)

args(round)

round(3.1415)

round(3.1415, digits=2)

sample(die, 1)

sample(size=1, x=die)

sample(die, size=2)

sample(die, size=2, replace=TRUE)

sample(die, size=2, replace=TRUE)

dice <- sample(die, size=2, replace=TRUE)
dice

sum(dice)

dice
dice
dice

# all returns the same, it's saved in a var

dice <- 1:6
dice <- sample(die, size=2, replace=TRUE)
sum(dice)

roll <- function() {
  dice <- 1:6
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)
}

roll()

roll

roll()
dice 
1+1
sqrt(2)

# these wont output anything because they're being saved in a variable
# you would need to log these values to see anything

dice <- sample(die, size=2, replace=TRUE)
two <- 1 + 1
a <- sqrt(2)

roll2 <- function() {
  # error, bones is undefined
  dice <- sample(bones, size=2, replace=TRUE)
  sum(dice)
}

roll2() # error

roll2 <- function(bones) {
  dice <- sample(bones, size=2, replace=TRUE)
  sum(dice)
}

roll2(bones=1:4)
roll2(bones=1:6)
roll2(bones=1:20)
roll2()

roll2 <- function(bones=1:6) {
  dice <- sample(bones, size=2, replace=TRUE)
  sum(dice)
}

roll2()
roll2(1:4)

# ===================== PART 3 - PACKAGES AND HELP PAGES =================
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x

y <- x^3
y

qplot(x,y)


x <- c(1,2,2,2,3,3)
qplot(x, binwidth=1)

x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)

x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)

replicate(3, 1+1)

replicate(10, roll())

rolls <- replicate(10000, roll())
qplot(rolls, binwidth=1)

roll <- function() {
  dice <- 1:6
  dice <- sample(die, size=2, replace=TRUE, prob=c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}


rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)


# ============================= PROJECT 2 ================================
die <- c(1,2,3,4,5,6)
die
is.vector(die)
five <- 5
five 
is.vector(five) # true, a vector of length 1
length(five) # 1
length(die) # 6

int <- 1L
text <- "ace"

int <- c(1L, 5L)
text <- c("ace", "hearts")

sum(int)
sum(text)

die <- c(1,2,3,4,5,6)
die
#

typeof(die) # double

int <- c(-1L, 2L, 4L)
int

typeof(int) # integer

sqrt(2)^2 - 2


text <- c("Hello", "World")
text

typeof(text) # character

typeof("Hello") # character

3 > 4 # false

logic <- c(TRUE, FALSE, TRUE)
logic
typeof(logic) # logical (Boolean)

typeof(F) # shorthand for FALSE?

comp <- c(1+1i, 1+2i, 1+3i)
comp

typeof(comp)


raw(3)

hand <- c("ace", "king", "queen", "jack", "ten")
hand

typeof(hand) # character

names(die) # NULL

names(die) <- c("one", "two", "three", "four", "five", "six")
names(die)
attributes(die)
names(die) <- c("uno", "dos", "tres", "quatro", "cinco", "seis")
die

names(die) <- NULL
die

######### 5.2 ###########################################################
dim(die) <- c(2,3)
die
dim(die)<- c(3,2)
die
dim(die) <- c(1,2,3)
die

m<-matrix(die, nrow=2)
m

m<-matrix(die, nrow=2, byrow=TRUE)
m

ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar

hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades", 
           "spades", "spades", "spades")

matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5, 2)

hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", 
           "spades", "ten", "spades")

matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)

dim(die) <- c(2,3)
typeof(die) # double

class(die) # matrix

attributes(die)

class("Hello")
class(5)
now <- Sys.time() ##  "double"
now
typeof(now)
class(now) ## "POSIXct" "POSIXt" 

unclass(now)
mil <- 1000000
mil #1e+06

class(mil) <- c("POSIXct", "POSIXt")
mil

######### 5.5.2 #############
gender <- factor(c("male", "female", "male", "female"))
typeof(gender) # integer

attributes(gender)

unclass(gender)
gender
as.character(gender)


### Exercise 5.4
card <- c("ace", "hearts", 1)
card

sum(c(TRUE, TRUE, FALSE, FALSE)) # true=1, false=0 
sum(c(1,1,0,0))

as.character(1)
as.logical(1)
as.numeric(FALSE)

list1 <- list(100:130, "R", list(TRUE, FALSE))
list1

card <- list("ace", "hearts", 1)
card

df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df


typeof(df)

class(df)

str(df)

df <- data.frame(face=c("ace","two","six"),suit=c("clubs","clubs","clubs"), value=c(1,2,3), stringsAsFactors = FALSE)
df


deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten", 
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace", 
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five", 
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine", 
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),  
  suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
           "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts"), 
  value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 
            7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 
            10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)
deck

head(deck)
write.csv(deck, file = "cards.csv", row.names = FALSE)

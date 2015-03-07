# INTERNET PRIVACY POLL

poll <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/AnonymityPoll.csv", 
                header = TRUE, stringsAsFactors = TRUE)

str(poll)
summary(poll)
dim(poll)

# Let's look at the breakdown of the number of people with 
# smartphones using the table() and summary() commands on the 
# Smartphone variable. (HINT: These three numbers should 
# sum to 1002.)

# How many interviewees responded that they use a smartphone?

table(poll$Smartphone)
487/1002
472/1002

table(poll$Sex, poll$Region)

# Which of the following are states in the Midwest census region?
summary(poll)
table(poll$State, poll$Region == "Midwest")


# Which was the state in the South census region with 
# the largest number of interviewees?
table(poll$State, poll$Region == "South")


# How many interviewees reported neither Internet 
# use nor smartphone use?

summary(poll)

x <- subset(poll, Smartphone == 0 & Internet.Use == 0)
dim(x)


table(poll$Smartphone == 0, poll$Internet.Use == 0)

# How many interviewees reported both Internet use and smartphone use?
table(poll$Smartphone == 1, poll$Internet.Use == 1)

# How many interviewees reported Internet use but no smartphone use?
table(poll$Smartphone == 0, poll$Internet.Use == 1)

# How many interviewees reported smartphone use but no Internet use?
table(poll$Smartphone == 1, poll$Internet.Use == 0)

# How many interviewees have a missing value for their Internet use?
summary(poll$Internet.Use)
# How many interviewees have a missing value for their smartphone use?
summary(poll$Smartphone)

# Use the subset function to obtain a data frame called "limited", 
# which is limited to interviewees who reported Internet use or who 
# reported smartphone use. In lecture, we used the & symbol to use 
# two criteria to make a subset of the data. To only take observations 
# that have a certain value in one variable or the other, the | character 
# can be used in place of the & symbol. This is also called a 
# logical "or" operation.

# How many interviewees are in the new data frame?

limited <- subset(poll, poll$Smartphone == 1 | poll$Internet.Use == 1)
dim(limited)

# Which variables have missing values in the limited data 
# frame? (Select all that apply.)

summary(limited)

# What is the average number of pieces of personal information 
# on the Internet, according to the Info.On.Internet variable?
mean(limited$Info.On.Internet, na.rm = TRUE)


# How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet == 0)

# How many interviewees reported the maximum value of 11 
# for Info.On.Internet?
table(limited$Info.On.Internet == 11)

# What proportion of interviewees who answered the Worry.About.Info 
# question worry about how much information is available about 
# them on the Internet? Note that to compute this proportion you will 
# be dividing by the number of people who answered the Worry.About.Info 
# question, not the total number of people in the data frame.
table(limited$Worry.About.Info == 1)
386 / (386+404)

# What proportion of interviewees who answered the 
# Anonymity.Possible question think it is possible to be 
# completely anonymous on the Internet?
table(limited$Anonymity.Possible == 1)
278 / (278+475)

# What proportion of interviewees who answered the 
# Tried.Masking.Identity question have tried masking 
# their identity on the Internet?
table(limited$Tried.Masking.Identity == 1)
128 / (128+656)

# What proportion of interviewees who answered the 
# Privacy.Laws.Effective question find United States 
# privacy laws effective?
table(limited$Privacy.Laws.Effective == 1)
186 / (186+541)

# Often, we are interested in whether certain characteristics 
# of interviewees (e.g. their age or political opinions) affect 
# their opinions on the topic of the poll (in this case, opinions 
# on privacy). In this section, we will investigate the relationship 
# between the characteristics Age and Smartphone and outcome 
# variables Info.On.Internet and Tried.Masking.Identity, again using 
# the limited data frame we built in an earlier section of this problem.

# Build a histogram of the age of interviewees. What is the best 
# represented age group in the population?
summary(limited)
hist(limited$Age)

# Both Age and Info.On.Internet are variables that take on many values, 
# so a good way to observe their relationship is through a graph. We 
# learned in lecture that we can plot Age against Info.On.Internet with 
# the command plot(limited$Age, limited$Info.On.Internet). However, 
# because Info.On.Internet takes on a small number of values, multiple 
# points can be plotted in exactly the same location on this graph.

# What is the largest number of interviewees that have exactly the same 
# value in their Age variable AND the same value in their 
# Info.On.Internet variable? In other words, what is the largest number 
# of overlapping points in the plot plot(limited$Age, 
# limited$Info.On.Internet)? (HINT: Use the table function to compare 
# the number of observations with different values of Age and 
# Info.On.Internet.)
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age, c(1, 2, 3)))
plot(jitter(c(1, 2, 3))
)

sum(jitter(c(1, 2, 3)))
jitter(c(1, 2, 3))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# Use the tapply() function to obtain the summary of the 
# Info.On.Internet value, broken down by whether an interviewee 
# is a smartphone user.

# What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, mean)

# Similarly use tapply to break down the Tried.Masking.Identity 
# variable for smartphone and non-smartphone users.

# What proportion of smartphone users who answered the 
# Tried.Masking.Identity question have tried masking their 
# identity when using the Internet?
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm = TRUE)




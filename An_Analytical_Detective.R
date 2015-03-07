## Week One
 mvtWeek1 <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/mvtWeek1.csv", 
             header = TRUE, stringsAsFactors = TRUE)

 str(mvtWeek1)
 summary(mvtWeek1)
# How many rows of data (observations) are in this dataset?
# How many variables are in this dataset?
 dim(mvtWeek1)

# Using the "max" function, what is the maximum value of the variable "ID"?
 max(mvtWeek1$ID, na.rm = TRUE)

# What is the minimum value of the variable "Beat"?
 min(mvtWeek1$Beat, na.rm = TRUE)
 
# How many observations have value TRUE in the Arrest variable 
# (this is the number of crimes for which an arrest was made)?
 summary(mvtWeek1$Arrest) 
 
# How many observations have a LocationDescription value of ALLEY?
 sum(as.numeric(mvtWeek1$LocationDescription == "ALLEY"))
 
# In what format are the entries in the variable Date?
 str(mvtWeek1$Date)
 
# What is the month and year of the median date in our dataset? 
# Enter your answer as "Month Year", without the quotes. 
# (Ex: if the answer was 2008-03-28, you would give the answer 
# "March 2008", without the quotes.) 
 
 DateConvert <- as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))
 median(DateConvert, na.rm = TRUE)
 
 
 mvtWeek1$Month <- as.factor(months(DateConvert)) 
 mvtWeek1$Weekday <- as.factor(weekdays(DateConvert))
 mvtWeek1$Date <- DateConvert
 
# In which month did the fewest motor vehicle thefts occur? 
 sort(table(mvtWeek1$Month))
 
 
# On which weekday did the most motor vehicle thefts occur?
 sort(table(mvtWeek1$Weekday))
 
 
# Each observation in the dataset represents a motor vehicle theft, 
# and the Arrest variable indicates whether an arrest was later 
# made for this theft. Which month has the largest number of motor 
# vehicle thefts for which an arrest was made
 
 table(mvtWeek1$Arrest,mvtWeek1$Month)

 x <- subset(mvtWeek1, mvtWeek1$Arrest == TRUE) 
 sort(table(x$Month))
 

 
 hist(mvtWeek1$Date, breaks=100)
 
 
# Create a boxplot of the variable "Date", sorted by the variable "Arrest" 
 boxplot(mvtWeek1$Date ~ mvtWeek1$Arrest)

 # For what proportion of motor vehicle thefts in 2001 was an arrest made?
 x <- table(mvtWeek1$Arrest, mvtWeek1$Year == "2001")
 x[2,2]/(x[2,2]+x[1,2]) 

 
 
 
 tail(sort(table(mvtWeek1$LocationDescription)))
 
 
# Create a subset of your data, only taking observations for which 
# the theft happened in one of these five locations, and call this 
# new data set "Top5". To do this, you can use the | symbol. In 
# lecture, we used the & symbol to use two criteria to make a subset 
# of the data. To only take observations that have a certain value 
# in one variable or the other, the | character can be used in place 
# of the & symbol. This is also called a logical "or" operation.
  
Top5 <- subset(mvtWeek1, LocationDescription == "STREET" |
                       LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                       LocationDescription == "ALLEY" |
                       LocationDescription == "GAS STATION" |
                       LocationDescription == "DRIVEWAY - RESIDENTIAL")
 
 dim(Top5)
 
 Top5$LocationDescription <- factor(Top5$LocationDescription)
 
 summary(Top5)
 
 
# One of the locations has a much higher arrest rate than the 
# other locations. Which is it? Please enter the text in 
# exactly the same way as how it looks in the answer options 
# for Problem 4.1.
 
 x <- table(Top5$Arrest, Top5$LocationDescription)
 dim(x)
 x[2, 1] / (x[2, 1] + x[1, 1])
 
 
 249/(249+2059)
132/(132+1543)
439/(439+1672)
1603/(1603+13249)
11595/(11595+144969)

 
 
# On which day of the week do the most motor vehicle 
# thefts at gas stations happen? 
 table(Top5$LocationDescription == "GAS STATION" , Top5$Weekday)
# On which day of the week do the fewest motor vehicle 
# thefts in residential driveways happen? 
 min(table(Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL" , Top5$Weekday))
 table(Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL" , Top5$Weekday)
 
 
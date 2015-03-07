### EMPLOYMENT
CPS <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/CPSData.csv", 
                header = TRUE, stringsAsFactors = TRUE)

str(CPS)
summary(CPS)
head(CPS)

dim(CPS)


summary(CPS)
x <- table(CPS$Citizenship)
dim(x)
(x[1]+x[2])/(x[1]+x[2]+x[3])


# Hispanic
Hispanic <- subset(CPS, Hispanic == 1)
sort(table(sort(Hispanic$Race)))

table(CPS$Race, CPS$Hispanic)

# Missing 
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))



table(CPS$State, is.na(CPS$MetroAreaCode))

# Which region of the United States has the largest proportion of 
# interviewees living in a non-metropolitan area?
summary(CPS)
x <- table(CPS$Region, is.na(CPS$MetroAreaCode))

x
1-((x[1,1]) / (x[1,1]+x[1,2]))
1-((x[2,1]) / (x[2,1]+x[2,2]))
1-((x[3,1]) / (x[3,1]+x[3,2]))
1-((x[4,1]) / (x[4,1]+x[4,2]))

table(CPS$Region, is.na(CPS$MetroAreaCode))

# Knowing this, use tapply() with the mean function to answer the following questions:
# Which state has a proportion of interviewees living in 
# a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))


##
MetroAreaMap <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/MetroAreaCodes.csv", 
                header = TRUE, stringsAsFactors = TRUE)
CountryMap <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/CountryCodes.csv", 
                header = TRUE, stringsAsFactors = TRUE)

dim(MetroAreaMap)
dim(CountryMap)
head(MetroAreaMap)
head(CountryMap)
head(CPS)
dim(CPS)
CPS1 <- CPS


CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
head(CPS)
dim(CPS)
summary(CPS)

# How many interviewees have a missing value for the new 
# metropolitan area variable? Note that all of these 
# interviewees would have been removed from the merged data 
# frame if we did not include the all.x=TRUE parameter.

summary(CPS)

# Which of the following metropolitan areas has the largest 
# number of interviewees?

# Atlanta-Sandy Springs-Marietta, GA 
# Baltimore-Towson, MD 
# Boston-Cambridge-Quincy, MA-NH 
# San Francisco-Oakland-Fremont, CA
sort(table(CPS$MetroArea))

# Which metropolitan area has the highest proportion of 
# interviewees of Hispanic ethnicity? Hint: Use tapply() 
# with mean, as in the previous subproblem. Calling sort() 
# on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector 
# of whether an interviewee is Asian, determine the number of 
# metropolitan areas in the United States from which at least 20% 
# of interviewees are Asian.
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))


#####
CPS2 <- CPS 
head(CountryMap)
head(MetroAreaMap)
head(CPS)
#CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

summary(CPS)


# What proportion of the interviewees from the 
# "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
# metropolitan area have a country of birth that is not the 
# United States? For this computation, don't include people 
# from this metropolitan area who have a missing country of birth.
summary(CPS)
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
      CPS$Country != "United States")

x <- subset(CPS, MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
summary(x)
dim(x)
1-3736/5409

# we can see that 1668 of interviewees from this metropolitan area 
# were born outside the United States and 3736 were born in the 
# United States (it turns out an additional 5 have a missing country 
# of origin). Therefore, the proportion is 1668/(1668+3736)=0.309.


# Which metropolitan area has the largest number (note -- not proportion) 
# of interviewees with a country of birth in India? Hint -- remember 
# to include na.rm=TRUE if you are using tapply() to answer this question.

table(CPS$Country == "India",
      CPS$MetroArea, sum, na.r)

summary(CPS)

sort(tapply(CPS$Country == "Somalia",
            CPS$MetroArea, sum, na.rm = TRUE))





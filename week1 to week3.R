# you can download the data files from this webiste: http://stat-computing.org/dataexpo/2009/the-data.html

# Ali Farjad
# some data analysis about indianopolise flights
#imported all the data from 2008 dataset
# of ASA Data Expo 2009


myDF <- read.csv("/Users/ALI/Downloads/2008.csv")
head(myDF)
# the first six flights
head(myDF)
# the last six flights
tail(myDF)
# airports of the first 6 orgins
head(myDF$Origin)
# airports of the last 6 orgins
tail(myDF$Origin)

# the destenation of firts and last six flight, respectively
head(myDF$Dest)
tail(myDF$Dest)

tail(myDF)
head(myDF$Origin=="IND")
myDF$Origin=="IND"
sum(myDF$Origin=="IND")

sum(myDF$Dest=="IND")


head(myDF$Origin=="ADK")
sum(myDF$Origin=="ADK")
sum(myDF$Dest=="ADK")


sum(myDF$Origin=="ORD")
sum(myDF$Dest=="ORD")

sum((myDF$Origin=="IND") & (myDF$Dest=="ORD"))


myIndyOrgins <- subset(myDF,myDF$Origin=="IND")
IndianapoliseDests <- subset(myDF,myDF$Dest=="IND")

head(myIndyOrgins)
head(IndianapoliseDests)

table(myIndyOrgins$Month)
plot(table(myIndyOrgins$Month))

table(IndianapoliseDests$Month)
plot(table(IndianapoliseDests$Month))

TUPOrgins <- subset(myDF,myDF$Origin=="TUP")
mean(TUPOrgins$DepDelay)


head(myIndyOrgins)
head(myIndyOrgins$DepTime<600)
head(myIndyOrgins$DepTime<1000)


sum(myIndyOrgins$DepTime<600, na.rm = TRUE)
?sum


sum(myIndyOrgins$DepTime<1200, na.rm = TRUE)

sum(myIndyOrgins$DepTime<1800, na.rm = TRUE)

sum(myIndyOrgins$DepTime<2400, na.rm = TRUE)
head(is.na(myIndyOrgins$DepTime))
sum(is.na(myIndyOrgins$DepTime))

739+42011


LAXdest <- subset(myDF,myDF$Dest=='LAX')
sum(LAXdest)


sum(myDF$Origin=="ATL" & myDF$Dest=="LAX")


ALTtoLAX <- subset(myDF,(myDF$Origin=="ATL")&(myDF$Dest=="LAX"))
sum(ALTtoLAX$DepTime<1200, na.rm = TRUE)
head(ALTtoLAX)
head(myDF)


newDF <- subset(myDF, (myDF$Origin == "ATL") & (myDF$Dest == "LAX"))
sum (newDF$DepTime < 1200, na.rm=TRUE)
##--------------------------------------------------------------


head(myIndyOrgins)
tail(myIndyOrgins)
head(myIndyOrgins$DepTime>600)
tail(myIndyOrgins$DepTime>600)
tail(myIndyOrgins$DepTime>1400)
sum(myIndyOrgins$DepTime>600,na.rm = TRUE)
sum(myIndyOrgins$DepTime>1400,na.rm = TRUE)
sum(myIndyOrgins$ArrTime>1700,na.rm = TRUE)

sum(myIndyOrgins$ArrTime<=2400,na.rm = TRUE)
sum(is.na(myIndyOrgins$ArrTime))
41917+833
sum(ALTtoLAX$DepTime<1200,na.rm = TRUE)
sum(myDF$Dest=="LAX")
#-------------------------------------------
table(ALTtoLAX$DepTime)
plot(table(ALTtoLAX$DepTime))


plot(table(round(ALTtoLAX$DepTime/100,digits=0)))
?round
plot(table(trunc(ALTtoLAX$DepTime/100,digits=0)))

#-----------------------------------------------------

table(myIndyOrgins$Month)
# build a table that shows how many ciies are the orgins
# for the flights throughout 2008
table(myDF$Origin)
# sort the result and show that Atlanta is the most often
sort(table(myDF$Origin))
# here are some sample departure time
head(myDF$DepTime)
# we want to break them up into these categories
seq(0,2400,by=100)
# this show number of flights departed in each hour
cut(myDF$DepTime,breaks = seq(0,2400,by=100))
table(cut(myDF$DepTime,breaks = seq(0,2400,by=100)))
plot(table(cut(myDF$DepTime,breaks = seq(0,2400,by=100))))

head(myDF$Origin)
head(myDF$Dest)
# paste command for paste to result together
head(paste(myDF$Origin,"to",myDF$Dest))
paste(myDF$Origin,"to",myDF$Dest)
# make a table of all orgin to destinamtion
table(paste(myDF$Origin,"to",myDF$Dest))
sort(table(paste(myDF$Origin,"to",myDF$Dest)))
# 20 most & least popular path
head(sort(table(paste(myDF$Origin,"to",myDF$Dest))),20)
tail(sort(table(paste(myDF$Origin,"to",myDF$Dest))),20)
----------
# How many origin-to-destination paths were only flown one time (each) in 2008?
sum(table(paste(myDF$Origin, "to", myDF$Dest)) == 1)

# how the tapply function works:
# find the average departure delay at each airports
tapply(myDF$DepDelay,myDF$Origin, mean,na.rm=TRUE)
sort(tapply(myDF$DepDelay,myDF$Origin, mean,na.rm=TRUE))
---------------------
# When considering all flights to an airport,
# take an average of the distances (in miles) of the flights to that airport. 
# Suppose that we do such an analysis of all airports. 
# Which airport has the longest average distance of flights (in miles) arriving to that airport?
head(myDF)
tail(tapply(myDF$Distance,myDF$Dest, mean))
tail(sort(tapply(myDF$Distance, myDF$Dest, mean)))

# which day of the week should we fly, if we want to minimize
# the expected arrival delay of the flight?
tapply(myDF$ArrDelay,myDF$DayOfWeek,mean,na.rm=TRUE)
plot(tapply(myDF$ArrDelay,myDF$DayOfWeek,mean,na.rm=TRUE))
?tapply
# Make a plot of the average departure delays for each airport of origin.
# Make a comment below to work on this with others.
plot(tapply(myDF$DepDelay,myDF$Origin,mean,na.rm=TRUE))
plot(sort(tapply(myDF$DepDelay,myDF$Origin,mean,na.rm=TRUE)))
plot(tapply(myDF$DepDelay [myDF$Origin=="CLE"], myDF$Origin [myDF$Origin=="CLE"], mean, na.rm=TRUE))


# which airlines have the best arrival delay?
# which airlines have the worst arrival delay? 
tapply(myDF$ArrDelay,myDF$UniqueCarrier,mean,na.rm=TRUE)

# Which airline has the worst average departure delay?
sort(tapply(myDF$DepDelay,myDF$UniqueCarrier,mean,na.rm=TRUE))

# on which day of the year were the expected ArrDelays the worst?
mydates <- paste(myDF$Month,myDF$DayofMonth,myDF$Year, sep = "/")

length(myDF$ArrDelay)
length(mydates)
sort(tapply(myDF$ArrDelay,mydates,mean,na.rm=TRUE))
# this is a leap year so had 366 days we got
length(sort(tapply(myDF$ArrDelay,mydates,mean,na.rm=TRUE)))
----------------------------
# On which day of the year were the average departure delays the worst?

sort(tapply(myDF$DepDelay,mydates,mean,na.rm=TRUE))

# On which day of the year were the average departure times the worst 
# for flights departing from O’Hare (ORD)?
sort(tapply(myDF$DepDelay[myDF$Origin=="ORD"],mydates[myDF$Origin=="ORD"],mean,na.rm=TRUE))


ordtoind <- myDF$Dest=="IND" & myDF$Origin=="ORD"
length(ordtoind)
sort(tapply(myDF$ArrDelay[ordtoind],
            mydates[ordtoind],mean,na.rm=TRUE))

# Consider flights that departed from ATL and landed at LAX in 2008.
# For how many days of the year were the average departure delays more than 90 minutes?
atltolax <- myDF$Dest=="LAX" & myDF$Origin=="ATL"
sum(sort(tapply(myDF$DepDelay[atltolax],mydates[atltolax],mean,na.rm=TRUE))>90)

tail(sort(tapply(myDF$DepDelay[atltolax], 
                 mydates[atltolax], 
                 mean, na.rm = TRUE)),1)


#------------------------------------------
#most popular airports
sort(table(myDF$Origin))

sort(table(myDF$Origin),decreasing = TRUE)
sort(table(myDF$Origin),decreasing = TRUE)[3]
sort(table(myDF$Origin),decreasing = TRUE)[2]
sort(table(myDF$Origin),decreasing = TRUE)[1:10]
sort(table(myDF$Dest),decreasing = TRUE)[1:10]

dim(myDF)
#names of 10 most popular airports in 2008
mostpopular <- names(sort(table(myDF$Origin),decreasing = TRUE)[1:10])
mostpopular
#check each flight to see if the orgin was one of the most popular airports
sum(myDF$Origin %in% mostpopular)
sum(myDF$Dest %in% mostpopular)
sum(myDF$Origin %in% mostpopular & myDF$Dest %in% mostpopular)

#We observed that 2363257 flights departed from the ten most popular airports.
#For the purposes of this question, treat the group of the 200 least
#popular airports according tothe number of flights having these as the origins.

#How many flights had one of these 200 least popular airports as their origin?
leastpopular <- names(sort(table(myDF$Origin))[1:200])
sum(myDF$Origin %in% leastpopular)

# Review
mostpopular
myDF$Origin[1:10]
(myDF$Origin %in% mostpopular)[1:10]
(myDF$Origin[1:10] %in% mostpopular)

# it is good to check
dim(myDF)
length(myDF)
length(mostpopular)
class(mostpopular)
class(myDF)
# here is the table of how many flights originates from each airports
table(myDF$Origin)
table(myDF$Origin)["IND"]
table(myDF$Origin)["IND","ORD","JFK"]

table(myDF$Origin)[c("IND","ORD","JFK")]

table(myDF$Origin)[mostpopular]
myairports <- c("IND","ORD","JFK")
myairports
table(myDF$Origin)[myairports]

# How many flights landed at Ronald Reagan Washington National or
# Washington Dulles Airport in 2008?
# Use just one command to get both of these counts simultaneously.
table(myDF$Dest)[c("IAD","DCA")]

# here are two ways to check the first 20
# flights and see which one departed on time or early
head(myDF$DepDelay<0 , n=20)
(myDF$DepDelay<0)[1:20]

# this tells us how many flights at each airport departed on tile or early
tapply(myDF$DepDelay<=0,myDF$Origin,sum,na.rm=TRUE)

# this is total number of flights that departed ftom each airports
table(myDF$Origin)

# we can ristricted attention to only 10 most popular airports
tapply(myDF$DepDelay<=0,myDF$Origin,sum,na.rm=TRUE)[mostpopular]
table(myDF$Origin)[mostpopular]

tapply(myDF$DepDelay<=0,myDF$Origin,sum,na.rm=TRUE)[mostpopular]/
  table(myDF$Origin)[mostpopular]

# What percentage of flights departed from IND on time or early?

tapply(myDF$DepDelay<=0,myDF$Origin,sum,na.rm=TRUE)["IND"]/
  table(myDF$Origin)["IND"]

# how many flights occur each hour
v <- table(cut(myDF$DepTime,breaks = seq(0,2400,by=100)))
head(myDF$DepTime)
head(myDF$DepTime/100)
# another way
w <- table(ceiling(myDF$DepTime/100))
w
# two methods have equal results
sum(v==w)
sum(v!=w)

plot(w)

# we can break the data in the depdelay vector
# according to which city of origin or according to the month
tapply(myDF$DepDelay,myDF$Origin,length)
tapply(myDF$DepDelay,myDF$Month,length)

# we now know how many flights occur from each airport in each month
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),length)
# we now know how many flights occur from each IND in june
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),length)["IND",6]

# notice that we need to give 2 dimensions when we extract data from a matrix
# we need to specify both the row and the column

tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("IND","AUS","ATL"),c(6,7,8,9,10,11,12)]

# OR
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("IND","AUS","ATL"),6:12]

# How many flights departed altogether from ATL, AUS, and BDL
# during the months of July 2008 through October 2008?

sum(tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("BDL","AUS","ATL"),7:10])

# all flights month by month from IND
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
           length)[c("IND"),1:12]
# OR
# make sure u put comma between rows and columns
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)["IND",]
# what about both IND and ORD
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("IND","ORD"),]
# check if it is a matrix?
class(tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
             length)[c("IND","ORD"),])
# how big is the matrix
dim(tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
           length)[c("IND","ORD"),])

#Can you create a table with 3 rows and 12 columns, 
# showing the monthly counts for 
# flights departing from ATL, ORD, and DFW?

tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("ATL","ORD","DFW"),]

# make a data frame with all of the flights that are
# delayed more than 30 min when departed
longdelayDF <- subset(myDF,myDF$DepDelay>30)
dim(longdelayDF)

tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("IND","ORD"),]
tapply(longdelayDF$DepDelay,list(longdelayDF$Origin,longdelayDF$Month),
       length)[c("IND","ORD"),]
# percentage
tapply(longdelayDF$DepDelay,list(longdelayDF$Origin,longdelayDF$Month),
       length)[c("IND","ORD"),]/
tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
       length)[c("IND","ORD"),]
# OR
aa <- tapply(longdelayDF$DepDelay,list(longdelayDF$Origin,longdelayDF$Month),
             length)[c("IND","ORD"),]
bb <- tapply(myDF$DepDelay,list(myDF$Origin,myDF$Month),
             length)[c("IND","ORD"),]

aa/bb

# we can plot this with dotchart
?dotchart
dotchart(aa/bb)

cc <- aa/bb
cc
cc["ORD",]-cc["IND",]

# How many flights departed altogether from IND or ORD in
# 2008 with a delay of more than 30 minutes each?
tapply(longdelayDF$DepDelay,longdelayDF$Origin,length)[c("IND","ORD")]
sum(tapply(longdelayDF$DepDelay,longdelayDF$Origin,length)[c("IND","ORD")])

# In which month of 2008 was the percentage of long delays 
# (i.e., flights with more than 30 minute delays) the highest?

tapply(longdelayDF$DepDelay,longdelayDF$Month,
       length)/
  tapply(myDF$DepDelay,myDF$Month,length)


# break the day into 4 parts
v <- (ceiling(myDF$DepTime/600))

# build a vector called part of day
# initially we put more than 7 million NA in it
partofday <- rep(NA,times=dim(myDF)[1])
head(partofday)
length(partofday)

partofday[v==1] <- "///early morning"
partofday[v==2] <- "///late morning"
partofday[v==3] <- "///early evening"
partofday[v==4] <- "///late evening"
table(partofday)

# length of partofday should be same as number of rows in myDF
length(partofday)
dim(myDF)
# so we can create new column
myDF$timeofday <- partofday
dim(myDF)

# How many flights departed from IND early in the morning?
EarlyMorningmyIndyOrgins <- subset(myIndyOrgins,myIndyOrgins$timeofday=="early morning")
EarlyMorningmyIndyOrgins
sum(EarlyMorningmyIndyOrgins)

dim(myIndyOrgins)
head(myIndyOrgins)
sum(myIndyOrgins$timeofday=="///early morning")
head(myDF$timeofday)
head(myDF)
myIndyOrgins <- subset(myDF,myDF$Origin=="IND")
head(myIndyOrgins)
sum(myIndyOrgins$timeofday=="///early morning")

sum(myDF$Origin=="IND" & myDF$timeofday=="///early morning", na.rm = TRUE)


-------------
  
tapply(myDF$DepDelay,list(myDF$Origin,myDF$timeofday),length)[c("IND","JFK"),]  

  




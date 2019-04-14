
# load data
df1 <- read.csv("/Users/ALI/Downloads/2006.csv")
df2 <- read.csv("/Users/ALI/Downloads/2007.csv")
df3 <- read.csv("/Users/ALI/Downloads/2008.csv")

# check dimension
dim(df1)
dim(df2)
dim(df3)
# row bine datasets
mydf <- rbind(df1,df2,df3)
dim(mydf)
# remove datasets
rm(df1,df2,df3)
head(mydf)
tail(mydf)
unique(mydf$Year)
# How many flights departed altogether from LAX during the period 2006 to 2008?
sum(mydf$Origin=="LAX")
# make a table
mytable <- table(list(mydf$Origin,mydf$Dest))
head(mytable)
dim(mytable)
sum(mytable)
sum(mytable==0)
sum(mytable!=0)
paste(mydf$Origin,mydf$Dest)
head(paste(mydf$Origin,mydf$Dest))
mynewtable <- table(paste(mydf$Origin,mydf$Dest))
length(mynewtable)
head(mynewtable)
tail(mynewtable)
# How many flights were there from IND to ORD from 2006 to 2008?
sum(mydf$Origin=="IND" & mydf$Dest=="ORD")
# Make a table that counts the number of entries (simultaneously) according
# to the origin, destination, and year. How many flights were there from BOS 
# to DEN in 2007? Use the same data frame we already built. 
# Do not reload all of the 2007 data.
head(mydf)
table(mydf$Origin=="BOS" & mydf$Dest=="DEN" & mydf$Year==2007)
sum(mydf$Origin=="BOS" & mydf$Dest=="DEN" & mydf$Year==2007)
# neither of these polts are very helpful
plot(mynewtable)
dotchart(mynewtable)
# we could try to sort the data first
dotchart(sort(mynewtable))
# we focusing on flights with IND as the orgin
plot(mytable["IND",])
dotchart(mytable["IND",])
# save that flight data into a vector
v <- mytable["IND",]
v[v!=0]
sort(v[v!=0])
# now we plot only the flights from IND to airports that at least have one flight
dotchart(sort(v[v!=0]))

dotchart(sort(v[v>4000]))

# import data about airports themselves
airportDF <- read.csv("/Users/ALI/Downloads/airports.csv")

dim(airportDF)
head(airportDF)
head(airportDF,n=50)
airportDF[airportDF$iata=="IND",]
airportDF[airportDF$iata %in% c("IND","ORD","MDW"),]

# we make a vector to store airport name, city and state
w <- paste(airportDF$airport,airportDF$city,airportDF$state, sep=", ")
w
head(w)


# we are going to make the "name" of each entry in the vector
# be the 3 letter airportcode itself
names(w) <- airportDF$iata
head(w)
w[c("IND","ORD")]

# According to the data in the airports.csv file, 
# how many airports are located in the City of Chicago?
sum(airportDF$city=="Chicago",na.rm = T)
airportDF[airportDF$city == "Chicago",]



v[v>4000]
w[names(v[v>4000])]


myvec <- v[v>4000]
names(myvec) <- w[names(v[v>4000])]

myvec
dotchart(myvec)
dotchart(sort(myvec))



head(airportDF)
table(airportDF$state)
subset(airportDF,state=="IN")
indyairports <- subset(airportDF,state=="IN")
# we can make a table shows all the flights counts
table(mydf$Origin)

indyairports$iata
class(indyairports$iata)
as.character(indyairports$iata)
table(mydf$Origin)[as.character(indyairports$iata)]
z <- table(mydf$Origin)[as.character(indyairports$iata)]
z[!is.na(z)]
names(z[!is.na(z)])

subset(airportDF, iata %in% names(z[!is.na(z)]))

# Combining the information from the airports.csv file 
# and from the 2006 to 2008 airline data sets, how many airports 
# in California had at least one flight in the data set during
# this time period (2006-2008)?

caliairports <- subset(airportDF,state=="CA")
caliairports$iata
class(caliairports$iata)
as.character(caliairports$iata)
table(mydf$Origin)[as.character(caliairports$iata)]
l <- table(mydf$Origin)[as.character(caliairports$iata)]
l[!is.na(l)]
ll <- subset(airportDF, iata %in% names(l[!is.na(l)]))
ll
count(ll,vars="airport")
#######
# let's build a function that for a state given by user will identify all of the airports
# in 2006 to 2008 data set that have commercial flights from that state


# start with "IN" as the state example

mystate <- "IN"
myairports <- subset(airportDF,state==mystate)
p <- table(mydf$Origin)[as.character(myairports$iata)]
subset(airportDF, iata %in% names(p[!is.na(p)]))
 

# now wrap it into a function
activeairports <- function(mystate){
  myairports <- subset(airportDF,state==mystate)
  p <- table(mydf$Origin)[as.character(myairports$iata)]
  subset(airportDF, iata %in% names(p[!is.na(p)]))
  
}
  
  
activeairports("IN")
activeairports("IL")
 
sapply(state.abb,function(x) dim(activeairports(x))[1])
sapply(state.abb,function(x) dim(activeairports(x))[1])[c("OK","LA","NY")]


# consider the distances flown by planes(this is the "data" in tapply)
# split up according to tailnum of the planes themselves(this is the way to split)
# and the function we use , within each group of distance is "sum"
tapply(mydf$Distance,mydf$TailNum,sum)
sort(tapply(mydf$Distance,mydf$TailNum,sum))
head(sort(tapply(mydf$Distance,mydf$TailNum,sum)))
tail(sort(tapply(mydf$Distance,mydf$TailNum,sum)))

dotchart(sort(tapply(mydf$Distance,mydf$TailNum,sum)))

h <- sort(tapply(mydf$Distance,mydf$TailNum,sum))
head(h)
h <- tail(h,23)
h
# now we have info about the 23 plane that flewthe mose miles
h
# but the last 3 are erroneous, lets remove them
h <- h[1:20]
h
planeDF <- read.csv("/Users/ALI/Downloads/plane-data.csv")
head(planeDF,200)


names(h)
?names
h
subset(planeDF,tailnum %in% names(h))
?%in%


# How many flights did the airplane with tail number N556AS make during 2006 to 2008?
#  
  millage <- function(myplane){
    myairplane <- subset(planeDF,tailnum==myplane)
    h <- table(mydf$Origin)[as.character(myairplane$tailnum)]
    subset(planeDF,tailnum %in% names(h))
    
  }
millage(N556AS)

h
subset(planeDF,tailnum %in% names(N556AS))#
table(mydf$TailNum)["N556AS"]
sum(mydf$TailNum == "N556AS")


# reality check: there should be this many days during 2006 to 2008
365+365+366
# remember that 2008 is a leap year

# handy: we have the month abbreviation in R : month.abb

month.abb
# we can use number from 1 to 12 as indices into this vector
month.abb[c(8,9,7,1,3,4,2)]

# the first six flights in our data frame mydf al fron Jan
head(month.abb[mydf$Month])

tail(month.abb[mydf$Month])

# full date format
head(paste(mydf$DayofMonth,month.abb[mydf$Month],mydf$Year))
tail(paste(mydf$DayofMonth,month.abb[mydf$Month],mydf$Year))


mydates <- paste(mydf$DayofMonth,month.abb[mydf$Month],mydf$Year)

# use tapply
# with departure delays as the data
# split data according to the value of mydates
# and the function we take(within each day) on the data is the "mean"
# of course we remove the NA values
tapply(mydf$DepDelay,mydates,mean,na.rm=TRUE)

# days with smallest average departure delays
head(sort(tapply(mydf$DepDelay,mydates,mean,na.rm=TRUE)))
# days with longest average departure delays
tail(sort(tapply(mydf$DepDelay,mydates,mean,na.rm=TRUE)))
# worst 20 days
tail(sort(tapply(mydf$DepDelay,mydates,mean,na.rm=TRUE)),20)
# most of them are in Dec

# Which date during the period 2006-2008 had the most flights?
sum(mydf$FlightNum)
tail(sort(tapply(mydf$FlightNum,mydates,length)))


 
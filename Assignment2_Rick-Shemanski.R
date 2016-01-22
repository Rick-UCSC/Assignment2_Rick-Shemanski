##Rick Shemanski
##Hw2 R lab
# 01/21/2016


library(foreign)


#create list which can be added to later as well
RickShemanskiAssignment2 <- list(
  first = "Rick",
  last = "Shemanski",
  email = "rshemans@ucsc.edu",
  studentid = 1504018
)
#############################################################################################

##1 

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"))

RickShemanskiAssignment2$s1a <- nrow(diamonds)
print(RickShemanskiAssignment2$s1a)

RickShemanskiAssignment2$s1b <- ncol(diamonds)
print(RickShemanskiAssignment2$s1b)

RickShemanskiAssignment2$s1c <- colnames(diamonds)
print(RickShemanskiAssignment2$s1c)

RickShemanskiAssignment2$s1d <- summary(diamonds$price)
print(RickShemanskiAssignment2$s1d)

######################################################################################################

##2

d2 <-read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt", sep ="\t", header = TRUE)


RickShemanskiAssignment2$s2a <- nrow(diamonds)

RickShemanskiAssignment2$s2b <- ncol(d2)  
print(RickShemanskiAssignment2$s2b)

RickShemanskiAssignment2$s2c <- colnames(d2)
print(RickShemanskiAssignment2$s2c)

RickShemanskiAssignment2$s2d<-mean(d2$weight)
  print("The mean weight is 266")
  
RickShemanskiAssignment2$s2e <- median(d2$weight)
  print("The median weight is 175")

##hist and table
  
  hist(d2$weight)
  
  table(d2$weight)

##create new column with outlier weights replaced as NA
d2$weightnew <-ifelse(d2$weight>=996&d2$weight<=999,NA,d2$weight)

##new mean and medians with na replace during calculations
RickShemanskiAssignment2$s2f <- mean(d2$weightnew, na.rm = TRUE)
print("The new adjusted mean is 174")

RickShemanskiAssignment2$s2g <- median(d2$weightnew, na.rm = TRUE)
print("The new adjusted median is 170")


#find summary of new weight column (has NA's) of men and women sperately

RickShemanskiAssignment2$s2h <-summary(d2$weightnew, d2$SEX==2)
print(RickShemanskiAssignment2$s2h)


RickShemanskiAssignment2$s2i <- summary(d2$weightnew, d2$SEX== 1)
print(RickShemanskiAssignment2$s2i)
###########################################################################################
###3

# vector composes of lowercase letters, and UPPERCASE letters
vec <- c(letters,LETTERS)
print(vec)

# store 2nd element of vector up to the 26th element by 2. (2,26,2)
RickShemanskiAssignment2$s3a <-vec[seq(2,26,2)]
print(RickShemanskiAssignment2$s3a)

#get values r i c 
RickShemanskiAssignment2$s3b <-paste(vec[c(18,9,3)], collapse="")
print(RickShemanskiAssignment2$s3b)

#Creates 3 three by three arrays and fills it with vec(letter,LETTERS) three, three, 3 of em
arr <- array(c(letters,LETTERS),dim=c(3,3,3))
print(arr)
print(vec)

# rows 1 -3 1st col, 2 nd array
RickShemanskiAssignment2$s3c <- arr[1:3,1,2]
print(RickShemanskiAssignment2$s3c)

# mids vals of arrays
RickShemanskiAssignment2$s3d <- arr[2,2,1:3]
print(RickShemanskiAssignment2$s3d)

# ric
RickShemanskiAssignment2$s3e <- paste(arr[3,3,2],arr[3,3,1],arr[3,1,1],sep="")
print(RickShemanskiAssignment2$s3e)
##################################################################################

##4

library(foreign)
d3<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

#sort ad list unqiue vals for month year and educ
sort(unique(d3$year))
sort(unique(d3$month))
sort(unique(d3$educ))


RickShemanskiAssignment2$s4<-aggregate(d3$rw,list(year=d3$year,month=d3$month,educ=d3$educ),mean,na.rm=TRUE)
print(RickShemanskiAssignment2$s4)



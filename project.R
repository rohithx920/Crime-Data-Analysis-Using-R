rm(list = ls())
library(data.table)   # for fread() 
library(sqldf)
library(bit64)
library(plyr)

library(heatmaply)
library(plotly)
library(ggvis)

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)

library("maps")
library(ggmap)
library(dplyr)


# Read the crime data
phildata <- fread("C:/Users/Rohith/Downloads/FIRST-SEM/R language/philadelphiacrimedata/crime.csv")
str(phildata)

# summarise latitude and longitude data
summary(phildata$Lon)
summary(phildata$Lat)


# checkin for NAs
sapply(phildata, function(x) sum(is.na(x)))
# note, even with 16722 NAs (missing values), it is less than ~0.8% of missing data.


# Summarise the data for crime categories
summary(phildata$Text_General_Code)

# checking for date time manipulation
phildata$dt = as.Date(phildata$Dispatch_Date)
phildata$year = as.numeric(format(phildata$dt, "%Y"))
phildata$mth = as.numeric(format(phildata$dt, "%m"))
phildata$day = as.numeric(format(phildata$dt, "%d"))

#step 1: 
#NO of crimes reported each year

require("RColorBrewer")
year_count <- table(phildata$year)
mth_count <- table(phildata$mth)
day_count <- table(phildata$day)
#bar plot to show how many crimes reported each year
barplot(year_count, col=c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),border = NA,
        xlim = NULL,
        xlab = "Years",
        ylab= "No. of Crimes")

#boxplot(phildata$year ~ phildata$Police_Districts )
#boxplot.stats(phildata$Police_Districts ~ phildata$year)
#year_count
#aggregate(phildata$year ~ phildata$mth, FUN=sum)
#mth_count

#step 2:
#Reported time
hour <- phildata$Hour
hour_frequency <- table(hour)
barplot(hour_frequency, col=c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),border = NA,
        xlim = NULL,
        xlab = "Hours(24HRS)",
        ylab= "Count")

#step 3:
#Reported dist
police_dist <- table(phildata$Police_Districts)
barplot(police_dist, col=c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),border = NA,
        xlim = NULL,
        xlab = "Dist",
        ylab= "Count")

#dist 11 reported to have more crime rates 

#step 4:
#No of different crimes Reported

Text_General_Code <- table(phildata$Text_General_Code)

# To display all the crimes  
Text_General_Code[order(Text_General_Code)]
?prop
Text_General_Code_prop <- round(prop.table(Text_General_Code),2)*100
t <- Text_General_Code_prop[Text_General_Code_prop>4]
barplot(t[order(t)],
        horiz = FALSE,
        col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,
        xlim = NULL,
        xlab = "Types of Crimes",
        ylab= "% of Crimes")
#?aggregate
#?count.fields
require("plyr")
?count


#step 5:
# type of crime happened at that point 

z <- aggregate(phildata$Hour ~ phildata$Text_General_Code, FUN=table)
z
# Theift 
l1 <- z[23,2]
l1
l2 <- as.data.frame(l1)
colnames(l2) <- c("Hour","Freq")
#l2 <- colnames(0:23)
barplot(l2[,2],col =brewer.pal(6, "YlOrRd"),
        border = NA,
        xlim = NULL,
        xpd = TRUE,
        xlab = "Hours from 0 to 23",
        ylab= "Count of that crime",
        main = "Narcotic / Drug Law Violations",
        names.arg={0:23}
        
)


#Step 6
# fig out what all possible crimes at that time period

#we want to figure out at what time most crimes happen by="hours" (we want to show it by hours) function calculates the frequenecy
#z1 will be a list of for 24 hrs
z1 <- aggregate(phildata$Text_General_Code ~ phildata$Hour, FUN=table)
# we selected 1st row and 2nd column to figure out the individual frequencies
z2 <- z1[1,2]
#converts list back to table 
z3 <- as.data.frame(z2)


#z4 <- lapply(z3, function(x)rep(x, z3$X01.Freq))  # Repeats each row by Freq
#z5 <- as.data.frame(z4)  # Converts from list back to data frame
#z6 <- z5[, -2]  # Removes fifth column with frequencies
#z7 <- table(z6)
#z8 <-as.data.frame(z7) 
#colnames(z8) <- z1[1,1]

z3
#row.names(z3) <- z3$X01.Freq
colnames(z3) <- c("Type","Freq")
#(-1) indicateds remove 1st row because it is unwanted data 
barplot(z3[-1,]$Freq,col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,
        xlim = NULL,
        xpd = TRUE,
        names.arg=z3$Type[-1],
        xlab = "Types of Crimes",
        ylab= "NO. of Crimes reported at that hour"
        
        )

#Step 7 
#yearwise crime order 

k1 <- aggregate(phildata$Text_General_Code ~ phildata$year, FUN=table)
kk1 <- as.data.frame(k1[10,2])
k111 <- round(prop.table(kk1[,2]),2)*100
k1111 <- as.data.frame(k111)
row.names(k1111) <- kk1[,1]
k112 <- k1111[k1111>4]
k113 <- as.data.frame(k112)
row.names(k113)<-k1111[,1][k1111>4] 
barplot(k112[order(k112)],
        horiz = FALSE,
        col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,
        xlim = NULL,
        xlab = "Types of Crimes",
        ylab= "% of Crimes",
        main="2010")



k11 <- nrow(k1)
k11
year1 <- 2005
i1 <- 0
i2 <- 0
for (i in 1:k11) {
  k2 <- k1[i,2]
  if(i1 ==0){
  k3 <- as.data.frame(k2)
  colnames(k3) <- c("Type","Freq")
  barplot(k3[,2],col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
          border = NA,
          xlim = NULL,
          xpd = TRUE,
          xlab = "Types of Crimes",
          ylab= "Crimes count",
          
         
  )
  }
  if(i1==1){
    k4 <- as.data.frame(k2)
    colnames(k4) <- c("Type","Freq")
    #colnames(k3) <- c("Type","Freq")
    barplot(k3[,2],col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
            border = NA,
            xlim = NULL,
            xpd = TRUE,
            xlab = "Types of Crimes",
            ylab= "Crimes count"
            
            
    )
    if(i2==0){
    k5 <- merge(k3,k4, by.x=c("Type"), by.y =c("Type"), all = TRUE)
    
    }
    else{
      k5 <- merge(k5,k4, by.x=c("Type"), by.y =c("Type"), all=TRUE)
    }
    k3 <- k4
    
    i1=i1-1
    i2=i2+1
  }
  
  i1=i1+1 
}
j
colnames(k5) <- c("Type",2006:2016)
for(j in 1:ncol(k5)){
  print(j)
  k6 <- k5[j,-1]
  k61 <- t(k6)
  colnames(k61) <- c("Count")
  barplot(k61[,1])
}


k2
#2006 data
k3 <- as.data.frame(k2)
k3
colnames(k3) <- c("Types","Freq")
k31 <- rbind(c(k))
#2007 data

k4 <- k1[2,2]
k5 <- as.data.frame(k4)
colnames(k5) <- c("Types","Freq")


#column binding k3 data with k5 data
#?cbind
#k6 <- cbind(k3,k5[0:32,],k3$X01.Var1) 
k7 <- merge(k3,k5, by.x=c("Types"), by.y = c("Types"), all = TRUE)

#Infer data(predicting 2008 data based on 2006 and 2007)

# Linear regression model

reg1 <- lm(k7$Freq.x ~ k7$Freq.y)
reg1
reg1.ma <- as.matrix(reg1)
summary(reg1)

# Confidence intervals for coefficients


confint(reg1)

# Predict values based on regression equation
predict(reg1)  # Predicted height based on girth
predicted_data <- predict(reg1, interval = "prediction")  # CI for predicted height
predicted_data
k71 <- k7$Types[1:34]
k72 <- as.data.frame(k71)
rownames(predicted_data) <-k72 



barplot(k3$X01.Freq,col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,
        xlim = NULL,
        xpd = TRUE,
        xlab = "2006:Types of Crimes",
        ylab= "Crimes count",
        
)

#step 8
#district with crime activities

j1 <- aggregate(phildata$Text_General_Code ~ phildata$Police_Districts, FUN=table)
j2 <- j1[1,2]
j2
j3 <- as.data.frame(j2)
j3
barplot(j3$X01.Freq,col =c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,
        xlim = NULL,
        xpd = TRUE,
        xlab = "Dist with more crime activities",
        ylab= "Crimes count",
        
)

#step 9

sample <- phildata[1:100,]
sample
sample1 <- sample$Hour[sample$Hour == 0]
sample1
x <- aggregate(sample$Text_General_Code ~ sample$Hour,FUN=table)
x
y <-x[1,2]
y
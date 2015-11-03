#Question 1 
flights <- read.table('D:/ACADS/Data Analysis and Methods/FAA_r.csv',header=TRUE,sep = ',')
attach(flights)
#2nd Question
length(names(flights))
names <-names(flights)

#3rd Question
Airbus<-flights[which(type=="Airbus"),]
#4th Question
subflights<-flights[c(2:7)]
colMeans(subflights,na.rm = TRUE,dims = 1)
apply(subflights, 2,sd,na.rm = TRUE)
#Question 5

par(mfrow=c(2,3))

for(i in 1:length(subflights))
{
  main<-paste("Histogram of", names[i+1])
  xLabel <- paste(names[i+1])
  hist(subflights[,i],xlab = xLabel,main = main)
  
}
#Question 6
#Total NA in flights$speed_air

apply(is.na(flights),2,sum)

#Total non NA in flights$speed_air

apply(!(is.na(flights)),2,sum)

proportion <- apply(is.na(flights),2,sum)[5]/apply(!(is.na(flights)),2,sum)[1]
print(proportion)
NA_speed_air <- flights[which(flights$speed_ground<87),]
print(NA_speed_air)
dim(NA_speed_air)

#Question 7

flights$speed_airflow <- (flights$speed_air - flights$speed_ground)
Average_Speed_airflow<-mean(abs(flights$speed_airflow),na.rm = TRUE)
Average_Speed_airflow
#Question 8
sum(flights$duration<40)
sum(flights$height<6)

flights <- flights[-c(c(which(flights$height<6)),which(flights$duration<40)),]
dim(flights)
#Question 9
flights_airbus <- flights[which(flights$type=="Airbus"),]
flights_Boeing <- flights[which(flights$type=="Boeing"),]

#Question 10
summary(flights_airbus)[c(1,3,4,6),c(4,5,6,7)]
summary(flights_Boeing)[c(1,3,4,6),c(4,5,6,7)]


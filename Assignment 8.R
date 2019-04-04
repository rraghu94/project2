
#1 all parts
pleasant_score <- function(x) #create function pleasant_score
  {  
    city = 0 #set city sore to be zero
    if(as.numeric(x[1])>46){city = city +9} #if value in education is greater than 46 add 9 to city
    else if(as.numeric(x[1]) > 20 ){city = city + 1} #if value in education row is greater than 20, add 1 to city 
  #healthcare
    if(as.numeric(x[10]) < 15) {city = city + 6}
    else if(as.numeric(x[10]) >12.5 ){city = city + 3}
  #poverty
  if(as.numeric(x[11]) > 22) {city = city + 5}
  else if(as.numeric(x[11]) < 16 ){city = city + 3}  
  
  
  return(city) #return the value of city after going through conditions
  }  

Best_Cities <- read.csv("C:/Users/rohit/Desktop/ds710spring2018assignment8/Best Cities.csv") #read in best cities csv
Best_Cities = data.frame(t(Best_Cities)[-1,]) #transpose the data
apply(Best_Cities,1,pleasant_score) #use apply function to determine scores





#2

Testing_Hancock <- function(s1,s2,n1,n2,mean1,mean2)
{
  mean1 <- mean(RandomSample1)
  mean2 <- mean(RandomSample2)
  s1 <- sd(RandomSample1)
  s2 <- sd(RandomSample2)
  n1 <- length(RandomSample1)
  n2 <- length(RandomSample2)
  
  standard_error = sqrt((((s1)^2)/n1)+(((s2)^2)/n2))
  t = (mean1-mean2)/(sqrt((((s1)^2)/n1)+(((s2)^2)/n2)))
  DoF = (((((s1)^2)/n1)+(((s2)^2)/n2))^2) / ((((((s1)^2)/n1)^2)/(n1-1)) + (((((s2)^2)/n2)^2)/(n2-1)))
  p_value = 2*pt(-abs(t), DoF)
  return(p_value)
  
}

RandomSample1 = rnorm(100,2,1)
RandomSample2 = rnorm(100,2.5,3)


Testing_Hancock(RandomSample1,RandomSample2)

t.test(RandomSample1, RandomSample2)



#2e

mourner_essay <- c(3, 7, 8, 3, 7, 3, 3, 6, 2, 3, 3, 2, 3, 4, 3, 8, 10, 2, 3, 3, 7, 4, 2, 10, 6, 3, 4, 9, 3, 6, 4, 2, 4, 2, 6, 4, 3, 8, 5, 2, 5, 4, 8, 11, 2, 6, 4, 4, 3, 3, 7, 2, 7, 3, 4, 2, 11, 2, 6, 5, 4, 8, 2, 3, 7, 2, 4, 6, 4, 3, 5, 6, 2, 3, 5, 10, 5, 6, 5, 4, 8, 8, 8, 2, 3, 8, 7, 2, 3, 6, 3, 6, 2, 3, 9, 3, 6, 4, 3, 3, 7, 3, 5, 2, 9, 3, 8, 8, 2, 6, 4, 3, 4, 5, 2, 3, 3, 4, 2, 7, 5, 6, 8, 4, 3, 7, 6, 6, 5, 2, 3, 6, 12, 7, 6, 2, 5, 5, 5, 6, 2, 5, 2, 3, 1, 7, 6, 3, 5, 4, 4, 1, 6, 4, 7)
mean(mourner_essay)
sd(mourner_essay)

Testing_Hancock(2.32,2.60,length(mourner_essay),121,4.70,4.69)
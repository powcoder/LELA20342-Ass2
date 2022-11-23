#Get and set working directory:
getwd()
#remember that you'll need to change the Windows \ to /
setwd('/home/wendell/Dropbox (The University of Manchester)/Teaching/20232/Week 5')
#always remember to check this afterwards!
getwd()

#Read in a file:
n = read.csv("nasals.csv")
#And look at the beginning of it!
head(n)

#Generate a histogram for the overal dataset for the quarter point:

hist(n$vquarter, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="cornflowerblue"
)

#And then for the mid point:

hist(n$vmidpoint, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="mediumpurple"
)

#Quarter point histogram for nasal codas, with mean and median lines:

nas = n[n$coda=="nasal",]

hist(nas$vquarter, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="gray"
)
abline(v=mean(nas$vquarter),col="darkred")
abline(v=median(nas$vquarter),col="darkblue")

#Mid point histogram for nasal codas:

hist(nas$vmidpoint, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="darkgray"
)
abline(v=mean(nas$vmidpoint),col="darkred")
abline(v=median(nas$vmidpoint),col="darkblue")


#Quarter point histogram for oral codas:

or = n[n$coda=="oral",]

hist(or$vquarter, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="lightsteelblue"
)
abline(v=mean(or$vquarter),col="darkred")
abline(v=median(or$vquarter),col="darkblue")

#Mid point histogram for oral codas:

hist(or$vmidpoint, 
     breaks=15,
     main="Nasal Airflow at Vowel Quarter Point", 
     xlab="Airflow (ml/s)",
     col="lightsteelblue4"
)
abline(v=mean(or$vmidpoint),col="darkred")
abline(v=median(or$vmidpoint),col="darkblue")

#QQ Plots

#Nasal, quarter point
qqnorm(nas$vquarter)
qqline(nas$vquarter)

#Nasal, midpoint
qqnorm(nas$vmidpoint)
qqline(nas$vmidpoint)

#Oral, quarter point
qqnorm(or$vquarter)
qqline(or$vquarter)

#Oral, mid point
qqnorm(or$vmidpoint)
qqline(or$vmidpoint)

#Shapiro-Wilk Tests

#Nasal, quarter point
shapiro.test(nas$vquarter)
  #yes, normally distributed.

#Nasal, mid point
shapiro.test(nas$vmidpoint)
  #no, not normally distributed

#Oral, quarter point
shapiro.test(or$vquarter)
  #No, not normally distributed

#Oral, mid poi8nt
shapiro.test(or$vmidpoint)
  #Yes, normally distributed

#95% Confidence Intervals
library(Rmisc)
ci.or.q = CI(or$vquarter,0.95)
ci.nas.q = CI(nas$vquarter,0.95)
ci.or.mid = CI(or$vmidpoint,0.95)
ci.nas.mid = CI(nas$vmidpoint,0.95)

#Box plot, quarter point nasal vs. oral
boxplot(n$vquarter ~ n$coda,
        main="Vowel Quarter Point by Coda Type",
        ylab="Nasal Airflow (ml/s)",
        col=c("orange1","royalblue3"))
abline(h=ci.or.q[1],col="darkgray",lty=2)
abline(h=ci.or.q[3],col="darkgray",lty=2)
abline(h=ci.nas.q[1],col="darkgray",lty=2)
abline(h=ci.nas.q[3],col="darkgray",lty=2)

#Box plot, mid point nasal vs. oral
boxplot(n$vmidpoint ~ n$coda,
        main="Vowel Mid Point by Coda Type",
        ylab="Nasal Airflow (ml/s)",
        col=c("orange1","royalblue3"))
abline(h=ci.or.mid[1],col="darkgray",lty=2)
abline(h=ci.or.mid[3],col="darkgray",lty=2)
abline(h=ci.nas.mid[1],col="darkgray",lty=2)
abline(h=ci.nas.mid[3],col="darkgray",lty=2)


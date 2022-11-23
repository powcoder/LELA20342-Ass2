#####PRACTICE FROM LAST WEEK#####

#Get and set working directory:
getwd()
  #remember that you'll need to change the Windows \ to /
setwd('/home/wendell/Dropbox (The University of Manchester)/Teaching/20232/Week 3')
  #always remember to check this afterwards!
getwd()

#Read in a file:
v = read.csv("vot.csv")
  #And look at the beginning of it!
head(v)

#Get a summary of a single column (vector)
summary(v$vot)

#Get mean, standard deviation, median, and IQR
mean(v$vot)
sd(v$vot)

median(v$vot)
IQR(v$vot)

#Make a basic box plot
boxplot(v$vot)


######HISTOGRAM#######

#Create a basic histogram
hist(v$vot)

#Play with the breaks in your histogram
hist(v$vot, breaks=3)
hist(v$vot, breaks=30)
hist(v$vot, breaks=30)

#Change the histogram from count/frequency to density
hist(v$vot, breaks=30, freq=F)

#Change the colour of your histogram
hist(v$vot, breaks=30, col="gray")
hist(v$vot, breaks=30, col="seagreen3")
hist(v$vot, breaks=30, col="#380B61")

#Change the axis and title labels
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)")
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")

#Add a vertical line with the mean
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(v$vot))
?abline

#Change the colour of your line.
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(v$vot),col="darkred")

#Change the type of your line.
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(v$vot),col="darkred",lty=2)

#Change the thickness of your line.
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(v$vot),col="darkred",lty=2, lwd=3)

#Add another verical line with the median
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(v$vot),col="darkred",lty=2, lwd=3)
abline(v=median(v$vot),col="darkblue",lty=2,lwd=3)

#Select a subset of your data!
vd = v[v$voicing=="voiced",]
head(vd)
mean(vd$vot)

vl = v[v$voicing=="voiceless",]
head(vl)
mean(vd$vot)

#Create a histogram with separte lines for your two means
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(vd$vot),col="cornflowerblue",lty=2,lwd=3)
abline(v=mean(vl$vot),col="seagreen",lty=2,lwd=3)
  #And add some text!
text(0,25,"voiced")
text(70,30,"voiceless")

#Change the colour of your text
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(vd$vot),col="cornflowerblue",lty=2,lwd=3)
abline(v=mean(vl$vot),col="seagreen",lty=2,lwd=3)
text(0,25,"voiced",col="cornflowerblue")
text(70,30,"voiceless",col="seagreen")

#Change the size of your text
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")
abline(v=mean(vd$vot),col="cornflowerblue",lty=2,lwd=3)
abline(v=mean(vl$vot),col="seagreen",lty=2,lwd=3)
text(0,25,"voiced",col="cornflowerblue",cex=1.5)
text(70,30,"voiceless",col="seagreen",cex=1.5)


#######CHECKING FOR NOMRALITY: HIST######

#Create a histogram for voiced plosives.  Does it look normal so far?
hist(vd$vot)
abline(v=mean(vd$vot),col="red")
abline(v=median(vd$vot),col="blue")
  #pretty close, maybe a little skewed

#Create a histogram for voiceless plosives.  Does it look normal so far?
hist(vl$vot)
abline(v=mean(vl$vot),col="red")
abline(v=median(vl$vot),col="blue")
  #pretty close



########QQ PLOTS########

#Create a quantile-quantile plot for voiced plosives.  Does it look normal so far?
qqnorm(vd$vot)
qqline(vd$vot, col="darkred")
  #A little wobbly, but fairly close.

#Create a quantile-quantile plot for voiceless plosives.  Does it look normal so far?
qqnorm(vl$vot)
qqline(vl$vot,col="darkred")
  #Looking pretty good


########SHAPIRO TESTS#####

#Conduct Shapiro-Wilk Tests for voiced and voiceless plosives.  Are they normal?
shapiro.test(vd$vot)
  # p > 0.05, we can't reject the null: we're normal
shapiro.test(vl$vot)
  # p > 0.05, we can't reject the null: we're normal


######BOXPLOTS#######

#Create a boxplot comparing voiced and voiceless plosives.  Do they look different?
boxplot(v$vot ~ v$voicing)
  #Yep!

#Create a boxplot comparing plosives across place of articulation.  Do they look different?
boxplot(v$vot ~ v$place)
  #Nope!

#Change the colors of your voicing boxplot
boxplot(v$vot ~ v$voicing, col=c("cornflowerblue", "seagreen"))

#Change the axis labels and title
boxplot(v$vot ~ v$voicing, col=c("cornflowerblue", "seagreen"), xlab="Voicing", ylab="Voice Onset Time (VOT)", main="VOT of Voiced and Voiceless Stops")

#Export your graph!

#######CONFIDENCE INTERVALS########

#Calculate the standard error for the VOT of voiced and voiceless plosives
sd(vd$vot)/sqrt(length(vd$vot))
sd(vl$vot)/sqrt(length(vl$vot))

#An easier way: the std.error() function from the plotrix package
library(plotrix)
  #If you don't have it already, install it:
#install.packages("plotrix")
#Calculate the standard error of the VOT of voiced and voiceless plosives. 
sem.vd = std.error(vd$vot)
sem.vl = std.error(vl$vot)

#Calculate 95% conficence intervals.  Do they overlap?
ci.vd = 1.96*sem.vd
ci.vl = 1.96*sem.vl
mean(vd$vot) - ci.vd
mean(vd$vot) + ci.vd
mean(vl$vot) - ci.vl
mean(vl$vot) + ci.vl
  #...they do not.

#An easier way!  The CI() function from the Rmisc package
library(Rmisc)
#install.packages("Rmisc")
#Calculate 95% confidence intervals.  Do they overlap?
ci.vd = CI(vd$vot,0.95)
ci.vl = CI(vl$vot,0.95)

#Select just one value from the result
ci.vd[1]

#Add them to your histogram...
hist(v$vot, breaks=30, col="gray", xlab="VOT (ms)", main="Histogram of Voice Onset Time (VOT)")

abline(v=ci.vd[1], col="cornflowerblue",lty=2)
abline(v=ci.vd[2], col="cornflowerblue",lty=3)
abline(v=ci.vd[3], col="cornflowerblue",lty=2)

abline(v=ci.vl[1],col="seagreen",lty=2)
abline(v=ci.vl[2],col="seagreen",lty=3)
abline(v=ci.vl[3],col="seagreen",lty=2)


#######EXERCISE###########
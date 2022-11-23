#Get and set working directory:
getwd()
#remember that you'll need to change the Windows \ to /
setwd('/home/wendell/Dropbox (The University of Manchester)/Teaching/20232/Week 6')
#always remember to check this afterwards!
getwd()


#### EXERCISE 1 #####
#Read in the file:
like = read.csv("like.csv")
#and check it:
head(like)
#Create a contingency table with the quotative verb and the sex of the speaker
like.table = table(like$sex, like$VQ)

#Create a barplot (with appropriate axis labels, title, colours, and a legend)
barplot(like.table,beside=T,
        xlab="Quotative",
        ylab="Count",
        main="Quotative Verbs by Speaker Sex",
        col=c("lightpink","lightblue"),
        legend=rownames(like.table)
)

#Extra: Because our codes here aren't the most user-friendly, we can manually change their labels...
barplot(like.table,beside=T,
        xlab="Quotative",
        ylab="Count",
        main="Quotative Verbs by Speaker Sex",
        col=c("lightpink","lightblue"),
        legend=rownames(like.table),
        names.arg=c("","be all", "be like","be all like","go","","say","other","zero")
)
#Notice that we've got some observations that don't fit into our labelled categories.  It's a good idea to remove those.

#Conduct a Chi Squared test
chisq.test(like.table)
  #The warning message here is because we have some cells with very few expected observations.
  #It's not the end of the world, but we should be a bit cautious about our interpretation.

##### EXERCISE 2 ######

#read in the file
flap = read.csv("flap.csv")
#and look at it
head(flap)

#Conduct tests for normality for each set of observations
vd = flap[flap$voiced_p=="1",]
vl = flap[flap$voiced_p=="0",]

hist(vd$v_dur)
  #a little skewed, but OK
qqnorm(vd$v_dur)
qqline(vd$v_dur)
  #not too far off,
shapiro.test(vd$v_dur)
  #we've got a departure from normality.

hist(vl$v_dur)
  #a little skewed, but pretty good
qqnorm(vl$v_dur)
qqline(vl$v_dur)
  #a little wonky at the edges, but not bad
shapiro.test(vl$v_dur)
  #we've got a departure from normality.

#Create a boxplot with colours, axis labels, and a title
boxplot(flap$v_dur ~ flap$voiced_p,
        xlab="Voicing",
        main="Vowel Duration Before Neutralised Stops",
        ylab="Duration (ms)",
        col=c("forestgreen","steelblue"))

#conduct a two-tailed t-test
t.test(flap$v_dur ~ flap$voiced_p)

#conduct a one-tailed t-test
t.test(flap$v_dur ~ flap$voiced_p, alternative="less")
  #note: 0 is voiceless and 1 is voiced.

#conduct a wilcoson rank sum test
wilcox.test(flap$v_dur ~ flap$voiced_p)

#based on your tests for normality, is a t-test appropriate?
#Because we have a large sample, we can tolerate a bit of departure from normality, so a t-test is OK.

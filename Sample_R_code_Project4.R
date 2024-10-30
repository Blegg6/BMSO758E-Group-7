workdir <- "C:\Users/mwedel/Dropbox/Courses/BMSO758E/Projects/Project4"
setwd(workdir)

install.packages('flexmix')
library(flexmix)

proj4 <- read.csv("EURetail.csv")
attach(proj4)
str(proj4)

#Mixture regression for 1-5 segments, with 5 runs for each
#A single run would be the following command:
#out2.1 <- flexmix(Store_Image ~ Service+Atmosphere+Price|Region,data= proj4,k=2)

out1 <- stepFlexmix(Store.Image ~ Service+Atmosphere+Price|Region,data= proj4,
 control = list(verbose = 0), k = 1:5, nrep = 5)
out1

#Pick the best model in terms of BIC
out2.1<-getModel(out1,"BIC")
summary(out2.1)

#Get the coefficients of Segment 1 and 2
out2.2<-refit(out2.1)
summary(out2.2)

#Save the posterior probabilities
pr<-posterior(out2.1)
post1<-tapply(pr[,1],Region,mean)
post2<-tapply(pr[,2],Region,mean)
post=cbind(post1,post2)

#Calculate segment sizes and Entropy (Es)
pi=colMeans(post)
Es=1-mean(-post*log(post))
pi
Es

#Cross-tabulate segments with countries
lab=levels(as.factor(Country))
segments<-tapply(clusters(out2.1),Region,min)
countries<-tapply((factor(Country,ordered=TRUE)),Region, min)
countries<-factor(countries,levels=c(1:7),labels=lab)
table(segments,countries)

#Print posterior probabilities by region
Region=as.factor(Region.Name)
lab2=levels(Region)
regions<-tapply((factor(Region,ordered=TRUE)),Region, min)
regions<-factor(regions,levels=c(1:length(lab2)),labels=lab2)
print(cbind(regions,post))

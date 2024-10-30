workdir <- "C:/Users/mwedel/Dropbox/Courses/BMSO758E/Projects/Project1"
setwd(workdir)
install.packages('data.table')
install.packages('car')
library(car)

tropic1 <- read.csv("tropic.csv")
attach(tropic1)
str(tropic1)

#Log Transformation of Variables Price and Quant
log.price <-log(Price)
log.quant <- log (Quant)

#Quarter Variables Creation 
library(data.table)
qrt1 <- ifelse((between(Week,1,13) | between(Week,53,65)), 1,0)
qrt2 <- ifelse((between(Week,14,26) | between(Week,66,78)),1,0)
qrt3 <- ifelse( between(Week, 27,39) | between(Week,79,91), 1, 0)
qrt4 <- ifelse( between(Week, 40,52) | between(Week,92,104), 1, 0)

# End9 Variable Creation
End9 <- 1*(round(10*(10*Price-floor(10*Price)))==9)

#Create Factor Variables
Season=as.factor(qrt1+2*qrt2+3*qrt3+4*qrt4)
store=as.factor(Store)
end9=as.factor(End9)

# Frequency of deal and store
table(Deal,store)
table(end9,store)

# Descriptive Statistics of Sales and Price
summary(Quant)
summary(Price)

#Diagnostic Plots
par(mfrow=c(2,2))
hist(Quant,main="Histogram sales quantity")
qqPlot(Quant,main="QQ plot sales quantity")
qqline(Quant,col="red",lwd=2)
hist(log.quant,main="Histogram log sales quantity")
qqPlot(log.quant,main="QQ plot log sales quantity")
qqline(log.quant,col="red",lwd=2)

# Average Sales Volume by Store
tapply(Quant,store, mean)

# Correlation of Sales, Price and Week
cor(tropic1,method= "pearson")

#Regression 
#Linear Regression
reg <- lm( Quant ~ Price+Deal,data = tropic1)
summary(reg)
qqPlot(reg)
outlierTest(reg)
leveragePlots(reg)
vif(reg)
durbinWatsonTest(reg)

#Log-Log Model
reg1 <- lm(log.quant ~ log.price+Deal, data = tropic1)
summary(reg1)
#Log-Log Models
reg2 <- lm(log.quant ~ log.price+Deal+Season, data = tropic1)
summary(reg2)
#Semi-Log Models
reg3 <- lm(log.quant ~ Price+Deal, data = tropic1)
summary(reg3)
#Semi-Log Models
reg4 <- lm(log.quant ~ Price+Deal+Season, data = tropic1)
summary(reg4)



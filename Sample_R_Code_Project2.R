workdir <- "C:/Users/miche/Desktop/BMSO758E(2020)/Project2"
setwd(workdir)
install.packages('car')
library(car)

project2 <- read.csv("printads.csv")
str(project2)
attach(project2)

# Mean - Descriptive Stats 
summary(cbind(brand_fix,pic_fix,brand_size,pic_size,page_num))

# Correlation - cor(x, method = "pearson", use = "complete.obs")
# the usual correlation does not work here as there are factor variables in the data
# sapply() function has to be used to identify the factor variables in the model
project2$brand=as.factor(project2$brand)
cor(project2[sapply(project2, function(x) !is.factor(x))])

# Frequency - List of all Brand Names
table(brand)

# Poisson Regression
# BRAND_FIX - Fixation Counts of the Brand Element
model1 <- glm(brand_fix ~ brand_size+page_pos+page_num , poisson(link="log"))
summary(model1)
BIC(model1)
vif(model1)
step(model1,k=log(nrow(project2)))
# PIC_FIX - Fixation Counts of the Pic Element
model2 <- glm(pic_fix ~ pic_size+page_pos+page_num , poisson(link = "log"))
summary(model2)
# RECALL_ACCU - Binary Logit Model
model3 <- glm(recall_accu ~ brand_fix+pic_fix+page_pos+page_num ,binomial(link = "logit"))
summary(model3)
model6=step(model3,k=log(nrow(project2)))

# prediction graphs for pictorial and brand fixations
model4 <- glm(recall_accu ~ brand_fix ,binomial(link = "logit"))
xbr_fix=seq(0,max(brand_fix),1)
y_mem4=predict(model4,list(brand_fix=xbr_fix),type="response")
model5 <- glm(recall_accu ~ pic_fix,binomial(link = "logit"))
xpc_fix=seq(0,max(pic_fix),1)
par(mfrow=c(2,1))
plot(brand_fix,recall_accu,pch=16,xlab="brand fixations",ylab="memory")
lines(xbr_fix,y_mem4,col='red',lwd=2)
y_mem5=predict(model5,list(pic_fix=xpc_fix),type="response")
plot(pic_fix,recall_accu,pch=16,xlab="picture fixations",ylab="memory")
lines(xpc_fix,y_mem5,col='red',lwd=2)



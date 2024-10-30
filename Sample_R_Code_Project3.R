workdir <- "C:/Users/mwedel/Documents/My Dropbox/Courses/BUDT758M1 (2019)/Projects/Project3"
setwd(workdir)
install.packages('data.table')
library(data.table)
install.packages("mlogit")
library("mlogit")

project <- read.csv("deterg.csv")
attach(project)

# Average of Regular Price
avg_rp <- ((regpr1+regpr2+regpr3+regpr4)/4)
project$averageregpr <- avg_rp

# Average of Price Cut
avg_pc <-((pcut1+pcut2+pcut3+pcut4)/4)
project$averagepcut <- avg_pc

tot_disp <- disp1+disp2+disp3+disp4;
tot_feat <- feat1+feat2+feat3+feat4;

#Dummy Variables for Display, Feature
project$cat_disp <- ifelse((tot_disp != 0),1,0)
project$cat_feat <- ifelse((tot_feat != 0),1,0)
attach(project)
str(project)

# Summary statistics of the Variables
summary(cbind(regpr1,regpr2,regpr3,regpr4,pcut1,pcut2,pcut3,pcut4,disp1,disp2,disp3,disp4,feat1,feat2,feat3, feat4,avg_rp,avg_pc, cat_disp,cat_feat))

#Frequency of the Variable incid
table(incid)

# Frequency of the Variable incid with respect to choice
table(choice,incid)

#Binary Logit Model - Purchase Incidence
model1<-glm(incid ~ averageregpr + averagepcut + cat_disp +cat_feat + lbpromot,data = project, family =  binomial)
summary(model1)
BIC(model1) # BIC function in stats package)

#Brand Choice Model
proj4 <- read.csv("choice_det.csv")
attach(proj4)
str(proj4)

#Three brand specific dummy variable for estimating the intercepts
proj4$intcpt1 <- ifelse((brand ==1),1,0)
proj4$intcpt2 <- ifelse((brand ==2),1,0)
proj4$intcpt3 <- ifelse((brand ==3),1,0)

choice_data <- mlogit.data(proj4,choice = "decision", id.var = "panid", alt.var = "brand", shape = "long")

model2 <-  mlogit(decision ~ intcpt1 + intcpt2 + intcpt3 + regpr + pcut + disp + feat | -1,
                     data = choice_data, print.level = 1)
summary(model2)
log.Likelihood <- logLik(model2)
AIC=-2*log.Likelihood+2*7
BIC=-2*log.Likelihood+log(nrow(proj4))*7
cbind(log.Likelihood,AIC,BIC)

#Purchase Quantity Models
#Subset of Project where Choice = 1
temp1 <- subset(project,choice == 1)
temp1$logvol1 <- log(temp1$volume)

summary(lm(logvol1 ~ avol + regpr1 + pcut1 + lbpromot,data = temp1))
summary(lm(logvol1 ~ avol + regpr1 + pcut1 + disp1 + feat1 + lbpromot,data = temp1))

#Subset of Project where Choice = 2
temp2 <- subset(project, choice ==2)
temp2$logvol2 <- log(temp2$volume)
summary(lm(logvol2 ~ avol + regpr2 + pcut2 + lbpromot,data = temp2))
summary(lm(logvol2 ~ avol + regpr2 + pcut2 + disp2 + feat2 + lbpromot,data = temp2))

#Subset of Project where Choice = 3
temp3 <- subset(project, choice ==3)
temp3$logvol3 <- log(temp3$volume)
summary(lm(logvol3 ~ avol + regpr3 + pcut3 + lbpromot,data = temp3))
summary(lm(logvol3 ~ avol + regpr3 + pcut3 + disp3 + feat3 + lbpromot,data = temp3))

#Subset of Project where Choice = 4
temp4 <- subset(project, choice ==4)
temp4$logvol4 <- log(temp4$volume)
summary(lm(logvol4 ~ avol + regpr4 + pcut4 + lbpromot,data = temp4))
summary(lm(logvol4 ~ avol + regpr4 + pcut4 + disp4 + feat4 + lbpromot,data = temp4))

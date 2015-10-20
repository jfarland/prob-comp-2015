"""
R Code for Exacebator modeling
Author: Jonathan Farland
Date: 11/25/2014
Incredible References:
(1) Introduction to GLM's in R - http://statmath.wu.ac.at/courses/heather_turner/glmCourse_001.pdf
(2) Amazing LATTICE Reference -  http://www.isid.ac.in/~deepayan/R-tutorials/labs/04_lattice_lab.pdf
"""

#include packages
library("date")
library("lattice")
library("forecast")
library("XLConnect")
library("gdata")
library("quantreg")
library("corrgram")
library("splines")
library("plyr")

library("date")
library("lattice")
library("forecast")


#read data

train0= read.csv("~/Documents/Research/Project - Exacerbation Modeling/Data/CAX_ExacerbationModeling_TRAIN_data.csv")
test0 = read.csv("~/Documents/Research/Project - Exacerbation Modeling/Data/CAX_ExacerbationModeling_Public_TEST_data.csv")

test = data.frame(test0)

train = data.frame(train0)
summary(train)
#attach(train)
hist(train$Exacebator, col="red")
hist(train$V3, col="blue")
hist(train$V2, col="blue")

scatterplot <- function(var){xyplot(Exacebator ~ var, data=train)}
scatterplot(V4)
xyplot(Exacebator ~ V2, data=train )

"""
Logit Model
"""


logit1 <- glm(Exacebator~0+V2, data = train, family = "binomial", na.action=na.omit)
summary(logit1)
plot(predict(logit1)~fitted(logit1, xlab=expression(hat(y)[i]), ylab=expression(bar(y)[i]))) 
abline(0,0,lty=2)
summary(logit1$resid)
hist(logit1$resid)
hist(logit1$fitted.values,col="red",xlab="Fitted Values",main="Histogram of Fitted Values")
xyplot(logit1$fitted.values~ logit1$resid, data = logit1, col='black')
data_check <- cbind(train$sid, logit1$fitted.values)
histogram(check)

mat1 = as.matrix(test[c("V2")])#, "V3" ,"V4")])
mat2 = logit1$coefficients
mat3 = exp(as.data.frame(mat1 %*% mat2))

hist(mat3$V1)

write.csv(mat3,file="~/Documents/Research/Project - Exacerbation Modeling/Data/Test_Output.csv")

"""
end of real code
"""
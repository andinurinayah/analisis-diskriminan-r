#Panggil library yang diperlukan dan pastikan sudah install library tersebut
library(caret)
library(MASS)
library(klaR)
library(MVN)
library(biotools)
library(candisc)
library(DFA.CANCOR)

#import data
data <- read.csv(file.choose(),header = T) 
#Multivariate Normality Test
nm.mardia.test <- mvn(data[,1:2], mvnTest = "mardia",
                      multivariateOutlierMethod = "adj",showNewData = F)
nm.mardia.test
#Homogenity of Covariance Matrix Test
factor(data[,3])
hc.test <- boxM(data[1:2],data[,3])
hc.test
#Correlation dan multikolinearity check
kor<-cor(data[,1:2])
kor
#Wilk Lamda Test
x<-as.matrix(data[,1:2])
x.manova<-manova(x~data[,3])
x.manova
x.wilks<-summary(x.manova,test="Wilks")
x.wilks

#Canonical Correlation (CC)
cc<-candisc(x.manova)

##LINEAR DISCRIMINANT ANALYSIS
#Fit the model
model <- lda(Status~., data = data)
model

#Make predictions
predictions <- predict(model,data)
predictions

#Prediction result
predictions.result=predictions$class
predictions.result=cbind(predictions.result)
real.data=cbind(data$Status)
out=data.frame(real.data,predictions.result)
out

#Confussion table
confusion.lda<-table(data$Status,predict(model)$class) 
confusion.lda
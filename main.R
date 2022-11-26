setwd("~/facebook metrics project")
# retrieve the dataset
data <- read.csv("dataset_Facebook.csv",sep = ";")
View(data)
# data manipulation
library(dplyr)
dataset <- data %>% select(Lifetime.Post.Total.Impressions,
                           Page.total.likes,
                           Type,
                           Post.Weekday,
                           Paid,
                           comment,
                           like,
                           share)
# data structure
str(dataset)
dataset$Type <- as.factor(dataset$Type)
dataset$Post.Weekday <- as.factor(dataset$Post.Weekday)
# transform variables
dataset$Lifetime.Post.Total.Impressions <- log(dataset$Lifetime.Post.Total.Impressions)
dataset$Page.total.likes<- log(dataset$Page.total.likes)
# linear regression model
model <- lm(Lifetime.Post.Total.Impressions~.,
            data = dataset)
summary(model)
b0<-coef(model)[1]
b0
b1<-coef(model)[2];b1
a<-data.frame(Page.total.likes<-139441,Type<-"Photo",Post.Weekday<-"5",Paid<-1,comment<-145,like<-100,share<-200)
predictions<-predict(model,a);predictions
plot(model,abline(lm(Lifetime.Post.Total.Impressions~.,data = dataset)))

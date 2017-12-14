#Car Project
library (caret)
dataset <- read.csv(file="car.csv", header=F)

#Giving the header names to data
colnames(dataset) <- c("Buying",
                       "maint",
                       "doors",
                       "persons",
                       "lug_boot",
                       "Safety",
                       "Quality")
dataset$Quality <- ordered(dataset$Quality,levels=c("unacc", "acc", "good", "vgood"))
dataset$Buying <- ordered(dataset$Buying,levels=c("low", "med", "high", "vhigh"))
dataset$maint <- ordered(dataset$maint,levels=c("low", "med", "high", "vhigh"))
dataset$doors <- ordered(dataset$doors,levels=c("2","3", "4", "5more"))
dataset$persons <- ordered(dataset$persons,levels=c("2", "4", "more"))
dataset$lug_boot <- ordered(dataset$lug_boot ,levels=c("small", "med", "big"))
dataset$Safety <- ordered(dataset$Safety,levels=c("low", "med", "high"))

levels(dataset$Quality)
levels(dataset$Buying)
levels(dataset$maint)
levels(dataset$doors)
levels(dataset$persons)
levels(dataset$lug_boot)
levels(dataset$Safety)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Quality, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

#Summarize

dim(dataset)
sapply(dataset,class)
head(dataset)
head(dataset,3)

# summarize the class distribution
percentage <- prop.table(table(dataset$Quality)) * 100
cbind(freq=table(dataset$Quality), percentage=percentage)

#visualize the data through plots

#Univariate plots
x <- dataset[,1:6]#input
y <- dataset[,5]#output

# boxplot for each attribute on one image
par(mfrow=c(1,6))
for(i in 1:6) {
  plot(x,y)
}


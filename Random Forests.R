mywine<-read.table(file.choose(), header = TRUE)
mytestingwine<-read.table(file.choose(), header = TRUE)
mywineNORM<-mywine
mytestingwineNORM<-mytestingwine
for(i in 1:length(colnames(mywine))-1){
if(class(mywine[,i])=="numeric" || class(mywine[,i])=="integer") {
  minimum <- min(mywine[,i])
  maximum <- max(mywine[,i])
  mywineNORM[,i]<-as.vector(scale(mywine[,i], center=minimum,scale = maximum-minimum))
  mytestingwineNORM[,i]<-as.vector(scale(mytestingwine[,i],center = minimum, scale = maximum-minimum))
  
  }
}
install.packages('randomForest',dependencies=TRUE)
library('randomForest')

set.seed(18)
myrf <- randomForest(Quality_of_wine~.,mywineNORM,ntree=500,mtry=6,importance=TRUE)
myrf
importance(myrf)
varImpPlot(myrf, main = "Variable Importance")


myprediction <- predict(myrf, mytestingwineNORM[,-12], type='class')
classificationtable <- table(myprediction,mytestingwineNORM[,12])
classificationtable
acctestrandomforest <- sum(diag(classificationtable))/sum(classificationtable)
acctestrandomforest

install.packages('FNN',dependencies=TRUE)
library('FNN')



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

install.packages('e1071',dependencies=TRUE)
library('e1071') 

set.seed(1000)
tunedmodellinear <- tune.svm(Quality_of_wine~., data=mywineNORM, cost=10^(-1:1), kernel="linear")
#for cost having a higher range, WARNING: reaching max number of iterations

summary (tunedmodellinear)
tunedmodellinear$best.parameters[[1]]
tunedmodellinear$best.model

finalmodellinear <- svm(Quality_of_wine~., data=mywineNORM, cost=tunedmodellinear$best.parameters[[1]], kernel="linear")
predictionlinear <- predict(finalmodellinear,mytestingwineNORM[,-12])
classificationtable <- table(pred= predictionlinear, mytestingwineNORM[,12])
classificationtable
acctestmodellinear <- sum(diag(classificationtable))/sum(classificationtable)
acctestmodellinear

#set.seed(1021)
tunedmodelRBF <- tune.svm(Quality_of_wine~., data=mywineNORM, gamma=2^(-1:1), cost=10^(-1:1))
summary(tunedmodelRBF)
tunedmodelRBF$best.parameters[[1]]
tunedmodelRBF$best.parameters[[2]]
tunedmodelRBF$best.model

finalmodelRBF <- svm(Quality_of_wine~., data=mywineNORM, gamma=tunedmodelRBF$best.parameters[[1]],cost=tunedmodelRBF$best.parameters[[2]])
predictionRBF <- predict(finalmodelRBF, mytestingwineNORM[,-12])

classificationtableRBF<-table(pred= predictionRBF, mytestingwineNORM[,12])
classificationtableRBF
acctestmodelRBF <- sum(diag(classificationtableRBF))/sum(classificationtableRBF)
acctestmodelRBF

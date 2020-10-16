mywine<-read.table(file.choose(),header=TRUE)
names(mywine)
str(mywine)
dim(mywine)
mytestingwine<-read.table(file.choose(), header = TRUE)
str(mytestingwine)

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

dim(mywineNORM)
dim(mytestingwineNORM)
summary(mywineNORM)
summary(mytestingwineNORM)

set.seed(60)
LRmodel<-glm(Quality_of_wine~ ., family = 'binomial', mywineNORM)
summary(LRmodel)

probabilitiesLR <- predict(LRmodel, mytestingwineNORM[,-12],type= "response")
predictionLR <- ifelse(probabilitiesLR > 0.5, "2", "1")
predictionLR[0:5]
classificationtable <- table(pred= predictionLR, mytestingwineNORM[,12])
classificationtable
acctestLR <- sum(diag(classificationtable))/sum(classificationtable)
acctestLR

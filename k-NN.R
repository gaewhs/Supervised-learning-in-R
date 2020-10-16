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
install.packages('FNN',dependencies=TRUE)
library('FNN')

myxtrain<-mywineNORM[,-12]
myytrain<-mywineNORM [,12]
myxtesting<-mytestingwineNORM [,-12]
myytesting<-mytestingwineNORM [,12]

dim(myxtrain)
dim(myytrain)
dim(myxtesting)
dim(myytesting)

myk1nn<-knn(data.frame(myxtrain), data.frame(myxtesting), cl=myytrain, k=1)
myccuracytablek1nn<-table(myk1nn,myytesting)
myccuracytablek1nn
mytestingaccuracyk1nn <- sum(diag(myccuracytablek1nn))/sum(myccuracytablek1nn)
mytestingaccuracyk1nn

myk3nn<-knn(data.frame(myxtrain), data.frame(myxtesting), cl=myytrain, k=3)
myccuracytablek3nn<-table(myk3nn,myytesting)
myccuracytablek3nn
mytestingaccuracyk3nn <- sum(diag(myccuracytablek3nn))/sum(myccuracytablek3nn)
mytestingaccuracyk3nn

myk15nn<-knn(data.frame(myxtrain), data.frame(myxtesting), cl=myytrain, k=15)
myccuracytablek15nn<-table(myk15nn,myytesting)
myccuracytablek15nn
mytestingaccuracyk15nn <- sum(diag(myccuracytablek15nn))/sum(myccuracytablek15nn)
mytestingaccuracyk15nn

bestk=0
bestaccuracy=0
accuracy <- NULL
for(auxk in  2:20){
  mycv <- knn.cv(train= myxtrain, cl= myytrain, k=auxk)
  mytable <- table (mycv, myytrain)
  accuracy[auxk] <- sum(diag(mytable))/sum(mytable)
  if(bestaccuracy< accuracy[auxk]) bestk=auxk
  if(bestaccuracy< accuracy[auxk]) bestaccuracy = accuracy[auxk]}
plot(accuracy, main = "Accuracy for various k values")
table(accuracy)
bestk
mybestknn <- knn(train= myxtrain, test= myxtesting, cl= myytrain, k=bestk)


mytestingaccuracytablebestknn <- table (mybestknn,myytesting)
mytestingaccuracytablebestknn
mytestingaccuracybestknn<- sum(diag(mytestingaccuracytablebestknn))/sum(mytestingaccuracytablebestknn)
mytestingaccuracybestknn

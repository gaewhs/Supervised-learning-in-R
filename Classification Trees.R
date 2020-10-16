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

install.packages('tree',dependencies=TRUE)
library('tree')

mytree <-tree(Quality_of_wine~., mywineNORM)
plot(mytree)
text(mytree)
summary(mytree)

set.seed(1)
mycrossval <- cv.tree(mytree,FUN=prune.tree)
mycrossval
tree(formula = Quality_of_wine ~ ., data = mywineNORM)


mybestsize <- mycrossval$size[which(mycrossval$dev==min(mycrossval$dev))] 
mybestsize
myprunedtree <- prune.tree(mytree,best =mybestsize[1])
myprunedtree
plot(myprunedtree)
text(myprunedtree)
summary(myprunedtree)
myprediction <- predict(myprunedtree, mytestingwineNORM[,-12], type='class')
classificationtable <- table(myprediction,mytestingwineNORM[,12])
classificationtable
acctesttree <- sum(diag(classificationtable))/sum(classificationtable)
acctesttree


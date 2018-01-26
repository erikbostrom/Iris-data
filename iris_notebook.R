## ------------------------------------------------------------------------
str(iris)

## ------------------------------------------------------------------------
#iris$Species <- as.character(iris$Species)
table(iris$Species)

## ------------------------------------------------------------------------
summary(iris[iris$Species=="setosa",])
summary(iris[iris$Species=="versicolor",])
summary(iris[iris$Species=="virginica",])

## ------------------------------------------------------------------------


## ----fig.height = 6, fig.width = 6---------------------------------------
library(ggplot2)
p   <- "Set2"
sfb <- scale_fill_brewer(palette=p)
t   <- theme_light()
p1 <- ggplot(iris, aes(x=Sepal.Length, fill=Species)) + geom_histogram(color="black",size=0.3,binwidth=0.05) + sfb + t
p2 <- ggplot(iris, aes(x=Petal.Length, fill=Species)) + geom_histogram(color="black",size=0.3,binwidth=0.05) + sfb + t
grid.arrange(p1, p2, ncol=1)

## ----fig.height = 6, fig.width = 6---------------------------------------
require(gridExtra)
p1 <- ggplot(iris, aes(x=Species,y=Sepal.Length,fill=Species))  + geom_boxplot(color="black",size=0.3,show.legend = FALSE) + sfb + t
p2 <- ggplot(iris, aes(x=Species,y=Sepal.Width, fill=Species))  + geom_boxplot(color="black",size=0.3,show.legend = FALSE) + sfb + t
p3 <- ggplot(iris, aes(x=Species,y=Petal.Length, fill=Species)) + geom_boxplot(color="black",size=0.3,show.legend = FALSE) + sfb + t
p4 <- ggplot(iris, aes(x=Species,y=Petal.Width, fill=Species))  + geom_boxplot(color="black",size=0.3,show.legend = FALSE) + sfb + t
grid.arrange(p1, p2, p3, p4, ncol=2)

## ----fig.height = 8, fig.width = 6---------------------------------------
scb = scale_color_brewer(palette=pal)
p1 <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) + geom_point(aes(color=Species),size=2) + scb + t
p2 <- ggplot(iris,aes(x=Petal.Length,y=Petal.Width)) + geom_point(aes(color=Species),size=2) + scb + t
grid.arrange(p1, p2, ncol=1)

## ------------------------------------------------------------------------
set.seed(1234)

## ------------------------------------------------------------------------
I <- sample(c(0,1), nrow(iris), replace=TRUE, prob=c(2/3, 1/3))
prop.table(table(I))

## ------------------------------------------------------------------------
iris.train        <- iris[I==0,1:4]
iris.train_full   <- iris[I==0,]
iris.train_labels <- iris[I==0,5]
iris.test         <- iris[I==1,1:4]
iris.test_full    <- iris[I==1,]
iris.test_labels  <- iris[I==1,5]

## ------------------------------------------------------------------------
library(class)
iris_knn <- knn(train=iris.train, test=iris.test, cl=iris.train_labels, k=3)
iris_knn

## ------------------------------------------------------------------------
comp <- data.frame(iris_knn,data.frame(iris.test_labels))
names(comp) <- c("kNN predicted Species","Correct Species")
count <- 0
j <- 1
err <- 0
for(i in 1:length(comp[,1])){
  if (comp[i,1]==comp[i,2]){
    count = count + 1
    }else{
      err[j] <- i
      j <- j+1
    }
}
print(paste(count,"out of",length(comp[,1]),"predicted correctly! This is",100*count/length(comp[,1]), "percent correct."))

## ------------------------------------------------------------------------
comp[err,]

## ------------------------------------------------------------------------
library(gmodels)
CrossTable(x=iris.test_labels, y=iris_knn, prop.chisq = FALSE)

## ------------------------------------------------------------------------
# Import the random forest package
library(randomForest)
# Compute the training
iris_randomforest <- randomForest(Species~ . , data=iris.train_full, ntree=100, mtry=2, importance=TRUE,proximity=TRUE)
iris_randomforest

## ----fig.height = 2.5, fig.width = 6-------------------------------------
imp <- varImpPlot(iris_randomforest)
graphics.off()
imp <- data.frame(imp)
p1 <- ggplot(imp,aes(y=row.names(imp),x=MeanDecreaseAccuracy)) + geom_point() + t + xlab("Mean accuracy loss [%]") + ylab(NULL)
p2 <- ggplot(imp,aes(y=row.names(imp),x=MeanDecreaseGini)) + geom_point() + t + xlab("Mean Gini impurity decrease [%]") + ylab(NULL)
grid.arrange(p1, p2, ncol=2)

## ------------------------------------------------------------------------
prediction <- predict(iris_randomforest,iris.test_full)
table(observed=iris.test_labels, predicted=prediction)


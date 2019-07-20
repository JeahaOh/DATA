instal.package(randomForest)
instal.package(rpart)


library(randomForest)
library(rpart)
data(iris)
model = randomForest(Species~., data=iris)  # model => deep learning
# hyper


from collections import OrderedDict

table = OrderedDict((  
    ("Item", ['Item0', 'Item0', 'Item1', 'Item1']),
    ('CType',['Gold', 'Bronze', 'Gold', 'Silver']),
    ('USD',  ['1dollor', '2dollor', '3dollor', '4dollor']),
    ('EU',   ['1E', '2E', '3E', '4E'])
)) 

set.seed(123)
result<-sample(1:nrow(wAUS), nrow(wAUS)*0.7)
train<-wAUS[result,]
test<-wAUS[-result,]
dim(train) 
dim(test)

model=randomForest(RainTomorrow~., data=train, ntree=100, mtry=2, imfortance=TRUE, 
                   na.action=na.omit)
model
p1<-predict(model, newdata=test)
library(caret)
(c<-confusionMatrix(p1, test$RainTomorrow)) 
importance(model)
varImpPlot(model) 

ntree <- c(200, 300, 400, 500,600)
mtry <- c(6:10)
param <- data.frame(n=ntree, m=mtry)
param 
for(i in param$n){
  cat('ntree = ', i, '\n')
  for(j in param$m){ 
    cat('mtry = ', j, '\n')
    model = randomForest(RainTomorrow~., data=train, 
                         ntree=i, mtry=j, 
                         na.action=na.omit )    
    print(model)
  }
}
str(model)

# 3
registerDoParallel(cores=4)
getDoParWorkers()
#model 생성
system.time( 
  rf_weather <- foreach(ntree=rep(200, 1), .combine=combine, 
                        .packages = 'randomForest', .multicombine=TRUE) %dopar%
    randomForest(RainTomorrow~., data=train, 
                 ntree=ntree, mtry=2, importance=TRUE, na.action=na.omit)
)
rf_weather
str(rf_weather) 




library(caret)
data(iris)
sum(is.na(iris))
set.seed(123)
result<-sample(1:nrow(iris), nrow(iris)*0.7)
train<-iris[result,]
test<-iris[-result,]
dim(train) 
dim(test)
traindata<-train[,1:4]
class(traindata)
trainclasses<-train[,5]
set.seed(300)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats=10)
grid_rf<-expand.grid(ntree=c(10, 100, 200), mtry=c(2,4,8))  


wdbc <- read.csv('wdbc_data.csv', stringsAsFactors = FALSE)
str(wdbc)
head(wdbc)
length(wdbc)

wdbc <- wdbc[-1] 
head(wdbc)
head(wdbc[, c('diagnosis')], 10) 
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
str(wdbc$diagnosis)
summary(wdbc[,c(2:31)])

normalize <- function(x){ 
  return ((x - min(x)) / (max(x) - min(x)))
}

(wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize)))
set.seed(415) 
idx = sample(1:nrow(wdbc_x), 0.7*nrow(wdbc_x))
wdbc_train = wdbc_x[idx, ] 
wdbc_test = wdbc_x[-idx, ] 
dim(wdbc_train)
dim(wdbc_test)
wdbc_train_y <- wdbc[idx, 1] 
wdbc_test_y <- wdbc[-idx, 1] 

a<-dim(wdbc_train)

k = sqrt(dim(wdbc_train)[1]) 
wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=19)
wdbc_pred
tb<-table(wdbc_pred, wdbc_test_y)
round(sum(diag(tb))/sum(tb)*100)


k <- 5:25
acc <- numeric() 
cnt <- 1

for(i in k){
  cat('i=', i, '\n')
  pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=i)
  t <- table(pred, wdbc_test_y)
  acc[cnt] <- (t[1,1]+t[2,2]) / sum(t)
  cat('분류정확도', acc[cnt], '\n')
  cnt <- cnt + 1 # 카운터 
}
acc
sort(acc, decreasing = T) 
first <- sort(acc, decreasing = T)[1]
first
for(i in k){
  if(acc[i-4] >= first){
    cat('k=', i)
    cat(', 분류정확도', acc[i-4], '\n')
  }
} 


library(caret)
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
plot(knnFit1)
knnPredict <- predict(knnFit1,  newdata = TrainData )
confusionMatrix(knnPredict, TrainClasses )
mean(knnPredict == TrainClasses) 



clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)
library(ggplot2)
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green')) 



set.seed(1)
iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3, trace=TRUE)) 
table(iris$Species, kmeans.result$cluster)

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 2) 
 
print(kmeans.result$totss)
print(kmeans.result$withinss)
print(kmeans.result$betweenss)

install.packages("NbClust")
library(NbClust)
nc <- NbClust(iris2, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Clusters의 수", ylab="Criteria 수",
        main="클러스터 갯수 결정 그라프")



wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Clusters 수",
       ylab="그룹내 sum of squares")}

wssplot(iris2) 



teens <- read.csv("./snsdata.csv")
set.seed(100)
str(teens)
library(NbClust)
interests <- teens[5:40]  
nc <- NbClust(interests, min.nc=2, max.nc=5, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Clusters의 수", ylab="Criteria 수",
        main="클러스터 갯수 결정 그라프")
table(teens$gender, useNA = "ifany")
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age) 
(avg <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm =T)) )
teens$age <- ifelse(is.na(teens$age), avg, teens$age)

interests <- teens[5:40]  
summary(interests)
interests_n <- data.frame(lapply(interests, scale))
summary(interests_n)

teen_clusters <- kmeans(interests_n, 5) 
teen_clusters$size
table(teen_clusters$cluster) 
teen_clusters$centers  



aggregate(data = teens, gender=='F' ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)
aggregate(data = teens, cute ~ cluster, mean)
library(PerformanceAnalytics) 
plot(teens[c("basketball", "cute")], col=teens$cluster)
summary(teens)
aggregate(data = teens, softball + volleyball + hair + dress ~ 
            gender=='F', mean)




library(arules)
install.packages("arulesViz")
library(arulesViz)
data(Groceries)
head(Groceries)
Groceries[1:6]
nrow(Groceries)
inspect(Groceries[1:6])

str(Groceries)
summary(Groceries)   
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules <- apriori(Groceries, parameter=list(support=0.1, confidence=0.5))  
rules <- apriori(Groceries, parameter=list(support=0.01, confidence=0.5)) 
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
inspect(rules[1:5])
plot(rules[1:5])
plot(rules[1:5],method="graph",engine='interactive')
plot(rules[1:15],method="graph",engine='interactive')

subrules2 <- head(sort(rules, by="lift"), 10)
inspect(subrules2)
plot(subrules2, method="graph")
plot(subrules2, method="grouped")
plot(subrules2, method="matrix")

rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5),
                 appearance=list(rhs= "sugar"))
inspect(rules)

subrules2 <- head(sort(rules, by="lift"), 10)
inspectDT(subrules2)
p <- inspectDT(subrules2)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html") 



basket_rules <- apriori(Groceries, parameter = list(sup = 0.003, conf = 0.5, 
                                                    target="rules"))
summary(basket_rules)
inspect(subset(basket_rules, size(basket_rules)>4)) # lhs의 item >4
yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt" & lift>3.5)
inspect(yogurt.rhs)              
meat.lhs <- subset(basket_rules, subset = lhs %in% "meat" & lift>1.5)
inspect(meat.lhs)
inspectDT(basket_rules)
p <- inspectDT(basket_rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html") 


library(arulesViz)
library(arules)
data("AdultUCI")
str(AdultUCI)
summary(AdultUCI)
head(AdultUCI)

AdultUCI[["age"]]<-ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)),
                           labels=c("young", "middle", "senior", "old"))
AdultUCI[["hours-per-week"]]<-ordered(cut(AdultUCI[["hours-per-week"]],  
                                          c(0,25,40,60,168)), labels=c("part-time", "full-time", "over-time", "workaholic"))
AdultUCI[["capital-gain"]]<-ordered(cut(AdultUCI[["capital-gain"]],
                                        c(-Inf,0,median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0]),Inf)),
                                    labels=c("None", "Low", "High"))
AdultUCI[["capital-loss"]]<-ordered(cut(AdultUCI[["capital-loss"]],
                                        c(-Inf,0,median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]]>0]),Inf)),
                                    labels=c("None", "Low", "High"))
AdultUCI[["fnlwgt"]]<-NULL
AdultUCI[["education-num"]]<-NULL

str(AdultUCI);  Adult_new <- as(AdultUCI, "transactions")
summary(Adult_new)
basket_rules <- apriori(Adult_new,
                        parameter = list(sup = 0.08, conf = 0.5, target="rules")) 



rules <- apriori(Adult_new,parameter = list(support=0.04, confidence=0.4,minlen=2,maxlen=14),
                 appearance=list(rhs=c("income=large","income=small"),default="lhs"))
large_income <- subset(rules, subset=rhs %in% "income=large" )
inspect(large_income)
inspect(head(sort(large_income, by="lift"),n=30))
# 문제3) 주당 일하는 시간과 소득과의 관계를 확인해 보시요 
worktime <- subset(basket_rules, subset = lhs %in% c( "hours-per-week=part-time", 
                                                      "hours-per-week=full-time", "hours-per-week=over-time", "hours-per-week=workaholic")
                   , default="lhs" ) 
worktime <- subset(worktime, subset = rhs %in% c("income=large", "income=large") )  
################################
# adabag: An R Package for Classification with
# Boosting and Bagging
################################
# 3. Functions
# 3.1. The boosting, predict.boosting and boosting.cv functions
# R >= 3.2.3 needed (pbkrtest wanted it.)
#install.packages("adabag")
library("adabag")
data("iris")
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
# 1)boosting
iris.adaboost <- boosting(Species ~ ., data = iris[train, ], mfinal = 10,
                          control = rpart.control(maxdepth = 1))
iris.adaboost
# C symbol name “rs_createGD” not in load table --> latest RStudio needed
# http://stackoverflow.com/questions/37392931/c-symbol-name-rs-creategd-not-in-load-table
barplot(iris.adaboost$imp[order(iris.adaboost$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")

table(iris.adaboost$class, iris$Species[train], dnn=c("Predicted Class", "Observed Class")) 
1-sum(iris.adaboost$class==iris$Species[train])/length(iris$Species[train])
# 2)predict.boosting
iris.predboosting<- predict.boosting(iris.adaboost, newdata=iris[-train,])
iris.predboosting
# 3)boosting.cv
iris.boostcv <- boosting.cv(Species ~ ., v = 10, data = iris, mfinal = 10,
                            control = rpart.control(maxdepth = 1))
iris.boostcv

# 3.2. The bagging, predict.bagging and bagging.cv functions
# 1)bagging(논문보다 오차가크게나옴)
iris.bagging <- bagging(Species ~ ., data = iris[train, ], mfinal = 10,
                        control = rpart.control(maxdepth = 1))
iris.bagging

barplot(iris.bagging$imp[order(iris.bagging$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")

table(iris.bagging$class, iris$Species[train], dnn=c("Predicted Class", "Observed Class")) 
1-sum(iris.bagging$class==iris$Species[train])/length(iris$Species[train])
# 2)predict.bagging(논문보다 오차가크게나옴.bagging에 의존)
iris.predbagging<- predict.bagging(iris.bagging, newdata=iris[-train,])
iris.predbagging
# 3)bagging.cv(논문보다 오차가크게나옴)
iris.baggingcv <- bagging.cv(Species ~ ., v = 10, data = iris,
                             mfinal = 10, control = rpart.control(maxdepth = 1))
iris.baggingcv
# 3.3. The margins and errorevol functions
# 1)margins(bagging과 predict.bagging 필요)
iris.bagging.margins <- margins(iris.bagging,iris[train,])  # training set
iris.bagging.predmargins <- margins(iris.predbagging,iris[-train,])  # test set

iris.bagging.margins # training set
iris.bagging.predmargins # test set
# bagging과 predict.bagging이 논문보다 오차가 크게 나와서 그래프가 논문과 많이다름.
margins.test<-iris.bagging.predmargins[[1]]
margins.train<-iris.bagging.margins[[1]]
plot(sort(margins.train), (1:length(margins.train)) /
       length(margins.train), type = "l", xlim = c(-1,1),
     main = "Margin cumulative distribution graph", xlab = "m",
     ylab = "% observations", col = "blue3", lwd = 2)
abline(v=0, col="red",lty=2, lwd=2)
lines(sort(margins.test), (1:length(margins.test)) / length(margins.test),
      type = "l", cex = 0.5, col = "green", lwd = 2)
legend("topleft", c("test","train"), col = c("green", "blue"), lty=1, lwd=2)
# 2)errorevol(boosting 필요)
evol.test <- errorevol(iris.adaboost,iris[-train,]) 
evol.train <- errorevol(iris.adaboost,iris[train,]) 
plot(evol.test$error, type = "l", ylim = c(0, 1),
     main = "Boosting error versus number of trees", xlab = "Iterations",
     ylab = "Error", col = "red", lwd = 2)
lines(evol.train$error, cex = .5 ,col="blue", lty=2, lwd=2)
legend("topleft", c("test","train"), col = c("red", "blue"), lty=1:2, lwd=2)

# 4. Examples
# 4.1. A dichotomous example
n <- 12000
p <- 10
set.seed(100)
x <- matrix(rnorm(n*p), ncol=p)
y <- as.factor(c(-1, 1)[as.numeric(apply(x^2, 1, sum)  > 9.34) + 1])
data <- data.frame(y, x)

train <- sample(1:n, 2000, FALSE)

formula <- y ~ .
vardep <- data[,as.character(formula[[2]])]

cntrl <- rpart.control(maxdepth = 1, minsplit = 0,  cp = -1)
mfinal <-400

# "Breiman"/FALSE(논문엔 없음.)
data.boosting <- boosting(formula=formula, data=data[train,],
                          mfinal=mfinal, coeflearn= "Breiman", boos=FALSE, control=cntrl)
data.boostingBreimanFalse <- data.boosting

table(data.boosting$class, vardep[train], dnn=c("Clase estimada", "Clase real")) 
1-sum(data.boosting$class==vardep[train])/length(vardep[train])

data.predboost <- predict.boosting(data.boosting, newdata=data[-train,]) 
data.predboost$confusion
data.predboost$error
data.boosting$imp

# "Breiman"/TRUE (논문 기준.논문과 결과값 다름)
data.boosting <- boosting(formula=formula, data=data[train,],
                          mfinal=mfinal, coeflearn= "Breiman", boos=TRUE, control=cntrl)
data.boostingBreimanTrue <- data.boosting

table(data.boosting$class, vardep[train], dnn=c("Predicted Class", "Observed Class")) 
1-sum(data.boosting$class==vardep[train])/length(vardep[train])
data.predboost <- predict.boosting(data.boosting, newdata=data[-train,]) 
data.predboost$confusion
data.predboost$error
data.boosting$imp

# "Freund"/FALSE(논문 기준)
data.boosting <- boosting(formula=formula, data=data[train,],
                          mfinal=mfinal, coeflearn= "Freund", boos=FALSE, control=cntrl)
data.boostingFreundFalse <- data.boosting

table(data.boosting$class, vardep[train], dnn=c("Predicted Class", "Observed Class")) 
1-sum(data.boosting$class==vardep[train])/length(vardep[train])

data.predboost <- predict.boosting(data.boosting, newdata=data[-train,])
data.predboost$confusion
data.predboost$error
data.boosting$imp

# "Freund"/TRUE (논문엔 없음.논문과 결과값 다름) 
data.boosting <- boosting(formula=formula, data=data[train,],
                          mfinal=mfinal, coeflearn= "Freund", boos=TRUE, control=cntrl)
data.boostingFreundTrue <- data.boosting

table(data.boosting$class, vardep[train], dnn=c("Predicted Class", "Observed Class")) 
1-sum(data.boosting$class==vardep[train])/length(vardep[train])

data.predboost <- predict.boosting(data.boosting, newdata=data[-train,])
data.predboost$confusion
data.predboost$error
data.boosting$imp

# "Breiman"/FALSE(논문엔 없음.)
data.boosting <- data.boostingBreimanFalse

errorevol.train <- errorevol(data.boosting, data[train,]) 
errorevol.test <- errorevol(data.boosting, data[-train,])

plot(errorevol.test[[1]], type="l", ylim=c(0,0.5),
     main="Adaboost error versus number of trees", xlab="Iterations",
     ylab="Error", col = "red",lwd=2)
lines(errorevol.train[[1]], cex = .5 ,col="blue", lty=1,lwd=2)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1,lwd=2)    
abline(h=min(errorevol.test[[1]]), col="red",lty=2,lwd=2)
abline(h=min(errorevol.train[[1]]), col="blue",lty=2,lwd=2)

# "Breiman"/TRUE (논문엔 없음.이전값이 다르므로 논문과 결과 다를걸로 추정) 
data.boosting <- data.boostingBreimanTrue

errorevol.train <- errorevol(data.boosting, data[train,]) 
errorevol.test <- errorevol(data.boosting, data[-train,])

plot(errorevol.test[[1]], type="l", ylim=c(0,0.5),
     main="Adaboost error versus number of trees", xlab="Iterations",
     ylab="Error", col = "red",lwd=2)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1,lwd=2)    
abline(h=min(errorevol.test[[1]]), col="red",lty=2,lwd=2)
abline(h=min(errorevol.train[[1]]), col="blue",lty=2,lwd=2)

# "Freund"/FALSE(논문 기준)
data.boosting <- data.boostingFreundFalse

errorevol.train <- errorevol(data.boosting, data[train,]) 
errorevol.test <- errorevol(data.boosting, data[-train,])

plot(errorevol.test[[1]], type="l", ylim=c(0,0.5),
     main="Adaboost error versus number of trees", xlab="Iterations",
     ylab="Error", col = "red",lwd=2)
lines(errorevol.train[[1]], cex = .5 ,col="blue", lty=1,lwd=2)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1,lwd=2)    
abline(h=min(errorevol.test[[1]]), col="red",lty=2,lwd=2)
abline(h=min(errorevol.train[[1]]), col="blue",lty=2,lwd=2)

# "Freund"/TRUE (논문엔 없음.이전값이 다르므로 논문과 결과 다를걸로 추정) 
data.boosting <- data.boostingFreundTrue

errorevol.train <- errorevol(data.boosting, data[train,]) 
errorevol.test <- errorevol(data.boosting, data[-train,])

plot(errorevol.test[[1]], type="l", ylim=c(0,0.5),
     main="Adaboost error versus number of trees", xlab="Iterations",
     ylab="Error", col = "red",lwd=2)
lines(errorevol.train[[1]], cex = 0.5,col="blue", lty=1,lwd=2)
legend("topright", c("test","train"), col = c("red", "blue"), lty=1,lwd=2)    
abline(h=min(errorevol.test[[1]]), col="red",lty=2,lwd=2)
abline(h=min(errorevol.train[[1]]), col="blue",lty=2,lwd=2)

# pruning
data.prune <- predict.boosting(data.boosting, newdata=data[-train,], newmfinal=253) 
data.prune$confusion
data.prune$error

# 4.2. A multiclass example
data("Vehicle")
# not included paper(only code) : start
#rows<-50
#col<-4
#errortrain<- array(0, c(rows,col))  
#errortest<- array(0, c(rows,col))  
# not included paper(only code) : end
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3) # moved to for statement in code
maxdepth <- 5
# not included paper(only code) : start
#mfinal<-50
#matrix.sub <- array(0, c(2*l/3,rows)) 
#cntrl<-rpart.control(maxdepth=5, cp=-1, minsplit=0)
#
#for (m in 1:rows) {
#
#sub <- sample(1:l,2*l/3)
#matrix.sub[,m]<-sub
# not included paper(only code) : end

# rpart
Vehicle.rpart <- rpart(Class~.,data=Vehicle[sub,],maxdepth=maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart,newdata=Vehicle,type="class")
1-sum(Vehicle.rpart.pred[sub]==Vehicle$Class[sub])/length(Vehicle$Class[sub])
tb <- table(Vehicle.rpart.pred[-sub],Vehicle$Class[-sub])
tb
1-sum(Vehicle.rpart.pred[-sub]==Vehicle$Class[-sub])/length(Vehicle$Class[-sub])

# Bagging
mfinal <- 50       # added only code part
cntrl <- rpart.control(maxdepth=5, minsplit=0, cp=-1)  # added only code part
Vehicle.bagging <- bagging(Class ~ ., data=Vehicle[sub,], mfinal=mfinal, control=cntrl)
1-sum(Vehicle.bagging$class==Vehicle$Class[sub])/length(Vehicle$Class[sub])
Vehicle.predbagging<- predict.bagging(Vehicle.bagging, newdata=Vehicle[-sub,])
Vehicle.predbagging$confusion
Vehicle.predbagging$error

# Adaboost.M1(논문과 결과다름)
Vehicle.adaboost <- boosting(Class ~., data = Vehicle[sub, ],
                             mfinal=mfinal, coeflearn="Freund", boos=TRUE, control=cntrl)  # TRUE 일때 문제
1-sum(Vehicle.adaboost$class==Vehicle$Class[sub])/length(Vehicle$Class[sub])
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error

# SAMME(논문과 결과다름)
Vehicle.SAMME <- boosting(Class ~ ., data = Vehicle[sub, ],
                             mfinal=mfinal, coeflearn="Zhu", boos=TRUE, control=cntrl) # TRUE 일때 문제
1-sum(Vehicle.SAMME$class==Vehicle$Class[sub])/length(Vehicle$Class[sub])
Vehicle.SAMME.pred <- predict.boosting(Vehicle.SAMME,newdata=Vehicle[-sub, ])
Vehicle.SAMME.pred$confusion
Vehicle.SAMME.pred$error

# Variable importance comparison
sort(Vehicle.bagging$importance,decreasing=TRUE)
sort(Vehicle.adaboost$importance,decreasing=TRUE)
sort(Vehicle.SAMME$importance,decreasing=TRUE)

# Bagging(논문과 결과다름.TRUE일때 문제)
barplot(sort(Vehicle.bagging$importance,decreasing=TRUE),
          main="Variables Relative Importance", col = "lightblue",
          horiz = TRUE,las=1, cex.names=.6, xlim = c(0, 20))
# AdaBoost.M1(논문과 결과다름.TRUE일때 문제.논문에 없는 코드)
barplot(sort(Vehicle.adaboost$importance,decreasing=TRUE),
        main="Variables Relative Importance", col = "lightblue",
        horiz = TRUE,las=1, cex.names=.6, xlim = c(0, 20))
# SAMME(논문과 결과다름.TRUE일때 문제.논문에 없는 코드)
barplot(sort(Vehicle.SAMME$importance,decreasing=TRUE),
        main="Variables Relative Importance", col = "lightblue",
        horiz = TRUE,las=1, cex.names=.6, xlim = c(0, 20))

# The margins plots
# not included paper(only code) : start
#sub<-matrix.sub[,which(errortest[,2]==min(errortest[,2]))[1]]
# not included paper(only code) : end
# Bagging(논문과 결과다름)
margins.train <- margins(Vehicle.bagging,Vehicle[sub,])[[1]]
#논문에는 Vehicle.bagging.pred 이지만 Vehicle.predbagging로 통일
margins.test <- margins(Vehicle.predbagging,Vehicle[-sub,])[[1]] 


plot(sort(margins.train), (1:length(margins.train))/length(margins.train), 
  type="l", xlim=c(-1,1),main="Margin cumulative distribution graph", xlab="m", 
  ylab="% observations", col="blue3", lty=2, lwd=2)
abline(v=0, col="red",lty=2, lwd=2)
lines(sort(margins.test), (1:length(margins.test))/length(margins.test), 
  type="l", cex = .5 ,col="green", lwd=2)
legend("topleft", c("test","train"), col = c("green", "blue3"), lty=1:2, lwd=2)

# AdaBoost.M1(논문과 조금다름.논문에 없는코드)
margins.train <- margins(Vehicle.adaboost,Vehicle[sub,])[[1]]
margins.test <- margins(Vehicle.adaboost.pred,Vehicle[-sub,])[[1]] 


plot(sort(margins.train), (1:length(margins.train))/length(margins.train), 
  type="l", xlim=c(-1,1),main="Margin cumulative distribution graph", xlab="m", 
  ylab="% observations", col="blue3", lty=2, lwd=2)
abline(v=0, col="red",lty=2, lwd=2)
lines(sort(margins.test), (1:length(margins.test))/length(margins.test), 
  type="l", cex = .5 ,col="green", lwd=2)
legend("topleft", c("test","train"), col = c("green", "blue3"), lty=1:2, lwd=2)

# SAMME(논문과 조금다름.논문에 없는코드)
margins.train <- margins(Vehicle.SAMME,Vehicle[sub,])[[1]]
margins.test <- margins(Vehicle.SAMME.pred,Vehicle[-sub,])[[1]] 


plot(sort(margins.train), (1:length(margins.train))/length(margins.train), 
  type="l", xlim=c(-1,1),main="Margin cumulative distribution graph", xlab="m", 
  ylab="% observations", col="blue3", lty=2, lwd=2)
abline(v=0, col="red",lty=2, lwd=2)
lines(sort(margins.test), (1:length(margins.test))/length(margins.test), 
  type="l", cex = .5 ,col="green", lwd=2)
legend("topleft", c("test","train"), col = c("green", "blue3"), lty=1:2, lwd=2)


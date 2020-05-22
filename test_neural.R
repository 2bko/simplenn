library(neuralnet)
library(NeuralNetTools)

data <-
  read.csv(
    "~/Downloads/titanic/train.csv",
    header = TRUE,
    sep = ","
  )

data <- na.omit(data)
data[complete.cases(data),]

data[] <- lapply(data, function(x) {
  if (is.factor(x))
    as.numeric(x)
  else
    x
})


smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind,]
test <- data[-train_ind,]

str(train)

formula <- as.formula("Survived ~ Age + Sex1")

field <- "Survived"

model_nn <-
  neuralnet(
    formula,
    data = train,
    hidden = c(2),
    linear.output = T,
    rep = 2,
    stepmax = 1000000,
    lifesign = "minimal"
  )

plot(model_nn, rep = "best")

d <- subset(test, select = -field)
str(d)

garson(model_nn)


prob <- neuralnet::compute(model_nn, test[, model_nn$model.list$variables])
tt <- test[, model_nn$model.list$variables]
tt$Prediction <- prob$net.result
str(tt)
table(tt)
resultTable <- table(test[, model_nn$model.list$variables], prob$net.result)
resultTable
pred <- ifelse(prob$net.result > 0.5, 1, 0)
confusionMatrix(factor(pred), factor(test$Survived))


pred <- predict(model_nn, test[, model_nn$model.list$variables])
table(test$Survived, apply(pred, 1, which.max))

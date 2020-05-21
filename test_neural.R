library(neuralnet)
library(NeuralNetTools)

data <-
  read.csv(
    "~/Downloads/train.csv",
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

formula <- as.formula("Survived ~ Age + Sex")

field <- "Survived"

model_nn <-
  neuralnet(
    formula,
    data = train,
    hidden = c(2, 2),
    linear.output = T,
    rep = 2,
    stepmax = 1000000,
    lifesign = "minimal"
  )

plot(model_nn, rep = "best")

d <- subset(test, select = -field)
str(d)

# prob <- compute(model_nn, subset(test, select=-c("Survived")))
# prob.result <- prob$net.result
#
# detach(package:neuralnet,unload = T)
#
# library(ROCR)
# nn.pred = prediction(prob.result, test$Survived)
# pref <- performance(nn.pred, "tpr", "fpr")
# plot(pref)


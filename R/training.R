# Train
#
train <- function() {
    library(caret)
    library(doMC)
    
    set.seed(123456789)
    registerDoMC(4)
    
    metrics1 <- prepare_metrics('./inst//extdata/metrics/url_metrics.csv', './inst//extdata/metrics/url_classification.csv')
    metrics2 <- prepare_metrics('./inst//extdata/metrics/url_metrics_user.csv', './inst//extdata/metrics/url_classification_user.csv')
    metrics3 <- prepare_metrics('./inst//extdata/metrics/url_metrics_user_2.csv', './inst//extdata/metrics/url_classification_user_2.csv')
    metrics <- rbind(metrics1, metrics2, metrics3)
    
    inTrain <- createDataPartition(metrics$class, p=0.7, list=FALSE)
    training <- metrics[inTrain,]
    testing <- metrics[-inTrain,]
    
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    
    model <- train(class ~ ., data = training, method = "rf", trControl = fitControl)
    confusionMatrix(predict(model, testing), testing[, 'class'])
    
    model
}

# Run
# Rserve(TRUE, args="--RS-conf ./Rserve.conf --no-save")
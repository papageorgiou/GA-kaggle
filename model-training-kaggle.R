library(caret)
library(randomForest)
library(doParallel)
library(tidyverse)
library(corrplot)
library(pROC)
library(ROCR)

registerDoParallel(4,cores=4)
getDoParWorkers()


set.seed(1234)

final_df <- read_rds("../input/final_df_converters.rds")

training_indexs <- createDataPartition(final_df$transactions_test, p = .7, list = F)

training <- final_df[training_indexs, ]
testing  <- final_df[-training_indexs, ]


# training ----------------------------------------------------------------

starting <- Sys.time()

control <- trainControl(method='repeatedcv', 
                        number=10,  
                        repeats=5, search="random", classProbs = T) 
# summaryFunction = twoClassSummary 
# classProbs = TRUE
#Metric compare model is Accuracy for classification
#Number randomely variable selected is mtry
#mtry <- sqrt(ncol(training))

rf_tune <- train(transactions_test~., 
                 data=training, 
                 method='rf', 
                 importance=T,
                 metric='Accuracy',
                 tuneLength  = 15, 
                 trControl=control,
                 allowParallel=TRUE)

cat("\n# Time taken: ", format(Sys.time() - starting))

## name the new model!

saveRDS(rf_tune, "rf_tune_converters.rds")


print(rf_tune)



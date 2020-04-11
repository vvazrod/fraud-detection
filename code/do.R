library(caret)

##### ROSE #####
# Create train and validation partitions
train_index_rose <- createDataPartition(data_balanced_rose$isFraud, p = .7, list = FALSE)
train_rose <- data_balanced_rose[train_index_rose, ]
val_rose <- data_balanced_rose[-train_index_rose, ]

lr_grid <- expand.grid(.nIter = 5)
lr_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
lr_model <- train(
  isFraud ~ .,
  data = train_rose,
  method = "LogitBoost",
  trControl = lr_control,
  tuneGrid = lr_grid
  )

##### SMOTE ####
library(tidyverse)
library(caret)

set.seed(0)

##### ROSE #####
train_index_rose <- createDataPartition(data_balanced_rose$isFraud, p = .7, list = FALSE)
train_rose <- data_balanced_rose[train_index_rose, ]
val_rose <- data_balanced_rose[-train_index_rose, ]

# Logistic Regression
lr_grid_rose <- expand.grid(.nIter = 5)
lr_control_rose <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
lr_model_rose <- train(
  isFraud ~ .,
  data = train_rose,
  method = "LogitBoost",
  trControl = lr_control_rose,
  tuneGrid = lr_grid_rose
  )

lr_prediction_rose <- predict(lr_model_rose, val_rose, type = "raw")

prec_lr_rose <- precision(data = lr_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")
rec_lr_rose <- recall(data = lr_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")
fmeas_lr_rose <- F_meas(data = lr_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")

# Multilayer Perceptron
mlp_control_rose <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
mlp_model_rose <- train(
  isFraud ~ .,
  data = train_rose,
  method = "mlp",
  trControl = mlp_control_rose
)

mlp_prediction_rose <- predict(mlp_model_rose, val_rose, type = "raw")

prec_mlp_rose <- precision(data = mlp_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")
rec_mlp_rose <- recall(data = mlp_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")
fmeas_mlp_rose <- F_meas(data = mlp_prediction_rose, reference = val_rose$isFraud, relevant = "Yes")

##### SMOTE ####
# Create train and validation partitions
train_index_smote <- createDataPartition(data_balanced_smote$isFraud, p = .7, list = FALSE)
train_smote <- data_balanced_smote[train_index_smote, ]
val_smote <- data_balanced_smote[-train_index_smote, ]

# Logistic Regression
lr_grid_smote <- expand.grid(.nIter = 5)
lr_control_smote <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
lr_model_smote <- train(
  isFraud ~ .,
  data = train_smote,
  method = "LogitBoost",
  trControl = lr_control_smote,
  tuneGrid = lr_grid_smote
)

lr_prediction_smote <- predict(lr_model_smote, val_smote, type = "raw")

prec_lr_smote <- precision(data = lr_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")
rec_lr_smote <- recall(data = lr_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")
fmeas_lr_smote <- F_meas(data = lr_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")

# Multilayer Perceptron
mlp_control_smote <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
mlp_model_smote <- train(
  isFraud ~ .,
  data = train_smote,
  method = "mlp",
  trControl = mlp_control_smote
)

mlp_prediction_smote <- predict(mlp_model_smote, val_smote, type = "raw")

prec_mlp_smote <- precision(data = mlp_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")
rec_mlp_smote <- recall(data = mlp_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")
fmeas_mlp_smote <- F_meas(data = mlp_prediction_smote, reference = val_smote$isFraud, relevant = "Yes")

##### Plots #####
# Create results data frame
dataset <- c(rep("ROSE", 6), rep("SMOTE", 6))
model <- rep(c(rep("LR", 3), rep("MLP", 3)), 2)
measure <- rep(c("Precision", "Recall", "F-measure"), 4)
value <- c(
  prec_lr_rose, rec_lr_rose, fmeas_lr_rose,
  prec_mlp_rose, rec_mlp_rose, fmeas_mlp_rose,
  prec_lr_smote, rec_lr_smote, fmeas_lr_smote,
  prec_mlp_smote, rec_mlp_smote, fmeas_mlp_smote
)
results <- data.frame(dataset, model, measure, value)

# Plot ROSE results
results %>%
  filter(dataset == "ROSE") %>%
  ggplot(aes(x = model, fill = measure, y = value)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Modelo", fill = "Medida", y = "Valor")

# Plot SMOTE results
results %>%
  filter(dataset == "SMOTE") %>%
  ggplot(aes(x = model, fill = measure, y = value)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Modelo", fill = "Medida", y = "Valor")

# Plot LR results
results %>%
  filter(model == "LR") %>%
  ggplot(aes(x = dataset, fill = measure, y = value)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Conjunto", fill = "Medida", y = "Valor")

# Plot MLP results
results %>%
  filter(model == "MLP") %>%
  ggplot(aes(x = dataset, fill = measure, y = value)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Conjunto", fill = "Medida", y = "Valor")

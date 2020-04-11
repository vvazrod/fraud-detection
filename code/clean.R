library(tidyverse)
library(funModeling)
library(caret)
library(zoo)
library(forcats)
library(DescTools)
library(Boruta)
library(ROSE)
library(DMwR)

# Set class and categorical varaibles as factor, discretize continuous variables and remove ID
data_discretized <- 
  train_dataset %>%
  mutate(isFraud = as.factor(ifelse(isFraud == 1, "Yes", "No"))) %>%
  mutate_at(c("ProductCD", "P_emaildomain", "R_emaildomain", "DeviceType", "DeviceInfo"), factor) %>%
  mutate_at(vars(starts_with("addr")), factor) %>%
  mutate_at(vars(starts_with("card")), factor) %>%
  mutate_at(vars(starts_with("M")), factor) %>%
  mutate_at(vars(matches("id_(1[2-9]|2[0-9]|3[0-8])")), factor) %>%
  mutate(
    TransactionAmt = cut_number(TransactionAmt, n = 10),
    TransactionDT = cut_number(TransactionDT, n = 10)
    ) %>%
  select(-TransactionID)

# Check dataset status
status <- df_status(data_discretized)

# Remove NA and high/low dispersity variables
na_cols <- 
  status %>%
  filter(p_na > 50) %>%
  select(variable)

high_dif_cols <- 
  status %>%
  filter(unique > 0.8 * nrow(data_discretized)) %>%
  select(variable)

low_dif_cols <-
  status %>%
  filter(type == "numeric" & unique < 0.01 * nrow(data_discretized)) %>%
  select(variable)

data_reduced <-
  data_discretized %>%
  select(-one_of(
    bind_rows(list(na_cols, high_dif_cols, low_dif_cols))$variable
    ))

# Replace NAs
data_replaced <-
  data_reduced %>%
  mutate_if(is.numeric, na.aggregate) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "Unknown")

# Calculate correlation matrix and remove highly correlated variables
corr_matrix <-
  data_replaced %>%
  select_if(is.numeric) %>%
  cor(.)

data_final <-
  data_replaced %>%
  select(-one_of(findCorrelation(corr_matrix, cutoff = 0.8, names = TRUE)))
  
# Feature selection with Boruta
# boruta_output <- Boruta(isFraud ~ ., data = train_reduced_cors, doTrace = 2)

# Check class imbalance
table(data_final$isFraud)
prop.table(table(data_final$isFraud))

# Generate synthetic data to balance dataset
data_balanced_rose <- ROSE(isFraud ~ ., data = data_final, seed = 1)$data
data_balanced_smote <- SMOTE(isFraud ~ ., as.data.frame(data_final))

# Check balance after synthetic data generation
table(data_balanced_rose$isFraud)
prop.table(table(data_balanced_rose$isFraud))

table(data_balanced_smote$isFraud)
prop.table(table(data_balanced_smote$isFraud))

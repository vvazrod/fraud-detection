library(funModeling)
library(caret)
library(zoo)
library(forcats)
library(DescTools)
library(Boruta)
library(earth)
library(ROSE)
library(DMwR)

# Set class and categorical features as factor, discretize continuous variables and remove ID
train_processed <- 
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

# Check columns' status
status <- df_status(train_processed)

# Remove NA and disperse columns
na_cols <- 
  status %>%
  filter(p_na > 50) %>%
  select(variable)

dif_cols <- 
  status %>%
  filter(unique > 0.8 * nrow(train_processed)) %>%
  select(variable)

train_reduced <- 
  train_processed %>%
  select(-one_of(
    bind_rows(list(na_cols, dif_cols))$variable
    ))

# Replace NAs
train_replaced <-
  train_reduced %>%
  mutate_if(is.numeric, na.aggregate) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "Unknown")

# Remove highly correlated features
na_cor_cols <-
  df_status(train_replaced, print_results = FALSE) %>%
  filter(type == "numeric") %>%
  filter(q_zeros == 0 | p_zeros == 100.0) %>%
  select(variable)

corr_matrix <-
  train_replaced %>%
  select_if(is.numeric) %>%
  select(-one_of(na_cor_cols$variable)) %>%
  cor(.)

train_reduced_cors <-
  train_replaced %>%
  select(-one_of(findCorrelation(corr_matrix, cutoff = 0.8, names = TRUE)))
  
# Feature selection with Boruta
# boruta_output <- Boruta(isFraud ~ ., data = train_replaced, doTrace = 2, maxRuns = 5)

# Feature selection with MARS
# mars <- earth(isFraud ~ ., data = train_reduced_cors)

# Check class imbalance
table(train_reduced_cors$isFraud)
prop.table(table(train_reduced_cors$isFraud))

# Generate synthetic data to balance dataset
train_balanced_rose <- ROSE(isFraud ~ ., data = train_reduced_cors, seed = 1)$data
train_balanced_smote <- SMOTE(isFraud ~ ., as.data.frame(train_reduced_cors))

# Check balance after synthetic data generation
table(train_balanced_rose$isFraud)
prop.table(table(train_balanced_rose$isFraud))

table(train_balanced_smote$isFraud)
prop.table(table(train_balanced_smote$isFraud))

library(funModeling)
library(caret)
library(zoo)
library(forcats)
library(Boruta)
# library(ROSE)
# library(DMwR)

# Check class imbalance
table(train_dataset$isFraud)
prop.table(table(train_dataset$isFraud))

# Quartiles
quantile(train_dataset$TransactionAmt)
quantile(train_dataset$TransactionDT)

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
na_cols <- status %>%
  filter(p_na > 50) %>%
  select(variable)

dif_cols <- status %>%
  filter(unique > 0.8 * nrow(train_processed)) %>%
  select(variable)

train_reduced <- train_processed %>%
  select(-one_of(
    bind_rows(list(na_cols, dif_cols))$variable
    ))

# Replace NAs
train_replaced <-
  train_reduced %>%
  mutate_if(is.numeric, na.aggregate) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "Unknown")

# Correlations
cor(train_replaced[sapply(train_replaced, is.numeric)])

# Remove highly correlated variables
cor_cols <-
  train_replaced %>%
  select_if(is.numeric) %>%
  select(one_of(findCorrelation(cor(na.omit(.)), cutoff = 0.7)))

boruta_output <- Boruta(isFraud ~ ., data = train_replaced, doTrace = 2, maxRuns = 5)

# Generate synthetic data to balance dataset
# train_balanced <- ROSE(isFraud ~ ., data = train_replaced, seed = 1)$data
# train_balanced <- SMOTE(isFraud ~ ., train_replaced, perc.over = 600, perc.under = 100)

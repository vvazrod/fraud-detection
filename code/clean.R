library(tidyverse)
library(funModeling)
library(caret)
library(zoo)
library(forcats)
library(DescTools)
library(Boruta)
library(ROSE)
library(DMwR)
library(reshape2)

# See variable class
glimpse(train_dataset)

# Set class and categorical variables as factor, discretize continuous variables and remove ID
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

# NA percentage graph
status %>%
  ggplot(aes(x = variable, y = p_na)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Porcentaje") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
    )

# Dispersity graph
status %>%
  ggplot(aes(x = variable, y = (unique / nrow(data_discretized) * 100))) +
  geom_col(show.legend = FALSE) +
  labs(y = "Porcentaje") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Reduced dispersity graph
status %>%
  filter(unique < 0.6 * nrow(data_discretized)) %>%
  ggplot(aes(x = variable, y = (unique / nrow(data_discretized) * 100))) +
  geom_col(show.legend = FALSE) +
  labs(y = "Porcentaje") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

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

# Calculate correlation matrix
corr_matrix <-
  data_replaced %>%
  select_if(is.numeric) %>%
  cor(.)

# Correlation heatmap
corr_matrix %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(fill = "Valor") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Remove highly correlated variables
data_final <-
  data_replaced %>%
  select(-one_of(findCorrelation(corr_matrix, cutoff = 0.8, names = TRUE)))
  
# Calculate variable importance with Boruta
boruta_output <- Boruta(isFraud ~ ., data = data_final, doTrace = 2)

# Plot variable importance
plot(boruta_output, whichShadow = c(FALSE, FALSE, FALSE), xlab = "Variables", ylab = "Importancia")

# Keep 10 most important variables, remove the rest
imp_cols <-
  attStats(boruta_output) %>%
  rownames_to_column(var = "variable") %>%
  top_n(10, meanImp) %>%
  select(variable)

data_boruta <-
  data_final %>%
  select(isFraud, one_of(imp_cols$variable))

# Check class imbalance
table(data_boruta$isFraud)
prop.table(table(data_boruta$isFraud))
data_boruta %>%
  ggplot(aes(x = isFraud, fill = isFraud)) +
  geom_bar(stat = "count") +
  labs(y = "Cantidad") +
  theme(legend.position = "none")

# Generate synthetic data to balance dataset
data_balanced_rose <- ROSE(isFraud ~ ., data = data_boruta, seed = 1)$data
data_balanced_smote <- SMOTE(isFraud ~ ., as.data.frame(data_boruta))

# Check balance after synthetic data generation
table(data_balanced_rose$isFraud)
prop.table(table(data_balanced_rose$isFraud))
data_balanced_rose %>%
  ggplot(aes(x = isFraud, fill = isFraud)) +
  geom_bar(stat = "count") +
  labs(y = "Cantidad") +
  theme(legend.position = "none")

table(data_balanced_smote$isFraud)
prop.table(table(data_balanced_smote$isFraud))
data_balanced_smote %>%
  ggplot(aes(x = isFraud, fill = isFraud)) +
  geom_bar(stat = "count") +
  labs(y = "Cantidad") +
  theme(legend.position = "none")

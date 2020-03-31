library(tidyverse)

if (!file.exists("data/train_innerjoin.csv") || !file.exists("data/test_innerjoin.csv")) {
  # Load train and test source datasets
  train_transaction <- read_csv("data/train_transaction.csv")
  train_identity <- read_csv("data/train_identity.csv")
  test_transaction <- read_csv("data/test_transaction.csv")
  test_identity <- read_csv("data/test_identity.csv")
  
  # Merge tables
  train_dataset <- merge(train_transaction, train_identity, by="TransactionID") %>%
    sample_n(size = 10000)
  test_dataset <- merge(test_transaction, test_identity, by="TransactionID")
  
  # Write merged datasets
  write_csv(train_dataset, "data/train_innerjoin.csv")
  write_csv(test_dataset, "data/test_innerjoin.csv")
} else {
  # Load train and test datasets
  train_dataset <- read_csv("data/train_innerjoin.csv")
  test_dataset <- read_csv("data/test_innerjoin.csv")
}

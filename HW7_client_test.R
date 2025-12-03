## Write your client test code here

library(httr)
library(jsonlite)
library(tidyverse)

# Load test data
test <- read_csv("test_dataset.csv.gz")

# Calculates the number of days between when the appointment was scheduled and 
# when it occurs

test$days_between <- as.numeric(difftime(test$appt_time, test$appt_made, units = "days"))

# Encoding categorical variables

test <- test %>%
  mutate(
    provider_id = as.factor(provider_id),
    specialty = as.factor(specialty),
    address = as.factor(address)
  )

# Select predictors

predictors <- test %>%
  select(age, provider_id, address, specialty, days_between)

# Test /predict_prob

predpoints <- toJSON(predictors)
r <- POST("http://127.0.0.1:8080/predict_prob", 
          body = predpoints, 
          content_type_json())
result_prob <- content(r, "text") |> fromJSON()
cat("Probability predictions:\n")
print(result_prob)

# Test /predict_class

r <- POST("http://127.0.0.1:8080/predict_class", 
          body = predpoints, 
          content_type_json())
result_class <- content(r, "text") |> fromJSON()
cat("\nClass predictions:\n")
print(result_class)
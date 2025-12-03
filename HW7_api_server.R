## Write your API endpoints in this file

library(plumber2)
library(tidyverse)

# Load and train model
training <- read_csv("train_dataset.csv.gz")
test <- read_csv("test_dataset.csv.gz")

# Calculates the number of days between when the appointment was scheduled and 
# when it occurs

training$days_between <- as.numeric(difftime(training$appt_time, training$appt_made, units = "days"))
test$days_between <- as.numeric(difftime(test$appt_time, test$appt_made, units = "days"))

# Encoding categorical variables

training <- training %>%
  mutate(
    provider_id = as.factor(provider_id),
    specialty = as.factor(specialty),
    address = as.factor(address),
    no_show = as.factor(no_show)
  )

test <- test %>%
  mutate(
    provider_id = as.factor(provider_id),
    specialty = as.factor(specialty),
    address = as.factor(address)
  )

# Logistic regression to predict probability a patient does not show up to the 
# appointment, trained on training data set

model <- glm(
  no_show ~ age + provider_id + address + specialty + days_between,
  data = training,
  family = binomial(link = "logit")
)

# API endpoints

#* Predict probability of no-show
#* @post /predict_prob
#* @serializer json
function(body) {
  body <- as.data.frame(body)
  pred <- predict(model, newdata = body, type = "response")
  as.numeric(pred)
}

#* Predict class of no-show
#* @post /predict_class
#* @serializer json
function(body) {
  body <- as.data.frame(body)
  prob <- predict(model, newdata = body, type = "response")
  as.numeric(ifelse(prob > 0.5, 1, 0))
}

# Run server in console:
## pa <- api("HW7_api_server.R", port = 8080)
## api_run(pa)
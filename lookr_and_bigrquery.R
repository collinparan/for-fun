#!/usr/bin/env Rscript

library(lookr)
library(bigrquery)
library(randomForest)

#LookR connection requirements
sdk <- lookr::LookerSDK$new(config = list(
  apiVersion = "3.0",
  basePath = "https://[instance].looker.com:19999",
  clientId = "[clientid]",
  clientSecret = "[secret]",
  embedSecret = "",
  userId = "",
  verifySSL = TRUE))

#Grab specific Look
df <- sdk$runLook(lookId = [look number int value])

#Do "random" stuff with the data
##############################################################################################################
colnames(df) <- c("start_date","subscriber_type", "trip_id", "bikeid", "start_station_id", "end_station_id", "duration_minutes")
df$trip_id <- as.numeric(df$trip_id)
df$subscriber_type <- as.factor(df$subscriber_type)
df$bikeid <- as.numeric(df$bikeid)
df$start_station_id <- as.numeric(df$start_station_id)
df$end_station_id <- as.numeric(df$end_station_id)
df$duration_minutes <-as.numeric(df$duration_minutes)

sample_size <- floor(0.90* nrow(df))
train_ind <- sample(seq_len(nrow(df)), size= sample_size)
train <- df[train_ind,]
test <- df[-train_ind,]

rf_model <- randomForest(duration_minutes ~ trip_id + subscriber_type + bikeid + start_station_id + end_station_id, data=train, keep.forest=TRUE)
predictions <- predict(rf_model, test)
cor.test(test$duration_minutes, predictions)

forecast_data <- cbind(test, predictions)

##############################################################################################################

#Insert data with Forecast back to BigQuery
insert_upload_job(project, "collin_test", "df", forecast_data, write_disposition = "WRITE_TRUNCATE")

setwd("~/Desktop/Kaggle/")
train <- read.csv("train_expedia.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("test_expedia.csv", header = TRUE, stringsAsFactors = FALSE)

prop.table(table(train$booking_bool))
train$srch_prop_id = paste(train$srch_id, train$prop_id, sep= "-")
test$srch_prop_id = paste(test$srch_id, test$prop_id, sep= "-")

test$booking_bool <- rep(0,295623)
prop.table(table(train$prop_review_score, train$booking_bool),1)
prop.table(table(train$prop_starrating, train$booking_bool),1)
prop.table(table(train$promotion_flag, train$booking_bool),1)
prop.table(table(train$prop_brand_bool, train$booking_bool),1)


test$booking_bool[test$prop_review_score >= 3.5] <- 1
test$booking_bool[test$prop_starrating >= 3] <-1

aggregate(booking_bool ~ prop_review_score + prop_starrating, data=train, FUN=sum)
aggregate(booking_bool ~ prop_review_score + prop_starrating, data=train, FUN=length)

aggregate(booking_bool ~ prop_review_score + prop_starrating, data=train, FUN=function(x) 
  {sum(x)/length(x)})

test$booking_bool[test$prop_review_score >= 1 & test$prop_review_score <= 1.5 & 
                    test$prop_starrating == 0] <- 0
test$booking_bool[test$prop_review_score >= 0 & test$prop_review_score <= 1.5 &
                    test$prop_starrating == 1] <- 0
test$booking_bool[test$prop_review_score == 3 & test$prop_starrating == 1] <- 0
test$booking_bool[test$prop_review_score == 4 & test$prop_review_score == 4.5 &
                  test$prop_starrating == 1] <- 0
test$booking_bool[test$prop_review_score == 1 & test$prop_review_score == 1.5 & 
                    test$prop_starrating == 3] <- 0
test$booking_bool[test$prop_review_score == 1.5 & test$prop_starrating == 4] <- 0
test$booking_bool[test$prop_review_score >= 1 & test$prop_review_score <= 2 & 
                    test$prop_starrating == 5] <- 0

#submit
submit <- data.frame(srch_prop_id= test$srch_prop_id, booking_bool= test$booking_bool)
write.csv(submit, file = "Star&Review.csv", row.names = FALSE)





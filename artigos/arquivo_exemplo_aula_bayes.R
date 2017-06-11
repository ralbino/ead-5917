install.packages("kernlab")
install.packages("wordcloud")
install.packages("tm")
install.packages("e1071")
install.packages("gmodels")
install.packages("caret")

library("kernlab")
library("wordcloud")
library("tm")
library("e1071")
library("gmodels")
library("caret")

data(spam)
set.seed(12345)
spam_rand <- spam[order(runif(4601)),]
spam_train <- spam_rand[1:3000,]
spam_test <- spam_rand[3001:4601,]
prop.table(table(spam_train$type))
prop.table(table(spam_test$type))

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No" )
  return(x)}

spam_train[,1:54] <- apply(spam_train[,1:54],MARGIN=2, convert_counts)
spam_test[,1:54] <- apply(spam_test[,1:54],MARGIN=2, convert_counts)
spam_train_matrix <- as.matrix(spam_train[,1:54])
spam_test_matrix <- as.matrix(spam_test[,1:54])

# dados tratados e tipo
spam_classifier <- naiveBayes(spam_train_matrix,spam_train$type)
spam_test_pred <- predict(spam_classifier, spam_test)

CrossTable(spam_test$type, spam_test_pred, prop.chisq = FALSE, prop.t =
               FALSE, prop.r=FALSE, dnn = c("actual" ,"predicted"))

spam_classifier2 <- naiveBayes(spam_train_matrix, spam_train$type, laplace=1)
spam_test_pred2 <- predict(spam_classifier2, spam_test)
CrossTable(spam_test$type, spam_test_pred2, prop.chisq = FALSE, prop.t =
               FALSE, prop.r=FALSE, dnn = c("actual" ,"predicted"))

confusionMatrix(spam_test_pred2, spam_test$type, positive="spam")


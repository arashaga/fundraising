setwd("your own directory")

library("readxl")
library("stringr")
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
future_fundraisting_df <- read_excel("FutureFundraising.xlsx",sheet=2)
# xlsx files
fundraisting_df <- read_excel("Fundraising.xlsx", sheet= 2)

names(fundraisting_df) <- str_replace_all(names(fundraisting_df), c(" " = "" ))
str(fundraisting_df)

fundraisting_df$zipconvert_2 <-factor(fundraisting_df$zipconvert_2)
fundraisting_df$zipconvert_3 <-factor(fundraisting_df$zipconvert_3)
fundraisting_df$zipconvert_4 <-factor(fundraisting_df$zipconvert_4)
fundraisting_df$zipconvert_5 <-factor(fundraisting_df$zipconvert_5)

fundraisting_df$homeownerdummy <- factor(fundraisting_df$homeownerdummy)
fundraisting_df$genderdummy <- factor(fundraisting_df$genderdummy)
fundraisting_df$TARGET_B <-factor(fundraisting_df$TARGET_B)
  
summary(fundraisting_df)

fundraisting_df <-  fundraisting_df[,3:23]

names(fundraisting_df)
set.seed(42)  # for reproducibility

smp_size <- floor(0.75 * nrow(fundraisting_df))
train_ind <- sample(seq_len(nrow(fundraisting_df)), size = smp_size)

train <- fundraisting_df[train_ind, ]
test <- fundraisting_df[-train_ind, ]
library("randomForest")

fundraising_model <- randomForest(TARGET_B ~ ., 
                             data = train)

print(fundraising_model)

###Aded variable importance
# Get importance
importance    <- importance(fundraising_model)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()
######

# Grab OOB error matrix & take a look
err <- fundraising_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(fundraising_model)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))


# Generate predicted classes using the model object
class_prediction <- predict(object = fundraising_model,   # model object 
                            newdata = test,  # test dataset
                            type = "class") # return classification labels
library("caret")
# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = class_prediction,       # predicted classes
                      reference = test$TARGET_B)  # actual classes
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

str(fundraising_model)

library("AUC")
# Generate predictions on the test set
pred <- predict(object = fundraising_model,
                newdata = test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc(actual = ifelse(test$TARGET_B == "yes", 1, 0), 
    predicted = pred[,"yes"])                    

res <- tuneRF(x = subset(train, select = -TARGET_B),
              y = train$TARGET_B,
              ntreeTry = 500)
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(train) * c(0.7, 0.8)



# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models

for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = TARGET_B ~ ., 
                        data = train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

new_model  <- randomForest(formula = TARGET_B ~ ., 
                                            data = train, mtry =12, nodesize=5, sampsize=1638)


###### see the result of the mode

print(new_model)


# Grab OOB error matrix & take a look
err <- new_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(fundraising_model)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))


# Generate predicted classes using the model object
class_prediction <- predict(object = fundraising_model,   # model object 
                            newdata = test,  # test dataset
                            type = "class") # return classification labels
library("caret")
# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = class_prediction,       # predicted classes
                      reference = test$TARGET_B)  # actual classes
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

str(fundraising_model)

library("AUC")
# Generate predictions on the test set
pred <- predict(object = fundraising_model,
                newdata = test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc(actual = ifelse(test$TARGET_B == "yes", 1, 0), 
    predicted = pred[,"yes"])     

## Activity classes: 

## Participants asked to perform 10 reps of the Unilateral Dumbbell Biceps Curl

## A: as specified? derp?
## B: throwing the elbows to the front?
## C: lifting the dumbbell only half way?
## D: lowering the dumbbell only half way?
## E: throwing the hips to the front?

if (file.exists("./MachineLearning") == FALSE) {
    dir.create("./MachineLearning")
}

setwd("./MachineLearning")

## Get training set
if (file.exists("./pml-training.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                  "pml-training.csv")
}

## Get test set
if (file.exists("./pml-testing.csv") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                  "pml-testing.csv")
}

library(dplyr); library(ggplot2)

train <- tbl_df(read.csv("pml-training.csv", stringsAsFactors = FALSE))

densplot <- function(varname, data) {
    ggplot(data, aes(varname, color = factor(classe))) + geom_density()
}

mysample <- train %>%
    sample_n(1000)

predictors <- c()

for (i in 1:ncol(mysample)) {
    nasum <- sum(is.na(mysample[,i]))
    if (nasum < 1){
        predictors <- c(predictors, names(mysample[i]))
    }
}

checknzv <- nearZeroVar(mysample, saveMetrics = TRUE)

mysample <- mysample[, predictors]
mysample <- mysample %>%
    select(-contains("kurtosis"), -contains("skewness"), -contains("window"),
           -contains("timestamp"), -contains("min"), -contains("max"),
           -contains("amplitude"), -user_name, -X)






# set.seed(n)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
ldafit1 <- train(classe ~ ., data = mysample, method = "lda", trControl = fitControl)
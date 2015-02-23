library(caret)

## Importing data
training.base <- read.csv(file="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing.problems <- read.csv(file="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


## Getting familiar with the data
dim(training.base); dim(testing.problems)

names(training.base)
head(training.base)
summary(training.base)

table(training.base$user_name, training.base$classe)

## training set setup
set.seed(12345)

inTrain <- createDataPartition(y=training.base$classe,
                               p=0.75,list=FALSE)
training <- training.base[inTrain,]
testing <- training.base[-inTrain,]
dim(training); dim(testing)

## Choosing covariates

# remove irrelevant variables

nsv <- nearZeroVar(training,saveMetrics=TRUE)
varstoremove <- row.names(nsv[nsv$nzv==TRUE,])
varstoremove <- c(varstoremove,names(training)[1:7])
varstoremove <- c(varstoremove, 'max_roll_belt', 'max_picth_belt', 'min_roll_belt', 'min_pitch_belt', 
                                  'amplitude_roll_belt', 'amplitude_pitch_belt', 'var_total_accel_belt', 
                                  'avg_roll_belt', 'stddev_roll_belt', 'var_roll_belt', 'avg_pitch_belt', 
                                  'stddev_pitch_belt', 'var_pitch_belt', 'avg_yaw_belt', 'stddev_yaw_belt', 
                                  'var_yaw_belt', 'var_accel_arm', 'max_roll_arm', 'max_picth_arm', 'max_yaw_arm',
                                  'min_roll_arm', 'min_pitch_arm', 'min_yaw_arm', 'amplitude_roll_arm', 
                                  'amplitude_pitch_arm', 'amplitude_yaw_arm', 'max_roll_dumbbell', 
                                  'max_picth_dumbbell', 'min_roll_dumbbell', 'min_pitch_dumbbell', 
                                  'amplitude_roll_dumbbell', 'amplitude_pitch_dumbbell', 'var_accel_dumbbell', 
                                  'avg_roll_dumbbell', 'stddev_roll_dumbbell', 'var_roll_dumbbell', 
                                  'avg_pitch_dumbbell', 'stddev_pitch_dumbbell', 'var_pitch_dumbbell', 
                                  'avg_yaw_dumbbell', 'stddev_yaw_dumbbell','var_yaw_dumbbell', 'max_picth_forearm',
                                  'min_pitch_forearm', 'amplitude_pitch_forearm', 'var_accel_forearm')
training <- training[, !(names(training) %in% varstoremove)]

### predicting 
# model, use preProcess=c("center","scale") or "knnImpute" in the train func

modFit.tree <- train(classe~., method="rpart", data=training, preProcess=c("center","scale"))
print(modFit.tree$finalModel)
plot(modFit.tree$finalModel,uniform=TRUE, main="Classification Tree")
text(modFit.tree$finalModel, use.n=TRUE, all=TRUE, cex=.8)
library(rattle)
fancyRpartPlot(modFit.tree$finalModel)

# random forest
modFit <- randomForest(classe ~ ., data=training)
modFit

pred <- predict(modFit,testing)
table(pred,testing$classe)

## predict the problem dataset
pred <- predict(modFit,testing.problems)

answers <- as.character(pred)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(answers)

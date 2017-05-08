> setwd("D:/R/library")
>
> library(caret)
> library(randomForest)
>
>
> trainDF<- read.csv("./data/pml-training.csv")
> testDF<- read.csv("./data/pml-testing.csv")
> set.seed(123)
> dim(trainDF); dim(testDF)
[1] 19622   160
[1]  20 160
>
> # Computation of near-zero values and removal from both DFs
> nzv<-nearZeroVar(trainDF)
> trainDF<-trainDF[,-nzv]
> testDF<-testDF[,-nzv]
>
> # Computation of variables with a high number of NAs and removal from both DFs
> NAs <- sapply(trainDF, function(x) mean(is.na(x))) > 0.95
> trainDF <- trainDF[, NAs==F]
> testDF <- testDF[, NAs==F]
> dim(trainDF); dim(testDF)
[1] 19622    59
[1] 20 59
> 
> # Variables still available (we make sure that the output variable has non been removed)
> names(trainDF)
 [1] "X"                    "user_name"            "raw_timestamp_part_1" "raw_timestamp_part_2" "cvtd_timestamp"      
 [6] "num_window"           "roll_belt"            "pitch_belt"           "yaw_belt"             "total_accel_belt"    
[11] "gyros_belt_x"         "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
[16] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
[21] "pitch_arm"            "yaw_arm"              "total_accel_arm"      "gyros_arm_x"          "gyros_arm_y"         
[26] "gyros_arm_z"          "accel_arm_x"          "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
[31] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
[36] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"     "accel_dumbbell_x"    
[41] "accel_dumbbell_y"     "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
[46] "roll_forearm"         "pitch_forearm"        "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
[51] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"      "accel_forearm_z"     
[56] "magnet_forearm_x"     "magnet_forearm_y"     "magnet_forearm_z"     "classe"              
> 
> # By their description we can expect some variables not to have a good prediction value, we remove the first 5 
> trainDF <- trainDF[, -(1:5)]
> testDF <- testDF[, -(1:5)]
> dim(trainDF); dim(testDF)
[1] 19622    54
[1] 20 54
> 
> # we split the train data into 2 sets
> intrain <- createDataPartition(y=trainDF$classe, p=0.7, list=F)
> trainDF_train<-trainDF[intrain,]
> trainDF_test<-trainDF[-intrain,]
> 
> # computation of the Random Forest model
> mymodel_RF<-train(classe ~ ., data = trainDF_train, method="rf", trControl=trainControl(method = "cv", number = 5, verboseIter = F))	
> plot(mymodel_RF, ylim = c(0.9, 1))
> 
> # we apply the RF model and check it's accuracy and Out Of Sample Error (oose)
> preds_RF<-predict(mymodel_RF, trainDF_test)
> confusionMatrix(trainDF_test$classe, preds_RF)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1674    0    0    0    0
         B    4 1135    0    0    0
         C    0    4 1022    0    0
         D    0    0    3  960    1
         E    0    0    0    0 1082

Overall Statistics
                                          
               Accuracy : 0.998           
                 95% CI : (0.9964, 0.9989)
    No Information Rate : 0.2851          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9974          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9976   0.9965   0.9971   1.0000   0.9991
Specificity            1.0000   0.9992   0.9992   0.9992   1.0000
Pos Pred Value         1.0000   0.9965   0.9961   0.9959   1.0000
Neg Pred Value         0.9991   0.9992   0.9994   1.0000   0.9998
Prevalence             0.2851   0.1935   0.1742   0.1631   0.1840
Detection Rate         0.2845   0.1929   0.1737   0.1631   0.1839
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9988   0.9978   0.9981   0.9996   0.9995
> 
> oose_RF <- 1 - as.numeric(confusionMatrix(trainDF_test$classe, preds_RF)$overall[1])
> oose_RF
[1] 0.002039082
> 
> # excellent accuracy and very low oose 
> 
> 
> # Same procedure with the Boosting model
> mymodel_B <- train(classe ~ ., method = "gbm", data = trainDF_train, verbose = F, trControl = trainControl(method = "cv"))
>
> plot(mymodel_B, ylim = c(0.9, 1))
> 
> preds_B <-predict(mymodel_B,trainDF_test)
> confusionMatrix(trainDF_test$classe,preds_B)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1671    2    0    1    0
         B    8 1122    9    0    0
         C    0    8 1017    0    1
         D    0    3   14  947    0
         E    0    1    3    9 1069

Overall Statistics
                                          
               Accuracy : 0.99            
                 95% CI : (0.9871, 0.9924)
    No Information Rate : 0.2853          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9873          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9952   0.9877   0.9751   0.9896   0.9991
Specificity            0.9993   0.9964   0.9981   0.9966   0.9973
Pos Pred Value         0.9982   0.9851   0.9912   0.9824   0.9880
Neg Pred Value         0.9981   0.9971   0.9946   0.9980   0.9998
Prevalence             0.2853   0.1930   0.1772   0.1626   0.1818
Detection Rate         0.2839   0.1907   0.1728   0.1609   0.1816
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9973   0.9920   0.9866   0.9931   0.9982
> 
> oose_B <- 1 - as.numeric(confusionMatrix(trainDF_test$classe, preds_B)$overall[1])
> oose_B
[1] 0.01002549
> 
> # good accuracy and low oose
> 
>
>
> # The Random Forest model gave better results so we will use it to predict on the real test data
> levels(trainDF[,"classe"])
[1] "A" "B" "C" "D" "E"
>
> names(testDF[54])
[1] "problem_id"
> 
>
> # The Random Forest model gave better results so we will use it to predict on the real test data (20 observations)
> predResults_FINAL <- predict(mymodel_RF, testDF[, -54])
> predResults_FINAL
> 
> # the predictions are printed out: one file per prediction
>write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./predictions/problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

>write_files(predResults_FINAL)
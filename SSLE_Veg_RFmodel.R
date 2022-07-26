# author: Simon Mägli (s.maegli@bluewin.ch)
# function: training and testing a random forest model for the classification of small-scale landscape elements


library(randomForest)
library(dplyr)
library(caret)
library(DataExplorer)
library(UBL)

#library(doParallel)
#cl <- makeCluster(detectCores(), type='PSOCK')
#registerDoParallel(cl)


source("SSLE_Veg_prep.R")
#table(df_train$class)   # check class distribution
#write.table(table(df_train$class), file = "results/XX_ClassDistribution/Veg_all.txt", sep = "\t")

#remove class 7 (hedgerows) because it consists of classes 8/9/10 -> hedgerow = merged 8+9+10 with length > 25m and gap between elements < 5m (?)
df_train <- filter(df_train, class != 7)
df_train <- df_train %>% mutate(class=droplevels(class))
table(df_train$class)   # check class distribution



#### preparation & cleanup ####

# first: max number per class should be = 30000 -> preparation for SMOTE 
class_0 <- filter(df_train, class == 0)
Smpl_class_0 <- class_0[sample(nrow(class_0), 30000), ]
df_train <- rbind(Smpl_class_0,(filter(df_train, class != 0)))

class_9 <- filter(df_train, class == 9)
Smpl_class_9 <- class_9[sample(nrow(class_9), 30000), ]
df_train <- rbind(Smpl_class_9,(filter(df_train, class != 9)))
table(df_train$class)   # check class distribution

# impute NA values by roughfix method
df_train <- na.roughfix(df_train)
#write.table(table(df_train$class), file = "results/XX_ClassDistribution/Veg_-Wels_A2_v2.txt", sep = "\t")


#### under- & oversampling ####

# use SMOTE to create a more "balanced problem" by applying both under- & oversampling
df_train_bal <- SmoteClassif(class ~ ., df_train, C.perc = "balance")
table(df_train_bal$class)    # check class distribution 
#write.table(table(df_train_bal$class), file = "results/XX_ClassDistribution/Veg_-Horn_A1_smoted.txt", sep = "\t")



#### Recursive Feature Elimination ####


## Exploratory Data Analysis
plot_intro(df_train_bal)
plot_bar(df_train_bal)
plot_correlation(df_train_bal)

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10,  # number of folds
                      allowParallel = TRUE) 

# Run RFE
result_rfe1 <- rfe(x = df_train_bal[,4:57], 
                   y = df_train_bal$class, 
                   sizes = c(1,5,10,15,20,25,30),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)
write.table(predictors(result_rfe1), file="results/XX_FeatureImportance/FeatRank_Veg_1300spl_all.txt", sep = "\t", quote = FALSE, row.names = TRUE)


# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()


varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:20],
                          importance = varImp(result_rfe1)[1:20, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90))




#### Random Forest model ####

Veg_Top20 <- df_train[,c("class",                   
                              "Mean_vhm",
                              "Max_Z",
                              "StdDev_Z",
                              "Mean_Z",
                              "Mean_NoR",
                              "StdDev_vhm",
                              "GLCM_StdDev",
                              "Mean_nir2",
                              "Mean_ndvi2",
                              "Mean_ndsi2",
                              "Length_main",
                              "Mean_blue2",
                              "Mean_ndvi1",
                              "Mean_red1",
                              "Density",
                              "Area",
                              "Mean_red2",
                              "Mean_blue1",
                              "Mean_green2",
                              "StdDev_ndsi2")]


Veg_Top20_Wels <- df_train[,c("class",                   
                             "Mean_vhm",
                             "Max_Z",
                             "StdDev_Z",
                             "Mean_Z",
                             "Mean_NoR",
                             "StdDev_vhm",
                             "GLCM_StdDev",
                             "Mean_nir2",
                             "Mean_ndvi2",
                             "Mean_ndsi2",
                             "Length_main",
                             "Mean_blue2",
                             "Density",
                             "Area",
                             "Mean_red2",
                             "Mean_green2",
                             "StdDev_ndsi2")]

rf <- randomForest(class ~ ., data=Veg_Top20_Wels, importance=TRUE, na.action=na.fail)
print(rf)
#write.table(rf$confusion, file = "results/XX_RfCM/CM_Veg_all.txt", sep = "\t")
#write.table(rf$confusion, file = "results/XX_RfCM/CM_Veg_30000spl_-Wels_A2_v2.txt", sep = "\t")


#### Prediction ####

#target <- Wels_A2_test   #ADJUST!

pred <- predict(rf, target[-3])
cm = table(target[,3], pred)

importance(rf)
#write.table(importance(rf), file = "results/XX_FeatureImportance/FeatImp_Veg_30000spl_-Wels_A2_v2.txt", sep = "\t")
#varImpPlot(rf)


target$pred <- pred
p <- predict(rf, target, "prob")
target$prob <- apply(p, 1, max)

#### store output ####
write.csv(target[c(3:61)], file="results/XX_Predictions/Pred_Veg_Wels_A2_test_v2.csv", quote = FALSE, row.names = TRUE)





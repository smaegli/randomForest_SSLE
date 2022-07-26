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


source("SSLE_NonVeg_prep.R")
#table(df_train$class)   # check class distribution
#write.table(table(df_train$class), file = "results/XX_ClassDistribution/NonVeg_all.txt", sep = "\t")

#### preparation & cleanup ####

# first: max number per class should be = 3000 -> preparation for SMOTE
class_0 <- filter(df_train, class == 0)
Smpl_class_0 <- class_0[sample(nrow(class_0), 3000), ]
df_train <- rbind(Smpl_class_0,(filter(df_train, class != 0)))

class_3 <- filter(df_train, class == 3)
Smpl_class_3 <- class_3[sample(nrow(class_3), 3000), ]
df_train <- rbind(Smpl_class_3,(filter(df_train, class != 3)))

class_6 <- filter(df_train, class == 6)
Smpl_class_6 <- class_6[sample(nrow(class_6), 3000), ]
df_train <- rbind(Smpl_class_6,(filter(df_train, class != 6)))

table(df_train$class)   # check class distribution

# impute NA values by roughfix method
df_train <- na.roughfix(df_train)
#write.table(table(df_train$class), file = "results/XX_ClassDistribution/NonVeg_-Wels_A2.txt", sep = "\t")

#### under- & oversampling ####

# use SMOTE to create a more "balanced problem" by applying both under- & oversampling
df_train_bal <- SmoteClassif(class ~ ., df_train, C.perc = "balance")
table(df_train_bal$class)    # check class distribution 
#write.table(table(df_train_bal$class), file = "results/XX_ClassDistribution/NonVeg_-Wels_A2_smoted.txt", sep = "\t")

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
result_rfe1 <- rfe(x = df_train_bal[,4:59], 
                   y = df_train_bal$class, 
                   sizes = c(1,5,10,15,20,25,30),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)
#write.table(predictors(result_rfe1), file="results/XX_FeatureImportance/Final/FeatRank_NonVeg_Max3000smpl_all.txt", sep = "\t", quote = FALSE, row.names = TRUE)


# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + 
  theme_bw() +
  labs(x = "Number of features") + 
  labs(y = "Accuracy") + 
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05),breaks = seq(0, 1, 0.1)) + 
  scale_x_continuous(minor_breaks = seq(0, 100, 5), breaks = seq(0, 70, 10)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.text = element_text(size = 16, face="bold")) +
  theme(axis.title = element_text(size = 16, face="bold")) 
  

#ggplot(data = result_rfe1, metric = "Kappa") + 
# theme_bw() +
# labs(x = "Number of features") + 
# labs(y = "Kappa") + 
# scale_y_continuous(minor_breaks = seq(0, 1, 0.05),breaks = seq(0, 1, 0.1)) + 
# scale_x_continuous(minor_breaks = seq(0, 100, 5), breaks = seq(0, 70, 10)) +
# theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
# theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
# theme(axis.text = element_text(size = 16, face="bold")) +
# theme(axis.title = element_text(size = 16, face="bold")) 

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:20],
                          importance = varImp(result_rfe1)[1:20, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity", theme="black") + 
  labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 1)), vjust=-0.5, color="black", size=5, fontface="bold") +

  theme_bw() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 16, face="bold")) +
  theme(axis.text.y = element_text(size = 16, face="bold")) +
  theme(axis.title.x = element_text(size = 16, face="bold")) +
  theme(axis.title.y = element_text(size = 16, face= "bold", margin = margin(t = 0, r = 12, b = 0, l = 0)))



#### Random Forest model ####

#Choose Top20 features
NonVeg_Top20 <- df_train_bal[,c("class",                   
                                "Mean_vecInd",
                                "Density",
                                "Mean_ndvi2",
                                "Width",
                                "StdDev_blue2",
                                "StdDev_ndsi2",
                                "Length_main",
                                "Mean_ndvi1",
                                "StdDev_Z",
                                "Area",
                                "StdDev_green2",
                                "GLCM_Corr",
                                "Compactness",
                                "Mean_red2",
                                "Curv_Length",
                                "Mean_blue1",
                                "Mean_blue2",
                                "Mean_ndwi1",
                                "Max_Z",
                                "Mean_red1")]


#Remove feature not working for Wels areas
NonVeg_Top20_Wels <- df_train_bal[,c("class",                   
                                     "Mean_vecInd",
                                     "Density",
                                     "Mean_ndvi2",
                                     "Width",
                                     "StdDev_blue2",
                                     "StdDev_ndsi2",
                                     "Length_main",
                                     "StdDev_Z",
                                     "Area",
                                     "StdDev_green2",
                                     "GLCM_Corr",
                                     "Compactness",
                                     "Mean_red2",
                                     "Curv_Length",
                                     "Mean_blue2",
                                     "Max_Z")]




rf <- randomForest(class ~ ., data=NonVeg_Top20, importance=TRUE, na.action=na.fail)
print(rf)
write.table(rf$confusion, file = "results/XX_RfCM/CM_NonVeg_all.txt", sep = "\t")
#write.table(rf$confusion, file = "results/XX_RfCM/CM_NonVeg_1684spl_-Wels_A2.txt", sep = "\t")


#### Prediction ####
target <- Wels_A2_test   #ADJUST!

pred <- predict(rf, target[-3])
cm = table(target[,3], pred)

importance(rf)
#write.table(importance(rf), file = "results/XX_FeatureImportance/FeatImp_NonVeg_1684spl_pred_Wels_A2.txt", sep = "\t")
#varImpPlot(rf)


target$pred <- pred
p <- predict(rf, target, "prob")
target$prob <- apply(p, 1, max)

#### store output ####
write.csv(target[c(3:63)], file="results/XX_Predictions/Pred_NonVeg_Wels_A2_test.csv", quote = FALSE, row.names = TRUE)


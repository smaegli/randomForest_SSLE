# author: Simon Mägli (s.maegli@bluewin.ch)
# function: load and clean up input data for Random Forest model


library(dplyr)

#### load extracted features from CSV tables ####

# training areas
Dens_A1_train <- read.csv("results/Dens_A1/Dens_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Horn_A1_train <- read.csv("results/Horn_A1/Horn_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Murg_A1_train <- read.csv("results/Murg_A1/Murg_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Seeh_A1_train <- read.csv("results/Seeh_A1/Seeh_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Thal_A1_train <- read.csv("results/Thal_A1/Thal_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Thal_A2_train <- read.csv("results/Thal_A2/Thal_A2.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Waed_A1_train <- read.csv("results/Waed_A1/Waed_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Wels_A1_train <- read.csv("results/Wels_A1/Wels_A1.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Wels_A2_train <- read.csv("results/Wels_A2/Wels_A2.Veg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]


# testing areas
Dens_A1_test <- read.csv("results/Dens_A1/Dens_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Horn_A1_test <- read.csv("results/Horn_A1/Horn_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Murg_A1_test <- read.csv("results/Murg_A1/Murg_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Seeh_A1_test <- read.csv("results/Seeh_A1/Seeh_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Thal_A1_test <- read.csv("results/Thal_A1/Thal_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Thal_A2_test <- read.csv("results/Thal_A2/Thal_A2.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Waed_A1_test <- read.csv("results/Waed_A1/Waed_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Wels_A1_test <- read.csv("results/Wels_A1/Wels_A1.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]
Wels_A2_test <- read.csv("results/Wels_A2/Wels_A2.TestVeg.csv",header = TRUE,sep = ";", na.strings = "undefined")[-c(3,5,44:45)]


#### data standardization and cleaning ####

# Dens_A1
Dens_A1_train <- Dens_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Dens_A1_train <- Dens_A1_train[Dens_A1_train$Standard.deviation.red1 != 0,]
Dens_A1_train <- Dens_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Dens_A1_train)[52] <- 'Area'
Dens_A1_test <- Dens_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Dens_A1_test <- Dens_A1_test[Dens_A1_test$Standard.deviation.red1 != 0,]
Dens_A1_test <- Dens_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Dens_A1_test)[52] <- 'Area'

# Horn_A1
Horn_A1_train <- Horn_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Horn_A1_train <- Horn_A1_train[Horn_A1_train$Standard.deviation.red1 != 0,]
Horn_A1_train <- Horn_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Horn_A1_train)[52] <- 'Area'
Horn_A1_test <- Horn_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Horn_A1_test <- Horn_A1_test[Horn_A1_test$Standard.deviation.red1 != 0,]
Horn_A1_test <- Horn_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Horn_A1_test)[52] <- 'Area'

# Murg_A1
Murg_A1_train <- Murg_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Murg_A1_train <- Murg_A1_train[Murg_A1_train$Standard.deviation.red1 != 0,]
Murg_A1_train <- Murg_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Murg_A1_train)[52] <- 'Area'
Murg_A1_test <- Murg_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Murg_A1_test <- Murg_A1_test[Murg_A1_test$Standard.deviation.red1 != 0,]
Murg_A1_test <- Murg_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Murg_A1_train)[52] <- 'Area'

# Seeh_A1
Seeh_A1_train <- Seeh_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Seeh_A1_train <- Seeh_A1_train[Seeh_A1_train$Standard.deviation.red1 != 0,]
Seeh_A1_train <- Seeh_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Seeh_A1_train["Mean.ndvi3"][Seeh_A1_train["Mean.ndvi3"] == -1] <- NA
Seeh_A1_train["Standard.deviation.ndvi3"][Seeh_A1_train["Standard.deviation.ndvi3"] <= 0.01] <- NA
Seeh_A1_train["Mean.ndsi3"][Seeh_A1_train["Mean.ndsi3"] == -1] <- NA
Seeh_A1_train["Standard.deviation.ndsi3"][Seeh_A1_train["Standard.deviation.ndsi3"] <= 0.01] <- NA
Seeh_A1_train["Mean.ndwi3"][Seeh_A1_train["Mean.ndwi3"] == -1] <- NA
Seeh_A1_train["Standard.deviation.ndwi3"][Seeh_A1_train["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Seeh_A1_train)[52] <- 'Area'
Seeh_A1_test <- Seeh_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Seeh_A1_test <- Seeh_A1_test[Seeh_A1_test$Standard.deviation.red1 != 0,]
Seeh_A1_test <- Seeh_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Seeh_A1_test["Mean.ndvi3"][Seeh_A1_test["Mean.ndvi3"] == -1] <- NA
Seeh_A1_test["Standard.deviation.ndvi3"][Seeh_A1_test["Standard.deviation.ndvi3"] <= 0.01] <- NA
Seeh_A1_test["Mean.ndsi3"][Seeh_A1_test["Mean.ndsi3"] == -1] <- NA
Seeh_A1_test["Standard.deviation.ndsi3"][Seeh_A1_test["Standard.deviation.ndsi3"] <= 0.01] <- NA
Seeh_A1_test["Mean.ndwi3"][Seeh_A1_test["Mean.ndwi3"] == -1] <- NA
Seeh_A1_test["Standard.deviation.ndwi3"][Seeh_A1_test["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Seeh_A1_test)[52] <- 'Area'

# Thal_A1
Thal_A1_train <- Thal_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Thal_A1_train <- Thal_A1_train[Thal_A1_train$Standard.deviation.red1 != 0,]
Thal_A1_train <- Thal_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Thal_A1_train)[52] <- 'Area'
Thal_A1_test <- Thal_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Thal_A1_test <- Thal_A1_test[Thal_A1_test$Standard.deviation.red1 != 0,]
Thal_A1_test <- Thal_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Thal_A1_test)[52] <- 'Area'

# Thal_A2
Thal_A2_train <- Thal_A2_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Thal_A2_train <- Thal_A2_train[Thal_A2_train$Standard.deviation.red1 != 0,]
Thal_A2_train <- Thal_A2_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Thal_A2_train)[52] <- 'Area'
Thal_A2_test <- Thal_A2_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Thal_A2_test <- Thal_A2_test[Thal_A2_test$Standard.deviation.red1 != 0,]
Thal_A2_test <- Thal_A2_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Thal_A2_test)[52] <- 'Area'

# Waed_A1
Waed_A1_train <- Waed_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Waed_A1_train <- Waed_A1_train[Waed_A1_train$Standard.deviation.red1 != 0,]
Waed_A1_train <- Waed_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Waed_A1_train)[52] <- 'Area'
Waed_A1_test <- Waed_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Waed_A1_test <- Waed_A1_test[Waed_A1_test$Standard.deviation.red1 != 0,]
Waed_A1_test <- Waed_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
names(Waed_A1_test)[52] <- 'Area'

# Wels_A1
Wels_A1_train <- Wels_A1_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Wels_A1_train <- Wels_A1_train[Wels_A1_train$Standard.deviation.red2 != 0,]
Wels_A1_train <- Wels_A1_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Wels_A1_train["Mean.ndvi1"][Wels_A1_train["Mean.ndvi1"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndvi1"][Wels_A1_train["Standard.deviation.ndvi1"] <= 0.01] <- NA
Wels_A1_train["Mean.ndvi3"][Wels_A1_train["Mean.ndvi3"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndvi3"][Wels_A1_train["Standard.deviation.ndvi3"] <= 0.01] <- NA
Wels_A1_train["Mean.ndsi1"][Wels_A1_train["Mean.ndsi1"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndsi1"][Wels_A1_train["Standard.deviation.ndsi1"] <= 0.01] <- NA
Wels_A1_train["Mean.ndsi3"][Wels_A1_train["Mean.ndsi3"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndsi3"][Wels_A1_train["Standard.deviation.ndsi3"] <= 0.01] <- NA
Wels_A1_train["Mean.ndwi1"][Wels_A1_train["Mean.ndwi1"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndwi1"][Wels_A1_train["Standard.deviation.ndwi1"] <= 0.01] <- NA
Wels_A1_train["Mean.ndwi3"][Wels_A1_train["Mean.ndwi3"] == -1] <- NA
Wels_A1_train["Standard.deviation.ndwi3"][Wels_A1_train["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Wels_A1_train)[52] <- 'Area'
Wels_A1_test <- Wels_A1_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Wels_A1_test <- Wels_A1_test[Wels_A1_test$Standard.deviation.red2 != 0,]
Wels_A1_test <- Wels_A1_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Wels_A1_test["Mean.ndvi1"][Wels_A1_test["Mean.ndvi1"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndvi1"][Wels_A1_test["Standard.deviation.ndvi1"] <= 0.01] <- NA
Wels_A1_test["Mean.ndvi3"][Wels_A1_test["Mean.ndvi3"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndvi3"][Wels_A1_test["Standard.deviation.ndvi3"] <= 0.01] <- NA
Wels_A1_test["Mean.ndsi1"][Wels_A1_test["Mean.ndsi1"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndsi1"][Wels_A1_test["Standard.deviation.ndsi1"] <= 0.01] <- NA
Wels_A1_test["Mean.ndsi3"][Wels_A1_test["Mean.ndsi3"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndsi3"][Wels_A1_test["Standard.deviation.ndsi3"] <= 0.01] <- NA
Wels_A1_test["Mean.ndwi1"][Wels_A1_test["Mean.ndwi1"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndwi1"][Wels_A1_test["Standard.deviation.ndwi1"] <= 0.01] <- NA
Wels_A1_test["Mean.ndwi3"][Wels_A1_test["Mean.ndwi3"] == -1] <- NA
Wels_A1_test["Standard.deviation.ndwi3"][Wels_A1_test["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Wels_A1_test)[52] <- 'Area'

# Wels_A2
Wels_A2_train <- Wels_A2_train %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Wels_A2_train <- Wels_A2_train[Wels_A2_train$Standard.deviation.red2 != 0,]
Wels_A2_train <- Wels_A2_train %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Wels_A2_train["Mean.ndvi1"][Wels_A2_train["Mean.ndvi1"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndvi1"][Wels_A2_train["Standard.deviation.ndvi1"] <= 0.01] <- NA
Wels_A2_train["Mean.ndvi3"][Wels_A2_train["Mean.ndvi3"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndvi3"][Wels_A2_train["Standard.deviation.ndvi3"] <= 0.01] <- NA
Wels_A2_train["Mean.ndsi1"][Wels_A2_train["Mean.ndsi1"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndsi1"][Wels_A2_train["Standard.deviation.ndsi1"] <= 0.01] <- NA
Wels_A2_train["Mean.ndsi3"][Wels_A2_train["Mean.ndsi3"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndsi3"][Wels_A2_train["Standard.deviation.ndsi3"] <= 0.01] <- NA
Wels_A2_train["Mean.ndwi1"][Wels_A2_train["Mean.ndwi1"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndwi1"][Wels_A2_train["Standard.deviation.ndwi1"] <= 0.01] <- NA
Wels_A2_train["Mean.ndwi3"][Wels_A2_train["Mean.ndwi3"] == -1] <- NA
Wels_A2_train["Standard.deviation.ndwi3"][Wels_A2_train["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Wels_A2_train)[52] <- 'Area'
Wels_A2_test <- Wels_A2_test %>% mutate_at(c('Mean.red1', 'Mean.red2', 'Mean.red3', 'Mean.green1', 'Mean.green2', 'Mean.green3', 'Mean.blue1', 'Mean.blue2', 'Mean.blue3',  'Mean.nir1', 'Mean.nir2', 'Mean.nir3'), ~(scale(.) %>% as.vector))
Wels_A2_test <- Wels_A2_test[Wels_A2_test$Standard.deviation.red2 != 0,]
Wels_A2_test <- Wels_A2_test %>% mutate_at(c('Standard.deviation.red1', 'Standard.deviation.red2', 'Standard.deviation.red3', 'Standard.deviation.green1', 'Standard.deviation.green2', 'Standard.deviation.green3', 'Standard.deviation.blue1', 'Standard.deviation.blue2', 'Standard.deviation.blue3',  'Standard.deviation.nir1', 'Standard.deviation.nir2', 'Standard.deviation.nir3'), ~(scale(.) %>% as.vector))
Wels_A2_test["Mean.ndvi1"][Wels_A2_test["Mean.ndvi1"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndvi1"][Wels_A2_test["Standard.deviation.ndvi1"] <= 0.01] <- NA
Wels_A2_test["Mean.ndvi3"][Wels_A2_test["Mean.ndvi3"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndvi3"][Wels_A2_test["Standard.deviation.ndvi3"] <= 0.01] <- NA
Wels_A2_test["Mean.ndsi1"][Wels_A2_test["Mean.ndsi1"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndsi1"][Wels_A2_test["Standard.deviation.ndsi1"] <= 0.01] <- NA
Wels_A2_test["Mean.ndsi3"][Wels_A2_test["Mean.ndsi3"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndsi3"][Wels_A2_test["Standard.deviation.ndsi3"] <= 0.01] <- NA
Wels_A2_test["Mean.ndwi1"][Wels_A2_test["Mean.ndwi1"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndwi1"][Wels_A2_test["Standard.deviation.ndwi1"] <= 0.01] <- NA
Wels_A2_test["Mean.ndwi3"][Wels_A2_test["Mean.ndwi3"] == -1] <- NA
Wels_A2_test["Standard.deviation.ndwi3"][Wels_A2_test["Standard.deviation.ndwi3"] <= 0.01] <- NA
names(Wels_A2_test)[52] <- 'Area'

#### combine data frames ####

df_train <- rbind(Dens_A1_train,Horn_A1_train,Murg_A1_train,Seeh_A1_train,Thal_A1_train,Thal_A2_train,Waed_A1_train,Wels_A1_train)  #,Wels_A2_train
table(df_train$class_name)   # check class distribution


#### set correct types ####

df_train$class_name <- as.factor(df_train$class_name)  #set dependent variable (class_name) as factor (categorical)

#df_train[,1:2] <- sapply(df_train[,1:2],as.integer)
#df_train[,4:59] <- sapply(df_train[,4:59],as.numeric)
#df_train[,60:61] <- sapply(df_train[,60:61],as.integer)


#test_List <- list(Dens_A1_test, Horn_A1_test, Murg_A1_test, Seeh_A1_test, Thal_A1_test, Thal_A2_test, Waed_A1_test, Wels_A1_test, Wels_A2_test)
#test_List <- lapply(test_List,
#                     function(x){
#                       x[,1:2] <- sapply( x[,1:2] , as.integer)   # set columns as integer
#                       x[,4:59] <- sapply( x[,4:59] , as.numeric)   # set columns as numeric
#                       x[,60:61] <- sapply( x[,60:61] , as.integer)   # set columns as integer
#                       return(x)
#                     }
#)


#### rename feature columns for better reading ####



colnames = c(  "inner_X",
               "inner_Y",
               "class",
               "Mean_red1",
               "StdDev_red1",
               "Mean_red2",
               "StdDev_red2",
               "Mean_red3",
               "StdDev_red3",
               "Mean_green1",
               "StdDev_green1",
               "Mean_green2",
               "StdDev_green2",
               "Mean_green3",
               "StdDev_green3",
               "Mean_blue1",
               "StdDev_blue1",
               "Mean_blue2",
               "StdDev_blue2",
               "Mean_blue3",
               "StdDev_blue3",
               "Mean_nir1",
               "StdDev_nir1",
               "Mean_nir2",
               "StdDev_nir2",
               "Mean_nir3",
               "StdDev_nir3",
               "Mean_ndvi1",
               "StdDev_ndvi1",
               "Mean_ndvi2",
               "StdDev_ndvi2",
               "Mean_ndvi3",
               "StdDev_ndvi3",
               "Mean_ndsi1",
               "StdDev_ndsi1",
               "Mean_ndsi2",
               "StdDev_ndsi2",
               "Mean_ndsi3",
               "StdDev_ndsi3",
               "Mean_ndwi1",
               "StdDev_ndwi1",
               "Mean_ndwi3",
               "StdDev_ndwi3",
               "GLCM_Corr",
               "GLCM_StdDev",
               "Mean_NoR",
               "Mean_Z",
               "StdDev_Z",
               "Max_Z",
               "Mean_vhm",
               "StdDev_vhm",
               "Curv_Length",
               "Length_main",
               "Area",
               "Width",
               "Compactness",
               "Density",
               "coord_X",
               "coord_Y")

df_train <- setNames(df_train, colnames)
Dens_A1_train <- setNames(Dens_A1_train, colnames)
Dens_A1_test <- setNames(Dens_A1_test, colnames)
Horn_A1_train <- setNames(Horn_A1_train, colnames)
Horn_A1_test <- setNames(Horn_A1_test, colnames)
Murg_A1_train <- setNames(Murg_A1_train, colnames)
Murg_A1_test <- setNames(Murg_A1_test, colnames)
Seeh_A1_train <- setNames(Seeh_A1_train, colnames)
Seeh_A1_test <- setNames(Seeh_A1_test, colnames)
Thal_A1_train <- setNames(Thal_A1_train, colnames)
Thal_A1_test <- setNames(Thal_A1_test, colnames)
Thal_A2_train <- setNames(Thal_A2_train, colnames)
Thal_A2_test <- setNames(Thal_A2_test, colnames)
Waed_A1_train <- setNames(Waed_A1_train, colnames)
Waed_A1_test <- setNames(Waed_A1_test, colnames)
Wels_A1_train <- setNames(Wels_A1_train, colnames)
Wels_A1_test <- setNames(Wels_A1_test, colnames)
Wels_A2_train <- setNames(Wels_A2_train, colnames)
Wels_A2_test <- setNames(Wels_A2_test, colnames)



table(df_train$class)


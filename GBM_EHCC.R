library(survival)
library(gbm)
library(caret)
library(survcomp)
library(Hmisc)
library(readr)
train_data <- read_csv("C:/Users/guhong/Desktop/SEER_EHCC.csv",
                       col_types = cols(Race = col_factor(levels = c("0","1","2")),
                                        AFP = col_factor(levels = c("0", "1")),
                                        Vascular = col_factor(levels = c("0","1")),
                                        Multifocal = col_factor(levels = c("0","1")),
                                        Fibrosis = col_factor(levels = c("0","1")),
                                        Grade = col_factor(levels = c("0","1","2"))))
train_data = data.frame(train_data)
set.seed(123)
gbm_model <- gbm(Surv(time, status) ~ ., data = train_data, distribution = "coxph",n.trees = 2000,
                 interaction.depth = 3, n.minobsinnode = 5, shrinkage = 0.01)
train_predict <- predict(gbm_model, newdata = train_data, n.trees = 2000, type = "link")
train_predict = data.frame(train_predict)
train_predict = cbind(train_data[,1:2], train_predict)
write.csv(train_predict,file = "C:/Users/guhong/Desktop/train_predict.csv",quote=F)

# Independent validation
validation_data  <- read_csv("C:/Users/guhong/Desktop/validation_data .csv",
                       col_types = cols(Race = col_factor(levels = c("0","1","2")),
                                        AFP = col_factor(levels = c("0", "1")),
                                        Vascular = col_factor(levels = c("0","1")),
                                        Multifocal = col_factor(levels = c("0","1")),
                                        Fibrosis = col_factor(levels = c("0","1")),
                                        Grade = col_factor(levels = c("0","1","2"))))
validation_data = data.frame(validation_data)
validation_predict <- predict(gbm_model, newdata = validation_data, n.trees = 2000, type = "link")
validation_predict = data.frame(validation_predict)
validation_predict = cbind(validation_data[,1:2], validation_predict)
write.csv(validation_predict,file = "C:/Users/guhong/Desktop/validation_predict.csv",quote=F)

# Note.
# Race_levels = c(0,1,2),labels = c("White","Asian/Pacifific Islander","Black/American Indian/Alaskan")
# AFP_levels = c(0,1),labels = c("Normal","Elevated")
# Vascular_levels = c(0,1),labels = c("No","Yes")
# Multifocal_levels = c(0,1),labels = c("No","Yes")
# Fibrosis_levels = c(0,1),labels = c("None to moderate fibrosis","Severe fibrosis or cirrhosis")
# Grade_levels = c(0,1,2),labels = c("Well-differentiated","Moderately differentiated","Poorly differentiated or undifferentiated")
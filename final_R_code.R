library(plyr)
library(e1071)
library(ROSE)
library(caret)

#Importing the dataset
CervCancer.df <-  read.csv('risk_factors_cervical_cancer.csv')
#Removing ? marks
CervCancer.df[CervCancer.df == '?'] <- NA
# to remove the samples with NA values in STD columns
Cerv_excmiss <- CervCancer.df[!is.na(CervCancer.df$STDs.cervical.condylomatosis), ]
#missing vales after elimination NA in STD columns
na_count <-sapply(Cerv_excmiss, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#subset removing columns 12, 13, 26, 27 and 28
Cerv_sub <- subset(Cerv_excmiss, select = -c(12, 13,15,22, 26:28))
na_count <-sapply(Cerv_sub, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Function to replace NA with Median values
fillNAwithMedian <- function(x){
    na_index <- which(is.na(x))       
  median_x <- median(x, na.rm=T)
  x[na_index] <- median_x
  return(x)
}

#Function to convert categorical to numeric type
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Converting into numeric type
Cerv_sub[,2:11] <- data.frame(sapply(Cerv_sub[,2:11], as.numeric.factor))

#Replacing NA with median with Median values
Cerv_sub[,2:11] <- data.frame(lapply(Cerv_sub[,2:11], fillNAwithMedian))
na_count <-sapply(Cerv_sub, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Dividing data per the Tests
Cerv_sub_Schiller <- Cerv_sub[,c(1:25,27)]
Cerv_sub_Hinselmann <- Cerv_sub[,c(1:26)]
Cerv_sub_Citology <- Cerv_sub[,c(1:25,28)]
Cerv_sub_Biopsy <- Cerv_sub[,c(1:25,29)]


################################Schiller###############################
#Checking counts of 0s and 1s
table(Cerv_sub_Schiller$Schiller)
#Over sampling of 1s
Cerv_sub_Schiller_over <- ovun.sample(Schiller ~ ., data = Cerv_sub_Schiller, method = "over",N = 1360)$data
#Checking counts of 0s and 1s
table(Cerv_sub_Schiller_over$Schiller)
#Dividing into Training and Testing
set.seed(2)
train.index_sc <- sample(c(1:dim(Cerv_sub_Schiller_over)[1]), dim(Cerv_sub_Schiller_over)[1]*0.6)
train.df_sc <- Cerv_sub_Schiller_over[train.index_sc, ]
valid.df_sc <- Cerv_sub_Schiller_over[-train.index_sc, ]

classifier = svm(formula =  Schiller ~ .,
                 data = Cerv_sub_Schiller_over,
                 type = 'C-classification',
                 kernel = 'linear')



# Predicting the results for original dataset
schiller_pred = predict(classifier, newdata = Cerv_sub_Schiller[-26])

# Making the Confusion Matrix & Accuracy
schi_cm <- table(schiller_pred, Cerv_sub_Schiller$Schiller)
confusionMatrix(schi_cm)


################Hinselmann#####################
#Checking counts of 0s and 1s
table(Cerv_sub_Hinselmann$Hinselmann)
#Over sampling of 1s
Cerv_sub_Hinselmann_over <- ovun.sample(Hinselmann ~ ., data = Cerv_sub_Hinselmann, method = "over",N = 1436)$data
#Checking counts of 0s and 1s
table(Cerv_sub_Hinselmann_over$Hinselmann)
#Dividing into Training and Testing
set.seed(2)
train.index_hi <- sample(c(1:dim(Cerv_sub_Hinselmann_over)[1]), dim(Cerv_sub_Hinselmann_over)[1]*0.6)
train.df_hi <- Cerv_sub_Hinselmann_over[train.index_hi, ]
valid.df_hi <- Cerv_sub_Hinselmann_over[-train.index_hi, ]


classifier = svm(formula =  Hinselmann ~ .,
                 data = Cerv_sub_Hinselmann_over,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the results on the original dataset
hinselmann_pred = predict(classifier, newdata = Cerv_sub_Hinselmann[-26])
# Making the Confusion Matrix & Accuracy
hinsel_cm <- table(hinselmann_pred, Cerv_sub_Hinselmann$Hinselmann)
confusionMatrix(hinsel_cm)



###########################################Citology#############################################
#Checking counts of 0s and 1s
table(Cerv_sub_Citology$Citology)
#Over sampling of 1s
Cerv_sub_Citology_over <- ovun.sample(Citology ~ ., data = Cerv_sub_Citology, method = "over",N = 1424)$data
#Checking counts of 0s and 1s
table(Cerv_sub_Citology_over$Citology)
#Dividing into Training and Testing
set.seed(2)
train.index_ci <- sample(c(1:dim(Cerv_sub_Citology_over)[1]), dim(Cerv_sub_Citology_over)[1]*0.6)
train.df_ci <- Cerv_sub_Citology_over[train.index_ci, ]
valid.df_ci <- Cerv_sub_Citology_over[-train.index_ci, ]


classifier_ci = svm(formula =  Citology ~ .,
                    data = Cerv_sub_Citology_over,
                    type = 'C-classification',
                    kernel = 'linear')

# Predicting the results for the original dataset
citology_pred = predict(classifier, newdata = Cerv_sub_Citology[-26])
# Making the Confusion Matrix & Accuracy
citology_cm <- table(citology_pred, Cerv_sub_Citology$Citology)
confusionMatrix(citology_cm)


#####################Biopsy###################
#Checking counts of 0s and 1s
table(Cerv_sub_Biopsy$Biopsy)
#Over sampling of 1s
Cerv_sub_Biopsy_over <- ovun.sample(Biopsy ~ ., data = Cerv_sub_Biopsy, method = "over",N = 1400)$data
#Checking counts of 0s and 1s
table(Cerv_sub_Biopsy_over$Biopsy)
#Dividing into Training and Testing
set.seed(2)
train.index_bi <- sample(c(1:dim(Cerv_sub_Biopsy_over)[1]), dim(Cerv_sub_Biopsy_over)[1]*0.6)
train.df_bi <- Cerv_sub_Biopsy_over[train.index_bi, ]
valid.df_bi <- Cerv_sub_Biopsy_over[-train.index_bi, ]

classifier_bi = svm(formula =  Biopsy ~ .,
                    data = Cerv_sub_Biopsy_over,
                    type = 'C-classification',
                    kernel = 'linear')

# Predicting the results for the original dataset
biopsy_pred = predict(classifier, newdata = Cerv_sub_Biopsy[-26])
# Making the Confusion Matrix & Accuracy
biopsy_cm <- table(biopsy_pred, Cerv_sub_Biopsy$Biopsy)
confusionMatrix(biopsy_cm)




###########################Biopsy Logistic model#################
logit.reg_bi <- glm(Biopsy ~ . , data = Cerv_sub_Biopsy_over, family = "binomial")
options(scipen = 999)
summary(logit.reg_bi)

biopsy_log_pred = predict(logit.reg_bi, newdata = Cerv_sub_Biopsy)
table(Cerv_sub_Biopsy$Biopsy, biopsy_log_pred>0.5)


####################Schiller Logistic model#####################
logit.reg_si <- glm(Schiller ~ . , data = Cerv_sub_Schiller_over, family = "binomial")
options(scipen = 999)
summary(logit.reg_si)

schiller_log_pred = predict(logit.reg_si, newdata = Cerv_sub_Schiller)
table(Cerv_sub_Schiller$Schiller, schiller_log_pred>0.5)


###################Citology Logistic model##################
logit.reg_ci <- glm(Citology ~ . , data = Cerv_sub_Citology_over, family = "binomial")
options(scipen = 999)
summary(logit.reg_ci)

citology_log_pred = predict(logit.reg_ci, newdata = Cerv_sub_Citology)
table(Cerv_sub_Citology$Citology, citology_log_pred>0.5)


################Hinselmann Logistic model###################
logit.reg_hi <- glm(Hinselmann ~ . , data = Cerv_sub_Hinselmann_over, family = "binomial")
options(scipen = 999)
summary(logit.reg_hi)

hinselmann_log_pred = predict(logit.reg_hi, newdata = Cerv_sub_Hinselmann)
table(Cerv_sub_Hinselmann$Hinselmann, hinselmann_log_pred>0.5)


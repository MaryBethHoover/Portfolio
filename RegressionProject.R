library(tidyverse)   
library(knitr)
library("EnvStats")
library(VIM)
library(mice)
library(forcats)
library("car") 
library(randomForest)
library(corrplot)
library(imputeTS)
library(Metrics)
library(caTools)

#Read in dataset. Available from Kaggle https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data
housingData <- data.frame(read.csv(file = 'housingData.csv')) #read in the dataset
glimpse(housingData)

#View dataset columns
str(housingData)

#Create new variables of age, age since remodel, and age of garage
housingData<-housingData %>% dplyr::mutate( 
  age=YrSold-YearBuilt, ageSinceRemodel=YrSold-YearRemodAdd, ageofGarage=YrSold-GarageYrBlt)

#Create data reports
#create tibble of only numeric variables
housingSubset<-housingData %>% dplyr::select(is.numeric)
housingNumeric<-as.tibble(housingSubset) 

#create tibble of only non numeric variables
housingSubset_NotNumeric<-housingData %>% dplyr::select(!is.numeric)
housingSubset_NotNumeric<-as.tibble(housingSubset_NotNumeric)
housingFactor<-housingSubset_NotNumeric %>% dplyr::mutate(across(everything(), as.factor)) #mutates all the variables to factors

#Create functions to calculate the first and third quantiles
Quant1<-function(x,na.rm=TRUE) {quantile(x,na.rm=na.rm)[2]}
Quant3<-function(x,na.rm=TRUE) {quantile(x,na.rm=na.rm)[4]}

#Create function to calculate the unique values, missing values, and means of nonmissing values
myNumericSummary <- function(x){c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),min(x,na.rm=TRUE), Quant1(x,na.rm=TRUE), median(x,na.rm=TRUE), Quant3(x,na.rm=TRUE),max(x,na.rm=TRUE), sd(x,na.rm=TRUE))}

#Apply function
numericSummary <-housingNumeric %>% summarise(across(everything(), myNumericSummary)) 

#Bind column names
numericSummary <-cbind(stat=c("n","unique","missing","mean","min","Quant1","median","Quant3","max","sd"), numericSummary)

#pivot table, format percentages, and kable
numericSummaryFinal <- numericSummary %>% pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>% pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>% kable()

#Create function to outputs modes
getmodes <- function(variable,type=1) {tibble <- table(variable) 
mode1<-which.max(tibble) 
if (type==1) { return (names(mode1))}
  else if (type==2) {return (names(which.max(tibble[-mode1])))}else if (type==-1) {return (names(which.min(tibble))) }}

#function to output the first mod freq, second mode freq, or least common freq depending on the type given
getmodesCnt <- function(variable,type=1) {tibble <- table(variable)
mode1<-which.max(tibble)
  if (type==1) {return (max(tibble))}
  else if (type==2) {return (max(tibble[-mode1]))}
  else if (type==-1) {return (min(tibble))}}

#function to return length, missingness, uniqueness, frequencies, and nodes
myFactorSummary<-function(x){c(length(x), sum(is.na(x)), sum(is.na(x))/length(x)*100, 
  n_distinct(x), n_distinct(x)/length(x)*100 , round(getmodesCnt(x, type=1)/getmodesCnt(x, type=2), digits=2) , 
  getmodes(x,type=1), getmodesCnt(x, type=1), getmodes(x, type=2), getmodesCnt(x,type=2), getmodes(x,type=-1), 
  getmodesCnt(x,type=-1))}

#Create factor summary and bind column names
factorSummary <-housingFactor %>% summarise(across(everything(), myFactorSummary))
factorSummary <-cbind(stat=c("n","missing","missing_pct","unique","unique_pct","freqRatio",
                             "1st node","1st node freq","2nd node", "2nd node freq", "least common", "least common freq"), factorSummary)

#pivot table and kable
factorSummaryFinal <- factorSummary %>% pivot_longer("MSZoning":"SaleCondition", names_to = "variable", values_to = "value") %>%  pivot_wider(names_from = stat, values_from = value)%>% kable()

#display reports
numericSummaryFinal #Lot frontage has a large percent missing. I will impute lot frontage
factorSummaryFinal #Neighborhood, Exterior1st and Exterior2nd have many factors. I will collapse the exterior variables.

#Count missingness in lot frontage
sum(is.na(housingData$LotFrontage)==TRUE)

#impute missing lot frontage with cart (classification and regression trees)
temp<-mice(housingNumeric, method="cart", m=5)
housingNumeric<-complete(temp)
housingNumeric

#compare density plot of original data and imputed lot frontage. Imputation is successful.
densityplot(housingData$LotFrontage, xlab="Original Lot Frontage") #density plot of original lot frontage
densityplot(housingNumeric$LotFrontag, xlab="Imputed Lot Frontage") #density plot of imputed lot frontage

#Compare distribution, mean, and variance between original data and imputed lot frontage. Imputation is successful.
par(mfrow=c(2,1))
Mean<-round(mean(housingNumeric$LotFrontage,na.rm=T),3)  
Variance<-round(var(housingNumeric$LotFrontage,na.rm=T),3)
hist(housingNumeric$LotFrontage, main="Original Data", xlab="x")
abline(v = Mean, col = "red", lwd = 2)
text(250,250, label=paste("Mean:",Mean, "  Var:", Variance))
hist(housingNumeric$LotFrontage, main="Predictive Mean Matching", xlab="x")
Mean<-round(mean(housingNumeric$LotFrontage),3)
svar<-round(var(housingNumeric$LotFrontage),3)
abline(v = Mean, col = "red", lwd = 2)
text(250, 250, label=paste("Mean:",Mean, "  Var:", Variance))
par(mfrow=c(1,1))

#Explore collapsing the factors Exterior1st and Exterior2nd
#display table of the facors
table(housingFactor$Exterior1st) #8 factors have more than 50 utilizations
table(housingFactor$Exterior2nd) #7 factors have more than 50 utilizations

#collapse the variables
Exterior1st_var <- fct_collapse(housingFactor$Exterior1st,
                             level_1 = "VinylSd",level_2 = "MetalSd", level_3="HdBoard", level_4="Wd Sdng", level_5 = "Plywood",level_6 = "CemntBd", level_7="BrkFace", level_8="WdShing",other_level = "other")
housingFactor$Exterior1st<-Exterior1st_var
Exterior2nd_var <- fct_collapse(housingFactor$Exterior2nd,
                             level_1 = "VinylSd",level_2 = "MetalSd", level_3="HdBoard", level_4="Wd Sdng", level_5 = "Plywood",level_6 = "CmentBd", level_7="Wd Shng", other_level = "other")
housingFactor$Exterior2nd<-Exterior2nd_var

#Display new factors
table(housingFactor$Exterior1st)
table(housingFactor$Exterior2nd)

#Display a boxplot of Sale Price vs Neighborhood. There is a clear trend in sale price over neighborhoods
ggplot(housingData, aes(fct_reorder(Neighborhood,SalePrice, median, .desc=TRUE), y=SalePrice, ))+
  geom_boxplot(fill="grey")+theme_bw(base_size = 8)+ labs(x="Neighborhood")

#combine the numeric and factor tibble
housingData_final<-cbind(housingNumeric, housingFactor)


#Create heatmap to evaluate correlations
correlations <- cor(na.omit(housingNumeric))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method='square')

#Replace other missing data with 0 or "None"
Missing_indices <- sapply(housingData_final,function(x) sum(is.na(x)))
Missing_Summary <- data.frame(index = names(housingData_final),Missing_Values=Missing_indices)
Missing_Summary[Missing_Summary$Missing_Values > 0,]

#Convert Ally
housingData_final$temp <- as.character(housingData_final$Alley)
housingData_final$temp[which(is.na(housingData_final$Alley))] <- "None"
table(housingData_final$temp)
housingData_final$Alley <- as.factor(housingData_final$temp)

#Convert MSZoning
housingData_final$temp <- as.character(housingData_final$MSZoning)
housingData_final$temp[which(is.na(housingData_final$MSZoning))] <- "None"
table(housingData_final$temp)
housingData_final$MSZoning <- as.factor(housingData_final$temp)

#Convert MasVnrType
housingData_final$temp <- as.character(housingData_final$MasVnrType)
housingData_final$temp[which(is.na(housingData_final$MasVnrType))] <- "None"
table(housingData_final$temp)
housingData_final$MasVnrType <- as.factor(housingData_final$temp)

#Convert Utilities
housingData_final$temp <- as.character(housingData_final$Utilities)
housingData_final$temp[which(is.na(housingData_final$Utilities))] <- "None"
table(housingData_final$temp)
housingData_final$Utilities <- as.factor(housingData_final$temp)

#Convert Exterior1st
housingData_final$temp <- as.character(housingData_final$Exterior1st)
housingData_final$temp[which(is.na(housingData_final$Exterior1st))] <- "None"
table(housingData_final$temp)
housingData_final$Exterior1st <- as.factor(housingData_final$temp)

#Convert Exterior2nd
housingData_final$temp <- as.character(housingData_final$Exterior2nd)
housingData_final$temp[which(is.na(housingData_final$Exterior2nd))] <- "None"
table(housingData_final$temp)
housingData_final$Exterior2nd <- as.factor(housingData_final$temp)

#Convert BsmtQual
housingData_final$temp <- as.character(housingData_final$BsmtQual)
housingData_final$temp[which(is.na(housingData_final$BsmtQual))] <- "None"
table(housingData_final$temp)
housingData_final$BsmtQual <- as.factor(housingData_final$temp)

#Convert BsmtCond
housingData_final$temp <- as.character(housingData_final$BsmtCond)
housingData_final$temp[which(is.na(housingData_final$BsmtCond))] <- "None"
table(housingData_final$temp)
housingData_final$BsmtCond <- as.factor(housingData_final$temp)

#Convert BsmtExposure
housingData_final$temp <- as.character(housingData_final$BsmtExposure)
housingData_final$temp[which(is.na(housingData_final$BsmtExposure))] <- "None"
table(housingData_final$temp)
housingData_final$BsmtExposure <- as.factor(housingData_final$temp)

#Convert BsmtFinType1
housingData_final$temp <- as.character(housingData_final$BsmtFinType1)
housingData_final$temp[which(is.na(housingData_final$BsmtFinType1))] <- "None"
table(housingData_final$temp)
housingData_final$BsmtFinType1 <- as.factor(housingData_final$temp)

#Convert BsmtFinType2
housingData_final$temp <- as.character(housingData_final$BsmtFinType2)
housingData_final$temp[which(is.na(housingData_final$BsmtFinType2))] <- "None"
table(housingData_final$temp)
housingData_final$BsmtFinType2 <- as.factor(housingData_final$temp)

#Convert Electrical
housingData_final$temp <- as.character(housingData_final$Electrical)
housingData_final$temp[which(is.na(housingData_final$Electrical))] <- "None"
table(housingData_final$temp)
housingData_final$Electrical <- as.factor(housingData_final$temp)

#Convert KitchenQual
housingData_final$temp <- as.character(housingData_final$KitchenQual)
housingData_final$temp[which(is.na(housingData_final$KitchenQual))] <- "None"
table(housingData_final$temp)
housingData_final$KitchenQual <- as.factor(housingData_final$temp)

#Convert Functional
housingData_final$temp <- as.character(housingData_final$Functional)
housingData_final$temp[which(is.na(housingData_final$Functional))] <- "None"
table(housingData_final$temp)
housingData_final$Functional <- as.factor(housingData_final$temp)

#Convert FireplaceQu
housingData_final$temp <- as.character(housingData_final$FireplaceQu)
housingData_final$temp[which(is.na(housingData_final$FireplaceQu))] <- "None"
table(housingData_final$temp)
housingData_final$FireplaceQu <- as.factor(housingData_final$temp)

#Convert GarageType
housingData_final$temp <- as.character(housingData_final$GarageType)
housingData_final$temp[which(is.na(housingData_final$GarageType))] <- "None"
table(housingData_final$temp)
housingData_final$GarageType <- as.factor(housingData_final$temp)

#Convert GarageQual
housingData_final$temp <- as.character(housingData_final$GarageQual)
housingData_final$temp[which(is.na(housingData_final$GarageQual))] <- "None"
table(housingData_final$temp)
housingData_final$GarageQual <- as.factor(housingData_final$temp)

#Convert GarageCond
housingData_final$temp <- as.character(housingData_final$GarageCond)
housingData_final$temp[which(is.na(housingData_final$GarageCond))] <- "None"
table(housingData_final$temp)
housingData_final$GarageCond <- as.factor(housingData_final$temp)

#Convert GarageFinish
housingData_final$temp <- as.character(housingData_final$GarageFinish)
housingData_final$temp[which(is.na(housingData_final$GarageFinish))] <- "None"
table(housingData_final$temp)
housingData_final$GarageFinish <- as.factor(housingData_final$temp)

#Convert PoolQC
housingData_final$temp <- as.character(housingData_final$PoolQC)
housingData_final$temp[which(is.na(housingData_final$PoolQC))] <- "None"
table(housingData_final$temp)
housingData_final$PoolQC <- as.factor(housingData_final$temp)

#Convert Fence
housingData_final$temp <- as.character(housingData_final$Fence)
housingData_final$temp[which(is.na(housingData_final$Fence))] <- "None"
table(housingData_final$temp)
housingData_final$Fence <- as.factor(housingData_final$temp)

#Convert MiscFeature
housingData_final$temp <- as.character(housingData_final$MiscFeature)
housingData_final$temp[which(is.na(housingData_final$MiscFeature))] <- "None"
table(housingData_final$temp)
housingData_final$MiscFeature <- as.factor(housingData_final$temp)

#Convert SaleType
housingData_final$temp <- as.character(housingData_final$SaleType)
housingData_final$temp[which(is.na(housingData_final$SaleType))] <- "None"
table(housingData_final$temp)
housingData_final$SaleType <- as.factor(housingData_final$temp)

#remove temp column
housingData_final <- subset(housingData_final,select = -temp)

#split into 80% train and 20% test
num <- sample.split(housingData_final, SplitRatio=0.8)
train <- housingData_final[num, ]
test  <- housingData_final[-num, ]

#Perform feature selection using Random forest to get VIF
model_rf <- randomForest(SalePrice~., data = housingData_final, importance=TRUE)
importance <- importance(model_rf)
varImpPlot(model_rf)

#perform predictions
y_test_pred <-predict(model_rf,test)

#Calculate performance
print(RMSE(y_test_pred, test$SalePrice))


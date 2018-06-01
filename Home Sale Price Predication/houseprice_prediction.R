library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(RcmdrMisc)
library(VIM)
library(caret)
library(mice)
library(doSNOW)
library(randomForest)

#setwd (C:\Users\Sandy\Desktop\Sandhya\Sandhya\Kaggle_HP)

train<- read.csv("train.csv",header=TRUE,stringsAsFactors = TRUE,na.strings=c("", "NA", "NULL","NaN","nan"))
test<- read.csv("test.csv",header=TRUE,stringsAsFactors = TRUE,na.strings=c("", "NA", "NULL","NaN","nan"))
summary(train)
str(train)
library(ggplot2)
table(complete.cases(train))

num_train_cols <- sapply(train,is.numeric)
table(num_train_cols)
num_train <- train[,num_train_cols]
str(num_train)
Correl1 <- rcorr.adjust(as.matrix(num_train), use ="pairwise.complete.obs", type="pearson")
##Saving the correlation data in a excel sheets
write.csv(file = "Correlation.csv",Correl1$R$r)
intercorrelation <- find_correlation<-findCorrelation(Correl1$R$r, cutoff = .88)
num_train_1 <- num_train [,-27]

head(num_train)

colnames(num_train)
colnames(num_train_1)
str(num_train_1)
summary(num_train_1)
as.factor(num_train_1$MSSubClass)
str(num_train_1$MSSubClass)
factor(num_train_1$MSSubClass)
head(num_train_1)
as.factor(num_train_1$OverallQual)
as.factor(num_train_1$OverallCond)
as.factor(num_train_1$BsmtFinType2)
as.factor(num_train_1$BsmtFullBath)
as.factor(num_train_1$BsmtHalfBath)
as.factor(num_train_1$FullBath)
as.factor(num_train_1$HalfBath)
as.factor(num_train_1$BedroomAbvGr)
as.factor(num_train_1$KitchenAbvGr)
as.factor(num_train_1$Fireplaces)


#lm= Linear smoothing
ggplot(num_train_1,aes(x = YearBuilt,y = SalePrice)) +
geom_point() +
geom_smooth(method="lm")+
xlab("YearBuilt")+
ylab("SalePrice")
#labs(fill="SalePrice")

ggplot(num_train_1,aes(x = LotArea,y = SalePrice)) +
geom_point() + 
geom_smooth(method="lm")

ggplot(num_train_1,aes(x = LotArea,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = OverallQual ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = GrLivArea ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = GarageArea ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = TotalBsmtSF ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = X1stFlrSF ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = FullBath ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(num_train_1,aes(x = TotRmsAbvGrd ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")
ggplot(num_train_1,aes(x = GarageYrBlt ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")
ggplot(num_train_1,aes(x = MasVnrArea ,y = SalePrice)) +
  geom_point() + 
  geom_smooth(method="lm")

char_train_cols <- sapply(train,is.factor)

char_train<- train[,char_train_cols]

str(char_train)
dim(char_train)
dim(num_train_1)

saleprice_factor<- cbind(char_train,SalePrice=num_train_1$SalePrice)
dim(saleprice_factor)
colnames(saleprice_factor)

#Chi Square test

n <- ncol(saleprice_factor)
find_Pvalue <- data.frame(PValue = sample(x=300,size = n))

for(i in 1:n){
  find_Pvalue$PValue[i] <-  (chisq.test(saleprice_factor[,i],saleprice_factor[,c("SalePrice")])$p.value)
}

#Adding the Features to the P VaLUE 
find_Pvalue$Feature <- colnames(saleprice_factor)


# Selecte only features that are significant P<0.05


find_Pvalue <- find_Pvalue[find_Pvalue$PValue<0.05,] %>% arrange(PValue)

find_Pvalue_set <- find_Pvalue$Feature[1:6]

Final_feature_set <- c(find_Pvalue_set, "YearBuilt","LotArea","OverallQual","GrLivArea","GarageArea","X1stFlrSF","FullBath","TotRmsAbvGrd","GarageYrBlt","MasVnrArea")

Final_feature_set
# Build Model
# Linear Model
Model_data <- train[,Final_feature_set]


dim(Model_data)
str(Model_data)
names(Model_data)

table(complete.cases(Model_data))

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

Linear_Model1 <- lm(SalePrice ~ .,data = Model_data)
Final_Linear_Model <- step(Linear_Model1)
summary(Final_Linear_Model)


Linear_Model2<-lm(formula = SalePrice ~ KitchenQual + BsmtQual + YearBuilt + LotArea + OverallQual + GrLivArea + 
     GarageArea + X1stFlrSF + MasVnrArea, data = Model_data)

stop_cluster(cl)
Final_Linear_Model$anova
summary(Linear_Model2)

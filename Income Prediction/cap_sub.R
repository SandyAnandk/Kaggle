  # STEP 1: Load Libraries
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
  
  
  # STEP 2: Load Data
  setwd("C:\\Users\\Sandy\\Desktop\\Sandhya\\Sandhya\\DS_capstoneproject")
  train<- read.csv("train.csv",header=TRUE,stringsAsFactors = TRUE,na.strings=c("", "NA", "NULL","NaN","nan"))
  test<- read.csv("test.csv",header=TRUE,stringsAsFactors = TRUE,na.strings=c("", "NA", "NULL","NaN","nan"))
  train.lables<- read.csv("train_labels.csv",header=TRUE,stringsAsFactors = TRUE,na.strings=c("", "NA","nan", "NULL","NaN"))
  str(train)
  
  incomefinder<- merge(x=train, y=train.lables, by.x  = "row_id", by.y= "row_id")

  # STEP 3: Select all binary columns and convert them into Factor  
  Program_assoc <- incomefinder[, grepl("program_assoc_",names(incomefinder))]
  for(i in 1:ncol(Program_assoc)){
    Program_assoc[,i]  <- as.factor(Program_assoc[,i])
    Program_assoc[,i] <- addNA(Program_assoc[,i])
  }
  str(Program_assoc)
  
  # look at the median of income by School_ownership
  #hist(train.lables$income,freq = T)
  #aggregate(income ~ school__ownership , data = incomefinder, FUN=median)

  
  
  # Make all Binary columns as Category variables and add NA as a type
  Program_bachelors<- incomefinder[,grepl("program_bachelors_",names(incomefinder))]
  for (i in 1:ncol(Program_bachelors)){
    Program_bachelors[,i]<-as.factor(Program_bachelors[,i])
    Program_bachelors[,i]<-addNA(Program_bachelors[,i])
  }
  str(Program_bachelors)
  
  Program_certificate<-incomefinder[,grepl("program_certificate",names(incomefinder))]
  for(i in 1:ncol(Program_certificate)){
    Program_certificate[,i] <- as.factor(Program_certificate[,i])
    Program_certificate[,i]<-addNA(Program_certificate[,i])
  }
  str(Program_certificate)
 
   # awarded_
   
   School_Awarded<-incomefinder[,grepl("degrees_awarded_",names(incomefinder))]
   for(i in 1:ncol(School_Awarded)){
     School_Awarded[,i] <- as.factor(School_Awarded[,i])
     School_Awarded[,i]<-addNA(School_Awarded[,i])
   }
  str(School_Awarded)
  # 
  # Get rest of the factor variables from train set
  
  factor_columns<-sapply(incomefinder,is.factor)
  only_factor_incomefinder<-incomefinder[, factor_columns]
  only_factor_incomefinder <- only_factor_incomefinder[,!(names(only_factor_incomefinder) %in% c("school__degrees_awarded_predominant","school__degrees_awarded_highest"))]
  
  str(only_factor_incomefinder)
  
  for(i in 1:ncol(only_factor_incomefinder)){
    only_factor_incomefinder[,i] <- as.factor(only_factor_incomefinder[,i])
    only_factor_incomefinder[,i] <- addNA(only_factor_incomefinder[,i])
  }
  str(only_factor_incomefinder)
  
  # comnbine all factor data frames into one
  only_factor_incomefinder <- cbind( only_factor_incomefinder, Program_assoc, Program_bachelors, Program_certificate,School_Awarded )
  
  # Save the object for future use
  #save(only_factor_incomefinder,file ='Only_Factor_incomeFinder')
    
  # STEP 3: selectt all NUMERIC columns
  only_num_incomefinder <- incomefinder[!(names(incomefinder) %in% names(only_factor_incomefinder))]
  str(only_num_incomefinder)
  names(only_num_incomefinder)
  
  dim(only_num_incomefinder)
  dim(only_factor_incomefinder)
  save(only_factor_incomefinder, file="only_factor_incomefinder")
  save(only_num_incomefinder, file="only_num_incomefinder")
  
  dim(only_factor_incomefinder)
  dim(only_num_incomefinder)
  
  #Finding the pair-wise correlation for all numeric columns
  only_num_incomefinder1 <- only_num_incomefinder[!(names(only_num_incomefinder) %in% c("income"))] # remove income column
  
  names(only_num_incomefinder1) # check if income is removed
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  #impute Num data
   imputed_Data <- mice( data = only_num_incomefinder1
                        ,m = 1
                        ,method = "pmm"
                        ,maxit = 10
                        ,seed = 500
                        ,printFlag = F
  )
  stopCluster(cl)
  
  Imputed_only_num_incomefinder<- complete(imputed_Data,1)
  
  names(Imputed_only_num_incomefinder)
  # Find count of NA per column
  na_count <-(sapply(Imputed_only_num_incomefinder,function(y) sum(is.na(y))))
  na_count <- na_count[na_count>500]
  
  #Columns with most NA after imputation 
  names(na_count)
  Imputed_only_num_incomefinder <- Imputed_only_num_incomefinder[!(names(Imputed_only_num_incomefinder) %in% (names(na_count)))]
  # check for dimensions
  dim(Imputed_only_num_incomefinder)
  # check for NA
  table(is.na(Imputed_only_num_incomefinder))
  
  Correl1 <- rcorr.adjust(as.matrix(Imputed_only_num_incomefinder),use ="complete.obs", type="pearson")
  
  write.csv(Correl1$R$r, "correlation_10_24_pmm.csv")
  
  
  Remove_Correleated_cols <- (Correl1$R$r, cutoff = .70, verbose = FALSE)
  
  Imputed_only_num_incomefinder <- cbind(Imputed_only_num_incomefinder, incomefinder[c("income")])
  
  Correl1 <- rcorr.adjust(as.matrix(Imputed_only_num_incomefinder[,-Remove_Correleated_cols]),use ="complete.obs", type="pearson")
  # Final correlation chart
  #corrplot(Correl1$R$r,type="upper", col=NULL,method = "number",tl.cex = 0.75,number.cex = .6)    
  
  Num_Correlated_Cols <- Correl1$R$r[,"income"][which(abs(Correl1$R$r[,"income"])>0.4)]
  Num_Correlated_Cols[order(Num_Correlated_Cols,decreasing = TRUE)]
  Num_Feature_set <-  names(Num_Correlated_Cols)
  
  
  
  # STEP 4: 
  
  
  only_factor_incomefinder <- cbind(only_factor_incomefinder, income=incomefinder[,"income"])
  
  n <- ncol(only_factor_incomefinder)
  Category_Pvalue <- data.frame(PValue = sample(x=300,size = n))
  
  # Run chisq.test for all categorical features
  for(i in 1:n){
    Category_Pvalue$PValue[i] <-  (chisq.test(only_factor_incomefinder[,i],only_factor_incomefinder[,c("income")])$p.value)
  }
  # gETTING THE  column names correspoding to the p values
  Category_Pvalue$feature <- names(only_factor_incomefinder)
  
  # filteriing only completed cases
  Category_Pvalue <- Category_Pvalue[complete.cases(Category_Pvalue),]
  
  # Selecte only features that are significant P<0.05
  Category_Feature_set <- Category_Pvalue[Category_Pvalue$PValue<0.05,]$feature
  
  # List of possible categorical features that are significant to be included in model.
  Category_Pvalue[Category_Pvalue$PValue<0.05,] %>% arrange(PValue)
  
  Final_feature_set <- c(Category_Feature_set, Num_Feature_set)
  #Final_feature_set<- Final_feature_set[-12]

  
  # plot the variables and see the correlation
  my.data<-aggregate(income ~ school__region_id , data = incomefinder, FUN=median) %>% arrange(-income)
  head(my.data)
  p <- ggplot2::ggplot(data=my.data, aes(y = (income), col =school__region_id, fill= school__region_id, aplha=0.5, x = school__region_id)) +
    geom_bar (stat = "identity")       + 
    ggtitle("Repayment vs School_region ") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #p <- ggplotly(p)
  p
  
  
  # build model
  # 1. Linear model
  Model_data <- cbind(Imputed_only_num_incomefinder[,Num_Feature_set],only_factor_incomefinder[,Category_Feature_set[!(Category_Feature_set %in% "income" )]])
  
  dim(Model_data)
  str(Model_data)
  names(Model_data)
  
  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  
  Linear_Model1 <- lm(income ~ .,data = Model_data)
  Final_Linear_Model <- step(Linear_Model1)
  summary(Final_Linear_Model)

  stop_cluster(cl)
  Final_Linear_Model$anova

  Linear_Model2 <- lm( income ~ school__institutional_characteristics_level +
                         school__ownership +
                         school__degrees_awarded_highest + 
                         school__region_id + 
                         report_year +
                         school__faculty_salary +
                         student__share_firstgeneration_parents_highschool,data = Model_data)
  
  # Ploting the final linear Model
  par(mfrow=c(1,1))
  plot(density(Final_Linear_Model$residuals)," Residuals of the model", col="magenta", lwd=2)
  
  par(mfrow=c(2,2))
  plot(Final_Linear_Model)
  
  # Process Test Data
  test_data <- test[, c(Num_Feature_set[!(Num_Feature_set %in% "income" )], Category_Feature_set[!(Category_Feature_set %in% "income" )])]
  dim(test_data)
  str(test_data)
  
  
  for(i in 1:length(Category_Feature_set[!(Category_Feature_set %in% "income" )])){
    test_data[,Category_Feature_set[i]] <- as.factor(test_data[,Category_Feature_set[i]])
    test_data[,Category_Feature_set[i]] <- addNA(test_data[,Category_Feature_set[i]])
  }
  
  table(is.na(test_data))
  
  LM_predict <- predict(Final_Linear_Model, newdata =test_data) #, na.action = na.omit)
  
  LM_predict2 <- predict(Linear_Model2, newdata =test_data) #, na.action = na.omit)
  
  save(Imputed_only_num_incomefinder,File="Imputed_only_num_incomefinder.bin")
  
  
  
  # write the predicted output in the required format
  a<- data.frame(row_id = integer(length(LM_predict2)),income = numeric(length(LM_predict2)))
  a$row_id <- as.integer(test$row_id)
  a$income <- as.numeric(LM_predict2)
  rownames(a)<-NULL
  
  
  write.csv(a,"LM_Predict_10_24_1.csv", row.names = FALSE, na = "0.0",eol = "\r",col.names = T,quote = F)

  cl <- makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  
    #2 Boosted Tree
    if(!file.exists("Rf_model.bin")){
      
      control <-trainControl(method="repeatedcv", number=10, repeats=5, search="random")
      
      set.seed(1234)
      mtry <- 20 #sqrt(ncol(Model_data))
      my.metric <- "RMSE"
      my.tunegrid <- (expand.grid(.mtry=20)) #sqrt(ncol(Model_data))))
      
      #my.TrainingSet$school__carnegie_basic <- as.factor(my.TrainingSet$school__carnegie_basic)
      Rf_Model <- train(income ~ .
                             , data =  Model_data
                             , method = 'rf'
                             , ntree = 500
                             #, preProcess = c("knnImpute")
                             , trControl=control
                             , metric= my.metric
                             , tuneLength=15
                             , tuneGrid=my.tunegrid
                             , na.action = na.omit
      )    
    save(Rf_Model,file="Rf_model1_TuneLength15.bin")
    #str(Model_data)
    #save(boosted_model, file = "Rf_model1.bin")
  } else {
    load("Rf_model.bin")
  }
  stopCluster(cl)
  print(Rf_Model$finalModel)
  
  
  plot(boosted_model$finalModel) 
  
  RF_Model_Predict <- predict(Rf_Model, newdata =test_data, na.action = na.roughfix)
  
  
  # write the predicted output in the required format
  a<- data.frame(row_id = integer(length(RF_Model_Predict)),income = numeric(length(RF_Model_Predict)))
  a$row_id <- as.integer(test$row_id)
  a$income <- as.numeric(RF_Model_Predict)
  rownames(a)<-NULL
  
  Rf_Model
  write.csv(a,"RF_Predict_10_26_1_tuneLength.csv", row.names = FALSE, na = "0.0",eol = "\r",col.names = T,quote = F)
  
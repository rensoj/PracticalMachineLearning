PML <- function(){
     library(tidyverse)
     library(caret)
     
     if (!(exists("training",envir=.GlobalEnv) & exists("testing",envir=.GlobalEnv))){
          training <- tibble(read.csv("./pml-training.csv",stringsAsFactors=FALSE,na.strings=c("",NA)))
          testing <- tibble(read.csv("./pml-testing.csv",stringsAsFactors=FALSE,na.strings=c("",NA)))
          pct <- sapply(training,function(x){sum(!is.na(x))/length(x)})
          training <- training[,pct > 0.3];testing <- testing[,pct > 0.3]
          training <- training[,-(1:7)];testing <- testing[,-(1:7)]
          training$classe <- factor(training$classe)
          assign("training",training,envir=.GlobalEnv)
          assign("testing",testing,envir=.GlobalEnv)
     }
     
     set.seed(254)
     
     TrainCtrl <- trainControl(method="cv",number=5)
     
     #Fit1 <- train(classe ~ .,method="rf",data=training,trControl=TrainCtrl)
     #saveRDS(Fit1,file="~/Desktop/FitSave.rds")
     Fit1 <- readRDS("~/Desktop/FitSave.rds")
     
     print(Fit1)
     
     Pred <- predict(Fit1,testing)
     print(Pred)
     
     
}
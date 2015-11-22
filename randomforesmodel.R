#############################################################################################


set.seed(614)
library(lattice); library(ggplot2); library(caret)
pml.training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings=c("NA","NaN", " ","",'""','"#DIV/0!"','#DIV/0!',dec="." ))

##Removing problem variables - they are usually NA in test set and read as logic + removing 'X' which immedeately causes overvfit as A's-E's are sorted in the set
pml.training1<-pml.training[,-which(names(pml.training) %in% c('X','cvtd_timestamp','kurtosis_roll_belt', 'kurtosis_picth_belt', 'skewness_roll_belt', 'skewness_roll_belt.1', 'max_roll_belt', 'max_picth_belt', 'max_yaw_belt', 'min_roll_belt', 'min_pitch_belt', 'min_yaw_belt', 'amplitude_roll_belt', 'amplitude_pitch_belt', 'var_total_accel_belt', 'avg_roll_belt', 'stddev_roll_belt', 'var_roll_belt', 'avg_pitch_belt', 'stddev_pitch_belt', 'var_pitch_belt', 'avg_yaw_belt', 'stddev_yaw_belt', 'var_yaw_belt', 'var_accel_arm', 'kurtosis_roll_arm', 'kurtosis_picth_arm', 'kurtosis_yaw_arm', 'skewness_roll_arm', 'skewness_pitch_arm', 'skewness_yaw_arm', 'max_picth_arm', 'max_yaw_arm', 'min_roll_arm', 'min_yaw_arm', 'amplitude_yaw_arm', 'kurtosis_roll_dumbbell', 'kurtosis_picth_dumbbell', 'skewness_roll_dumbbell', 'skewness_pitch_dumbbell', 'max_roll_dumbbell', 'max_picth_dumbbell', 'max_yaw_dumbbell','min_roll_dumbbell', 'min_pitch_dumbbell', 'min_yaw_dumbbell', 'amplitude_roll_dumbbell', 'amplitude_pitch_dumbbell', 'var_accel_dumbbell', 'avg_roll_dumbbell', 'stddev_roll_dumbbell', 'var_roll_dumbbell', 'avg_pitch_dumbbell', 'stddev_pitch_dumbbell', 'var_pitch_dumbbell', 'avg_yaw_dumbbell', 'stddev_yaw_dumbbell', 'var_yaw_dumbbell', 'kurtosis_roll_forearm', 'kurtosis_picth_forearm', 'skewness_roll_forearm', 'skewness_pitch_forearm', 'max_picth_forearm', 'max_yaw_forearm', 'min_pitch_forearm', 'min_yaw_forearm', 'amplitude_roll_forearm', 'amplitude_pitch_forearm', 'var_accel_forearm'))]

inTrain <- createDataPartition(y=pml.training1$classe, p=0.9, list=FALSE)
training <- pml.training1[inTrain,]
testing <- pml.training1[-inTrain,]


nsv<-nearZeroVar(training,saveMetrics=TRUE)
nsv

training2<-training[,!(nsv$nzv)]
# training2 <- training2[,colSums(is.na(training2))<nrow(training2)] ## remove columns that consist from na's

tr2<-training2[complete.cases(training2),]


library(doMC)
registerDoMC(cores = 4)


library(randomForest)
model1 <- randomForest(classe~.,data=tr2,ncores=4)
print(model1)

predict1 <- predict(model1,tr2)
table(predict1, tr2$classe)

##Percent of correct predicitions
sum(predict1==tr2$classe)/length(tr2$classe)


################

my_pml_testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings=c("NA","NaN", " ","",'""','"#DIV/0!"','#DIV/0!'),dec="." )

## converting logicals to numeric
 l_columns<-which(sapply(my_pml_testing,class) %in% c("logical"))
 my_pml_testing[,l_columns]<-sapply(my_pml_testing[,l_columns],as.numeric)

my_pml_testing<-my_pml_testing[,-which(names(my_pml_testing) %in% c('X','cvtd_timestamp','kurtosis_roll_belt', 'kurtosis_picth_belt', 'skewness_roll_belt', 'skewness_roll_belt.1', 'max_roll_belt', 'max_picth_belt', 'max_yaw_belt', 'min_roll_belt', 'min_pitch_belt', 'min_yaw_belt', 'amplitude_roll_belt', 'amplitude_pitch_belt', 'var_total_accel_belt', 'avg_roll_belt', 'stddev_roll_belt', 'var_roll_belt', 'avg_pitch_belt', 'stddev_pitch_belt', 'var_pitch_belt', 'avg_yaw_belt', 'stddev_yaw_belt', 'var_yaw_belt', 'var_accel_arm', 'kurtosis_roll_arm', 'kurtosis_picth_arm', 'kurtosis_yaw_arm', 'skewness_roll_arm', 'skewness_pitch_arm', 'skewness_yaw_arm', 'max_picth_arm', 'max_yaw_arm', 'min_roll_arm', 'min_yaw_arm', 'amplitude_yaw_arm', 'kurtosis_roll_dumbbell', 'kurtosis_picth_dumbbell', 'skewness_roll_dumbbell', 'skewness_pitch_dumbbell', 'max_roll_dumbbell', 'max_picth_dumbbell', 'max_yaw_dumbbell','min_roll_dumbbell', 'min_pitch_dumbbell', 'min_yaw_dumbbell', 'amplitude_roll_dumbbell', 'amplitude_pitch_dumbbell', 'var_accel_dumbbell', 'avg_roll_dumbbell', 'stddev_roll_dumbbell', 'var_roll_dumbbell', 'avg_pitch_dumbbell', 'stddev_pitch_dumbbell', 'var_pitch_dumbbell', 'avg_yaw_dumbbell', 'stddev_yaw_dumbbell', 'var_yaw_dumbbell', 'kurtosis_roll_forearm', 'kurtosis_picth_forearm', 'skewness_roll_forearm', 'skewness_pitch_forearm', 'max_picth_forearm', 'max_yaw_forearm', 'min_pitch_forearm', 'min_yaw_forearm', 'amplitude_roll_forearm', 'amplitude_pitch_forearm', 'var_accel_forearm'))]

my_pml_testing2<-my_pml_testing[,!(nsv$nzv)]

#we do not even need an imputation - everything is fine now
#my_pml_testing2<-knnImputation(my_pml_testing2[,-length(my_pml_testing2)],k=10,distData=tr2[,-length(tr2)])


predict2 <- predict(model1,my_pml_testing2)
predict2

## B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 

## Bingo!s

answers <- as.character(predict2)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers) 

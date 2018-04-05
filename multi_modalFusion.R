rm(list=ls(all=TRUE));
setwd("E:/Rworkspace/NGFexperiment");
library(randomForest);

Measure<-c(1,2,3);
f=100#循环次数
for(z in 1:f)
{#求相似矩阵
source("SinglMode_SimilarMatrix.R");
#pet_sm<-SinglMode_SimilarMatrix(1,"E:/Rworkspace/NGFexperiment/","MCI_NCpetBalance2.mat","MCI_NClabelBalance2.csv");
mri_sm<-SinglMode_SimilarMatrix(2,"E:/Rworkspace/NGFexperiment/","MCI_NCmriBalance2.mat","MCI_NClabelBalance2.csv");
#csf_sm<-SinglMode_SimilarMatrix(3,"E:/Rworkspace/NGFexperiment/","MCI_NCcsfBalance2.mat","MCI_NClabelBalance2.csv");

#正则化相似矩阵
source("normalize_SimilarMatrix.R");
#pet_sm_N<-normalize_SimilarMatrix(pet_sm);
mri_sm_N<-normalize_SimilarMatrix(mri_sm);
#csf_sm_N<-normalize_SimilarMatrix(csf_sm);
write.csv(mri_sm_N,file="mri_sm_N.csv",quote=F);
#write.csv(pet_sm_N,file="pet_sm_N.csv",quote=F);
#write.csv(csf_sm_N,file="csf_sm_N.csv",quote=F);
# #稀疏矩阵
# source("Sparse_SimilarMatrix.R");
# K<-15;#K近邻参数可更改
# pet_sm_sparse<-Sparse_SimilarMatrix(K,pet_sm_N);
# mri_sm_sparse<-Sparse_SimilarMatrix(K,mri_sm_N);
# #csf_sm_sparse<-Sparse_SimilarMatrix(K,csf_sm_N);
# #
# # #融合
# t<-20;#迭代次数0
# m<-2;#模态数
# pet_sm_f<-pet_sm_N;
# mri_sm_f<-mri_sm_N;
# #csf_sm_f<-csf_sm_N;
# for(i in 1:t)
# {
#   pet_sm_f1<-pet_sm_sparse%*%((mri_sm_f)/(m-1))%*%t(pet_sm_sparse)+diag(ncol(pet_sm_N));
#   mri_sm_f1<-mri_sm_sparse%*%((pet_sm_f)/(m-1))%*%t(mri_sm_sparse)+diag(ncol(pet_sm_N));
#   #csf_sm_f1<-csf_sm_sparse%*%((pet_sm_f+mri_sm_f)/(m-1))%*%t(csf_sm_sparse)+diag(ncol(pet_sm_N));
#   pet_sm_f<-round(pet_sm_f1,5);
#   mri_sm_f<-round(mri_sm_f1,5);
#   #csf_sm_f<-round(csf_sm_f1,5);
# }
# sm_f<-(pet_sm_f+mri_sm_f)/m;
# sm_f<-round(sm_f,5);
# for(i in 1:ncol(sm_f))
# {sm_f[i,i]<-1;}
#  write.csv(sm_f,file="sm_f_MCI_NCBalance2_pet_mri.csv",quote=F);
#  writeMat("sm_f_MCI_NCBalance2_pet_mri.mat",sm_f=sm_f);

#绘制相似矩阵彩色图
picture<-mri_sm_N;
MIN<-min(picture);
picture1<-picture-diag(diag(picture));
MAX<-max(picture1);
for(i in 1:ncol(picture))
{picture[i,i]<-MAX;}
#for(i in 1:ncol(picture))
#{for(j in 1:ncol(picture))
#{picture[i,j]<-1-picture[i,j];} }
cc<-rainbow(ncol(picture),start=0,end=4/6);
heatmap(picture,Colv=NA,Rowv=NA,col = rainbow(256,start=0,end=4/6), scale = "column",
        ColSideColors = cc)

#划分训练集测试集
lable<-read.csv("MCI_NClabelBalance2.csv");
sm_fr<-read.csv("mri_sm_N.csv");
Data<-cbind(lable,sm_fr[-1]);
ind <- sample(2, nrow(Data), replace=TRUE, prob=c(0.7, 0.3)); 
trainData <- Data[ind==1,];  
testData <- Data[ind==2,]; 



#训练随机森林模型
rf <- randomForest(category~., data=trainData, ntree=5000, proximity=TRUE,importance=TRUE);
#测试随机森林模型
Pred <- predict(rf, newdata=testData);
StatisticPred<-table(Pred,testData$category);
source("MeasureValue.R");
sensitivity<-MeasureValue(TYPE=1,TP=StatisticPred[1,1],TN=StatisticPred[2,2],FP=StatisticPred[2,1],FN=StatisticPred[1,2],SUM=sum(StatisticPred))
specificity<-MeasureValue(TYPE=2,TP=StatisticPred[1,1],TN=StatisticPred[2,2],FP=StatisticPred[2,1],FN=StatisticPred[1,2],SUM=sum(StatisticPred))
accuracy<-MeasureValue(TYPE=3,TP=StatisticPred[1,1],TN=StatisticPred[2,2],FP=StatisticPred[2,1],FN=StatisticPred[1,2],SUM=sum(StatisticPred))
Measure<-rbind(Measure,c(sensitivity,specificity,accuracy));}
Measure<-rbind(Measure,c(mean(Measure[-1,1]),mean(Measure[-1,2]),mean(Measure[-1,3])))
Measure_mean<-as.matrix(Measure[-1,]);#最后一行是每个值多次试验的平均数
Measure1<-rbind(c("sensitivity","specificity","accuracy"),Measure_mean);
write.csv(Measure1,file="MCIvsNCMeasureValueBalance2_mri.csv",row.names = F,quote=F)
plot(c(1:f),Measure_mean[-1-f,1],ylim=c(0,1),type="o",col=3,main="AD classification by NGF",xlab="实验次数",ylab="精确度");
lines(Measure_mean[-1-f,2],type="o",col=10);
lines(Measure_mean[-1-f,3],type="o",col=4);
legend("bottomright",c("sensitivity","specificity","accuracy"),fill=c(3,10,4),bty="n",cex=0.8)

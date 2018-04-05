SinglMode_SimilarMatrix<-function(TYPE=0,PAth,Name,LableName)
  #参数说明
  #PAth,文件所在路径
  #Name,数据的名字mat格式，要加.mat
  #LableName,lable名字csv格式，要加.csv
  #TYPE,要计算的相似矩阵的模态
   #TYPE=1，pet
   #TYPE=2，mri
   #TYPE=3，csf
  
{
  #rm(Path,pathName,Data,dataM,FeatureNum,Lable,data_lable,SM,SM_M);
  library(randomForest);
  library(R.matlab);
  
  #读取数据
  Path<-PAth;
  pathName<- file.path(Path, Name);
  Data<- readMat(pathName);
  Lable<-read.csv(LableName);
  if((Name=="pet.mat")|TYPE==1){
    dataM<-Data$pet;
    #dataM<-Data$feature;
  }else if((Name=="mri.mat")|TYPE==2){
    dataM<-Data$mri;
  }else if((Name=="csf.mat")|TYPE==3){
    dataM<-Data$csf;
  }else{
    print("文件名有错误");
  }
  
  #求单个模态的相似矩阵
  data_lable<-cbind(dataM,Lable);#数据和LABLE放到同一个数据框中
  FeatureNum<-ncol(dataM);#当前模态特征数
  data_lable<-cbind(dataM,Lable);#数据和LABLE放到同一个数据框中
  SM<- randomForest(data_lable[,-FeatureNum-1],data_lable[,FeatureNum+1],ntree=5000,mtry=round(sqrt(FeatureNum)),prox=TRUE);
  SM_M<-SM$proximity;
  return(SM_M);
}
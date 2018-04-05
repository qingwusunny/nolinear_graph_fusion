SinglMode_SimilarMatrix<-function(TYPE=0,PAth,Name,LableName)
  #����˵��
  #PAth,�ļ�����·��
  #Name,���ݵ�����mat��ʽ��Ҫ��.mat
  #LableName,lable����csv��ʽ��Ҫ��.csv
  #TYPE,Ҫ��������ƾ����ģ̬
   #TYPE=1��pet
   #TYPE=2��mri
   #TYPE=3��csf
  
{
  #rm(Path,pathName,Data,dataM,FeatureNum,Lable,data_lable,SM,SM_M);
  library(randomForest);
  library(R.matlab);
  
  #��ȡ����
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
    print("�ļ����д���");
  }
  
  #�󵥸�ģ̬�����ƾ���
  data_lable<-cbind(dataM,Lable);#���ݺ�LABLE�ŵ�ͬһ�����ݿ���
  FeatureNum<-ncol(dataM);#��ǰģ̬������
  data_lable<-cbind(dataM,Lable);#���ݺ�LABLE�ŵ�ͬһ�����ݿ���
  SM<- randomForest(data_lable[,-FeatureNum-1],data_lable[,FeatureNum+1],ntree=5000,mtry=round(sqrt(FeatureNum)),prox=TRUE);
  SM_M<-SM$proximity;
  return(SM_M);
}
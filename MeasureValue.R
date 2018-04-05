MeasureValue<-function(TYPE,TP,TN,FP,FN,SUM)
  ##TYPE表示所求值的类型
  #TYPR==1:sensitivity
  #TYPR==2:specificity
  #TYPE==3:accuracy
  
{ if(TYPE==1)
{MV<-round(TP/(TP+FN),4);
   }
else if(TYPE==2){
  MV<-round(TN/(TN+FP),4);
}else if(TYPE==3){
  MV<-round((TP+TN)/SUM,4);
}else{MV<-round(3/10,4);
  print("TYPE类型错误，TYPR==1:sensitivity，TYPR==2:specificity，TYPE==3:score");}
return(MV);}
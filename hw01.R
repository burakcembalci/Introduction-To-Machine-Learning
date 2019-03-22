#setting working directory
setwd("C:/Users/burak/Desktop/Burak/KU/Comp421/comp_indr421_521_elec443_543_fall2018_hw01")
#defining safe_log not to get errors for log(0)

safe_log <- function(x) {
  return (log(x + 1e-100))
}

#reading data
imgs<-read.csv("hw01_data_set_images.csv",header= FALSE)
labels<-read.csv("hw01_data_set_labels.csv", header=FALSE)

#splitting the dataset as 25 train 14 test for A images
Aimgs_train<-imgs[1:25,]
Aimgs_test<-imgs[26:39,]
#splitting the dataset as 25 train 14 test for B images
Bimgs_train<-imgs[40:64,]
Bimgs_test<-imgs[65:78,]
#splitting the dataset as 25 train 14 test for C images
Cimgs_train<-imgs[79:103,]
Cimgs_test<-imgs[104:117,]
#splitting the dataset as 25 train 14 test for D images
Dimgs_train<-imgs[118:142,]
Dimgs_test<-imgs[143:156,]
#splitting the dataset as 25 train 14 test for E images
Eimgs_train<-imgs[157:181,]
Eimgs_test<-imgs[182:195,]

#row binding the test sets to form a one test set
test_set<-rbind.data.frame(Aimgs_test,Bimgs_test,Cimgs_test,Dimgs_test,Eimgs_test)
#row binding the train sets to form a one train set
train_set<-rbind.data.frame(Aimgs_train,Bimgs_train,Cimgs_train,Dimgs_train,Eimgs_train)
#getting the labels for train set
train_labels<-c(labels[1:25,],labels[40:64,],labels[79:103,],labels[118:142,],labels[157:181,])
#Type checking for personal purposes
typeof(Aimgs_train)
#Summing each images for A images train dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Aimgs_train_sumpix<-colSums(Aimgs_train)
#Dividing it with number of samples which is 25 for each train dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Aimgs_pcd<- Aimgs_train_sumpix/25
#Summing each images for B images train dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Bimgs_train_sumpix<-colSums(Bimgs_train)
#Dividing it with number of samples which is 25 for each train dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Bimgs_pcd<- Bimgs_train_sumpix/25
#Summing each images for C images train dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Cimgs_train_sumpix<-colSums(Cimgs_train)
#Dividing it with number of samples which is 25 for each train dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Cimgs_pcd<- Cimgs_train_sumpix/25
#Summing each images for D images train dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Dimgs_train_sumpix<-colSums(Dimgs_train)
#Dividing it with number of samples which is 25 for each train dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Dimgs_pcd<- Dimgs_train_sumpix/25

#Summing each images for E images train dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Eimgs_train_sumpix<-colSums(Eimgs_train)
#Dividing it with number of samples which is 25 for each train dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Eimgs_pcd<- Eimgs_train_sumpix/25



#Summing each images for A images test dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Aimgs_test_sumpix<-colSums(Aimgs_test)
#Dividing it with number of samples which is 14 for each test dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Aimgs_test_pcd<- Aimgs_test_sumpix/14
#Summing each images for B images test dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Bimgs_test_sumpix<-colSums(Bimgs_test)
#Dividing it with number of samples which is 14 for each test dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Bimgs_test_pcd<- Bimgs_test_sumpix/14
#Summing each images for C images test dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Cimgs_test_sumpix<-colSums(Cimgs_test)
#Dividing it with number of samples which is 14 for each test dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Cimgs_test_pcd<- Cimgs_test_sumpix/14
#Summing each images for D images test dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Dimgs_test_sumpix<-colSums(Dimgs_test)
#Dividing it with number of samples which is 14 for each test dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Dimgs_test_pcd<- Dimgs_test_sumpix/14

#Summing each images for E images test dataset first pixel, second pixel etc... (Sigma i=1,i=N )Xid*1(Yi=C) #lecture notes 5 page 6 calculating the pcd first step
Eimgs_test_sumpix<-colSums(Eimgs_test)
#Dividing it with number of samples which is 14 for each test dataset class (Sigma i=1 to i=N) 1*(Yi=C) #lecture notes 5 page 6 calculating the pcd second step
Eimgs_test_pcd<- Eimgs_test_sumpix/14


#Column bind of pcds for each class of train dataset
pcd<-cbind(Aimgs_pcd,Bimgs_pcd,Cimgs_pcd,Dimgs_pcd,Eimgs_pcd)
#OUTPUT OF QUESTION 4
pcd[,1]
pcd[,2]
pcd[,3]
pcd[,4]
pcd[,5]
#wrong approach as it uses PCD of test data not to be graded or evaluated as it is only left here for me to see my mistake
gcA_test<- function(data){
  totalA_test=0  
  totalA_test=totalA_test+(as.matrix(data)%*%safe_log(Aimgs_test_pcd)+ as.matrix((1-data))%*%safe_log(1-Aimgs_test_pcd))+safe_log(0.2)
    
  totalA_test
}
#wrong approach as it uses PCD of test data not to be graded or evaluated as it is only left here for me to see my mistake
gcB_test<- function(data){
  totalB_test=0  
  totalB_test=totalB_test+(as.matrix(data)%*%safe_log(Bimgs_test_pcd)+ as.matrix((1-data))%*%safe_log(1-Bimgs_test_pcd))+safe_log(0.2)
  
  totalB_test
}
#wrong approach as it uses PCD of test data not to be graded or evaluated as it is only left here for me to see my mistake
gcC_test<- function(data){
  totalC_test=0  
  totalC_test=totalC_test+(as.matrix(data)%*%safe_log(Cimgs_test_pcd)+ as.matrix((1-data))%*%safe_log(1-Cimgs_test_pcd))+safe_log(0.2)
  
  totalC_test
}
#wrong approach as it uses PCD of test data not to be graded or evaluated as it is only left here for me to see my mistake
gcD_test<- function(data){
  totalD_test=0  
  totalD_test=totalD_test+(as.matrix(data)%*%safe_log(Dimgs_test_pcd)+ as.matrix((1-data))%*%safe_log(1-Dimgs_test_pcd))+safe_log(0.2)
  
  totalD_test
}
#wrong approach as it uses PCD of test data not to be graded or evaluated as it is only left here for me to see my mistake
gcE_test<- function(data){
  totalE_test=0
    totalE_test<- totalE_test+(as.matrix((data))%*%safe_log(Eimgs_test_pcd)+ as.matrix((1-data))%*%safe_log(1-Eimgs_test_pcd))+safe_log(0.2)
  
  totalE_test
}
#score funtion which uses PCD of training data which takes data as dataset to give the similarity score of the data corresponding to image set of A. #lecture notes 5 page 6 Gc(x)=...
gcA<- function(data){
  totalA=0  
  totalA=totalA+(as.matrix(data)%*%safe_log(Aimgs_pcd)+ as.matrix((1-data))%*%safe_log(1-Aimgs_pcd))+safe_log(0.2)
  
  totalA
}
#score funtion which uses PCD of training data which takes data as dataset to give the similarity score of the data corresponding to image set of B.#lecture notes 5 page 6 Gc(x)=...
gcB<- function(data){
  totalB=0  
  totalB=totalB+(as.matrix(data)%*%safe_log(Bimgs_pcd)+ as.matrix((1-data))%*%safe_log(1-Bimgs_pcd))+safe_log(0.2)
  
  totalB
}
#score funtion which uses PCD of training data which takes data as dataset to give the similarity score of the data corresponding to image set of C.#lecture notes 5 page 6 Gc(x)=...
gcC<- function(data){
  totalC=0  
  totalC=totalC+(as.matrix(data)%*%safe_log(Cimgs_pcd)+ as.matrix((1-data))%*%safe_log(1-Cimgs_pcd))+safe_log(0.2)
  
  totalC
}
#score funtion which uses PCD of training data which takes data as dataset to give the similarity score of the data corresponding to image set of D.#lecture notes 5 page 6 Gc(x)=...
gcD<- function(data){
  totalD=0  
  totalD=totalD+(as.matrix(data)%*%safe_log(Dimgs_pcd)+ as.matrix((1-data))%*%safe_log(1-Dimgs_pcd))+safe_log(0.2)
  
  totalD
}
#score funtion which uses PCD of training data which takes data as dataset to give the similarity score of the data corresponding to image set of E.#lecture notes 5 page 6 Gc(x)=...
gcE<- function(data){
  totalE=0
  totalE<- totalE+(as.matrix((data))%*%safe_log(Eimgs_pcd)+ as.matrix((1-data))%*%safe_log(1-Eimgs_pcd))+safe_log(0.2)
  
  totalE
}
#some failed/ inefficient/ hard to use functions which is  the initial point of my later functions to create confusion matrix not to be graded or/ evaluated as this only lasts for me to see my mistakes 

#  compareFa<-function(data){
#   Amax<- max(gcA(data),gcB(data),gcC(data),gcD(data),gcE(data))
#   Amatrix= matrix(data = c(0,0,0,0,0),nrow = 5,ncol =1 )
#   if(Amax == gcA(data)){
#     Amatrix[1,1]=Amatrix[1,1]+1
#   }else if (Amax ==gcB(data)){
#     Amatrix[2,1]=Amatrix[2,1]+1
#   }else if (Amax ==gcC(data)){
#     Amatrix[3,1]=Amatrix[3,1]+1
#   }else if (Amax ==gcD(data)){
#     Amatrix[4,1]=Amatrix[4,1]+1
#   }else if (Amax ==gcE(data)){
#     Amatrix[5,1]=Amatrix[5,1]+1
#   }else Amatrix[1,1]=Amtarix[1,1]+1
#   Amatrix
#  }
#  compareFb<-function(data){
#    Bmax<- max(gcA(data),gcB(data),gcC(data),gcD(data),gcE(data))
#    Bmatrix= matrix(data = c(0,0,0,0,0),nrow = 5,ncol =1 )
#    if(Bmax == gcA(data)){
#      Bmatrix[1,1]=Bmatrix[1,1]+1
#    }else if (Bmax ==gcB(data)){
#      Bmatrix[2,1]=Bmatrix[2,1]+1
#    }else if (Bmax ==gcC(data)){
#      Bmatrix[3,1]=Bmatrix[3,1]+1
#    }else if (Bmax ==gcD(data)){
#      Bmatrix[4,1]=Bmatrix[4,1]+1
#    }else if (Bmax ==gcE(data)){
#      Bmatrix[5,1]=Bmatrix[5,1]+1
#    }
#    Bmatrix
#  }
#  compareFc<-function(data){
#    Cmax<- max(gcA(data),gcB(data),gcC(data),gcD(data),gcE(data))
#    Cmatrix= matrix(data = c(0,0,0,0,0),nrow = 5,ncol =1 )
#    if(Cmax == gcA(data)){
#      Cmatrix[1,1]=Cmatrix[1,1]+1
#    }else if (Cmax ==gcB(data)){
#      Cmatrix[2,1]=Cmatrix[2,1]+1
#    }else if (Cmax ==gcC(data)){
#      Cmatrix[3,1]=Cmatrix[3,1]+1
#    }else if (Cmax ==gcD(data)){
#      Amatrix[4,1]=Cmatrix[4,1]+1
#    }else if (Cmax ==gcE(data)){
#      Cmatrix[5,1]=Cmatrix[5,1]+1
#    }
#    Cmatrix
#  }
#  compareFd<-function(data){
#    Dmax<- max(gcA(data),gcB(data),gcC(data),gcD(data),gcE(data))
#    Dmatrix= matrix(data = c(0,0,0,0,0),nrow = 5,ncol =1 )
#    if(Dmax == gcA(data)){
#      Dmatrix[1,1]=Dmatrix[1,1]+1
#    }else if (Dmax ==gcB(data)){
#      Dmatrix[2,1]=Dmatrix[2,1]+1
#    }else if (Dmax ==gcC(data)){
#      Dmatrix[3,1]=Dmatrix[3,1]+1
#    }else if (Dmax ==gcD(data)){
#      Dmatrix[4,1]=Dmatrix[4,1]+1
#    }else if (Dmax ==gcE(data)){
#      Dmatrix[5,1]=Dmatrix[5,1]+1
#    }
#    Dmatrix
#  }
 #  compareFe<-function(data){
 #  
 #      for(i in 0:length(data)-1){
 #    Emax<- max(gcA(data)[,i],gcB(data)[,i],gcC(data)[,i],gcD(data)[i],gcE(data)[,i])
 #    Ematrix= matrix(data = c(0,0,0,0,0),nrow = 5,ncol =1 )
 #    if(Emax == gcA(data)){
 #      Ematrix[1,1]=Ematrix[1,1]+1
 #    }else if (Emax ==gcB(as.matrix(data))){
 #      Ematrix[2,1]=Ematrix[2,1]+1
 #    }else if (Emax ==gcC(data)){
 #      Ematrix[3,1]=Ematrix[3,1]+1
 #    }else if (Emax ==gcD(data)){
 #      Ematrix[4,1]=Ematrix[4,1]+1
 #    }else if (Emax ==gcE(data)){
 #      Ematrix[5,1]=Ematrix[5,1]+1
 #    }
 #   
 #    }
 #    Ematrix
 #    }
 # compareFe(train_set)
 # 
 # 

#just to see the output and do some manual computing before generalizing the function
gcA(train_set)
#just to see the output and do some manual computing before generalizing the function
gcB(train_set)
#just to see the output and do some manual computing before generalizing the function
gcC(train_set)
#just to see the output and do some manual computing before generalizing the function
gcD(train_set)
#just to see the output and do some manual computing before generalizing the function
gcE(train_set)
#just to see the output and do some manual computing before generalizing the function
gcA(train_set)


#creating a matrix to store the class scores of each element of the training set using the PCD values of A,B,C,D and E train sets
mtrx<-as.matrix(cbind(gcA(train_set),gcB(train_set),gcC(train_set),gcD(train_set),gcE(train_set)))
#finding the maximum element in each row to decide which one gives the biggest class score gcA,gcB,gcC,gcD or gcE
maxofrows<-apply(mtrx, 1,max)


mtrx_test<-as.matrix(cbind(gcA(test_set),gcB(test_set),gcC(test_set),gcD(test_set),gcE(test_set)))
maxofrows_test<-apply(mtrx_test,1,max)

cmatrix_train=matrix(0,nrow = 5,ncol = 5)
#with a for loop iterating the whole train data set building the confusion matrix by comparing the maximum element to the classes scores using different PCDs.
#knowing that first 25 element belongs to class A next ,25 belongs to class B next 25 belongs to class C, next 25 belongs to class D, next 25 belongs to class E
#otherwise labels set would be in use insted of indexes.
for(i in 1:125){
  if(i<26){
    if(maxofrows[i]==gcA(train_set)[i]){
        cmatrix_train[1,1]=cmatrix_train[1,1]+1
    }else if (maxofrows[i]==gcB(train_set)[i]){
      cmatrix_train[2,1]=cmatrix_train[2,1]+1
    }else if(maxofrows[i]==gcC(train_set)[i]){
      cmatrix_train[3,1]=cmatrix_train[3,1]+1
    }else if(maxofrows[i]==gcD(train_set)[i]){
      cmatrix_train[4,1]=cmatrix_train[4,1]+1
    }else{
      cmatrix_train[5,1]=cmatrix_train[5,1]+1
    }
  }else if (i<51){
    if(maxofrows[i]==gcA(train_set)[i]){
      cmatrix_train[1,2]=cmatrix_train[1,2]+1
    }else if (maxofrows[i]==gcB(train_set)[i]){
      cmatrix_train[2,2]=cmatrix_train[2,2]+1
    }else if(maxofrows[i]==gcC(train_set)[i]){
      cmatrix_train[3,2]=cmatrix_train[3,2]+1
    }else if(maxofrows[i]==gcD(train_set)[i]){
      cmatrix_train[4,2]=cmatrix_train[4,2]+1
    }else{
      cmatrix_train[5,2]=cmatrix_train[5,2]+1
    }

  }else if (i<=75){
    if(maxofrows[i]==gcA(train_set)[i]){
      cmatrix_train[1,3]=cmatrix_train[1,3]+1
    }else if (maxofrows[i]==gcB(train_set)[i]){
      cmatrix_train[2,3]=cmatrix_train[2,3]+1
    }else if(maxofrows[i]==gcC(train_set)[i]){
      cmatrix_train[3,3]=cmatrix_train[3,3]+1
    }else if(maxofrows[i]==gcD(train_set)[i]){
      cmatrix_train[4,3]=cmatrix_train[4,3]+1
    }else{
      cmatrix_train[5,3]=cmatrix_train[5,3]+1
    }
  }else if(i<= 100){
    if(maxofrows[i]==gcA(train_set)[i]){
      cmatrix_train[1,4]=cmatrix_train[1,4]+1
    }else if (maxofrows[i]==gcB(train_set)[i]){
      cmatrix_train[2,4]=cmatrix_train[2,4]+1
    }else if(maxofrows[i]==gcC(train_set)[i]){
      cmatrix_train[3,4]=cmatrix_train[3,4]+1
    }else if(maxofrows[i]==gcD(train_set)[i]){
      cmatrix_train[4,4]=cmatrix_train[4,4]+1
    }else{
      cmatrix_train[5,4]=cmatrix_train[5,4]+1
    }
  }else{
    if(maxofrows[i]==gcA(train_set)[i]){
      cmatrix_train[1,5]=cmatrix_train[1,5]+1
    }else if (maxofrows[i]==gcB(train_set)[i]){
      cmatrix_train[2,5]=cmatrix_train[2,5]+1
    }else if(maxofrows[i]==gcC(train_set)[i]){
      cmatrix_train[3,5]=cmatrix_train[3,5]+1
    }else if(maxofrows[i]==gcD(train_set)[i]){
      cmatrix_train[4,5]=cmatrix_train[4,5]+1
    }else{
      cmatrix_train[5,5]=cmatrix_train[5,5]+1
    }
  }

}
#confusion matrix for training dataset
cmatrix_train

#similar to the training set approach but instead gcClass functions take test_set as data instead of training sets and iterate over it accordingly.
cmatrix_test=matrix(0,nrow = 5,ncol = 5)
for(i in 1:70){
  if(i<15){
    if(maxofrows_test[i]==gcA(test_set)[i]){
      cmatrix_test[1,1]=cmatrix_test[1,1]+1
    }else if (maxofrows_test[i]==gcB(test_set)[i]){
      cmatrix_test[2,1]=cmatrix_test[2,1]+1
    }else if(maxofrows_test[i]==gcC(test_set)[i]){
      cmatrix_test[3,1]=cmatrix_test[3,1]+1
    }else if(maxofrows_test[i]==gcD(test_set)[i]){
      cmatrix_test[4,1]=cmatrix_test[4,1]+1
    }else{
      cmatrix_test[5,1]=cmatrix_test[5,1]+1
    }
  }else if (i<29){
    if(maxofrows_test[i]==gcA(test_set)[i]){
      cmatrix_test[1,2]=cmatrix_test[1,2]+1
    }else if (maxofrows_test[i]==gcB(test_set)[i]){
      cmatrix_test[2,2]=cmatrix_test[2,2]+1
    }else if(maxofrows_test[i]==gcC(test_set)[i]){
      cmatrix_test[3,2]=cmatrix_test[3,2]+1
    }else if(maxofrows_test[i]==gcD(test_set)[i]){
      cmatrix_test[4,2]=cmatrix_test[4,2]+1
    }else{
      cmatrix_test[5,2]=cmatrix_test[5,2]+1
    }
    
  }else if (i<=42){
    if(maxofrows_test[i]==gcA(test_set)[i]){
      cmatrix_test[1,3]=cmatrix_test[1,3]+1
    }else if (maxofrows_test[i]==gcB(test_set)[i]){
      cmatrix_test[2,3]=cmatrix_test[2,3]+1
    }else if(maxofrows_test[i]==gcC(test_set)[i]){
      cmatrix_test[3,3]=cmatrix_test[3,3]+1
    }else if(maxofrows_test[i]==gcD(test_set)[i]){
      cmatrix_test[4,3]=cmatrix_test[4,3]+1
    }else{
      cmatrix_test[5,3]=cmatrix_test[5,3]+1
    }
  }else if(i<= 56){
    if(maxofrows_test[i]==gcA(test_set)[i]){
      cmatrix_test[1,4]=cmatrix_test[1,4]+1
    }else if (maxofrows_test[i]==gcB(test_set)[i]){
      cmatrix_test[2,4]=cmatrix_test[2,4]+1
    }else if(maxofrows_test[i]==gcC(test_set)[i]){
      cmatrix_test[3,4]=cmatrix_test[3,4]+1
    }else if(maxofrows_test[i]==gcD(test_set)[i]){
      cmatrix_test[4,4]=cmatrix_test[4,4]+1
    }else{
      cmatrix_test[5,4]=cmatrix_test[5,4]+1
    }
  }else{
    if(maxofrows_test[i]==gcA(test_set)[i]){
      cmatrix_test[1,5]=cmatrix_test[1,5]+1
    }else if (maxofrows_test[i]==gcB(test_set)[i]){
      cmatrix_test[2,5]=cmatrix_test[2,5]+1
    }else if(maxofrows_test[i]==gcC(test_set)[i]){
      cmatrix_test[3,5]=cmatrix_test[3,5]+1
    }else if(maxofrows_test[i]==gcD(test_set)[i]){
      cmatrix_test[4,5]=cmatrix_test[4,5]+1
    }else{
      cmatrix_test[5,5]=cmatrix_test[5,5]+1
    }
  }
  
}

#Confusion matrix for test set OUTPUT OF QUESTION 6
cmatrix_test

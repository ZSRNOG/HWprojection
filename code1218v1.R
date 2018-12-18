library(VGAM)
library(RODBC)
library(TailRank)
#library(dplyr)

dbhandle <- odbcDriverConnect('driver={SQL Server}; server=10.22.97.90; uid=sa; pwd=!QAZ2wsx; 
                              database=R123DATA') #TESTdatabase为数据库
dat <- sqlQuery(dbhandle, "SELECT ALL [案例名称]
                ,[DateHour]
                ,[BoardNum]
                ,[FailNum]
                FROM [R123DATA].[dbo].[批次问题特征数据] order by DateHour asc ")
index <- levels(dat$案例名称)

funx <- function(id){
  dt <- dat[dat$案例名称==index[id],-1]
  #plot(dt$FailNum,type='l')
  nfix=50
  tre=TRUE
  while (tre) {
    index1 <- which(dt$BoardNum<nfix)
    if(any(index1!=nrow(dt))) {
      dt[index1,2:3]<- dt[index1,2:3] + dt[index1+1,2:3]
      dt <- dt[-index1,]
    }else{
      dt[nrow(dt),2:3]<- dt[nrow(dt),2:3] + dt[nrow(dt)-1,2:3]
    }
    
    tre=any(dt$BoardNum<nfix)
  }
  
  nfails=rbind(dt$FailNum,dt$BoardNum)
  nfailsx=nfails
  nfails[2,]=nfailsx[2,]-nfailsx[1,]
  
  ##可信水平
  alpha=0.95
  
  ##beta-binomial分布的参数值1:nb-5, 2:nb-4,3:nb-3,4:nb-2
  
  
  block=10000
  ##ss:组的个数
  nnfails=nfails
  ss=ncol(nfails);ns=apply(nnfails,2,sum)
  ##thres： 可信区间上限
  thres=thres1=rep(0,ss)
  if(any(nfails[1,]!=0)){
  if(ss>block+1){
    for(i in (block+1):ss){
      shap1=sum(nnfails[1,(i-block):(i-1)])+1
      shap2=sum(nnfails[2,(i-block):(i-1)])+1
      thres[i]=qbb(alpha,ns[i],shap1,shap2)+1
      thres1[i]=floor(mean(thres[(i):(i-1)]))
      alarm <- nfails[1,i]>thres[i]
      nnfails[1,i]=ifelse(alarm,mean(nnfails[1,(i-block):(i-1)]),nnfails[1,i])
    }
    for (i in 2:block) {
      shap1=sum(nnfails[1,1:i])+1
      shap2=sum(nnfails[2,1:i])+1
      thres1[i]=qbb(alpha,ns[i+1],shap1,shap2)+1
    }
  }else{
    for (i in 2:ss) {
      shap1=sum(nnfails[1,1:(i-1)])+1
      shap2=sum(nnfails[2,1:(i-1)])+1
      thres1[i]=qbb(alpha,ns[i],shap1,shap2)+1
    }
  }
  alarm=which(nfails[1,]>thres1)
  if(length(alarm)>0){
    re=dt[alarm+1,]
    write.csv(re,paste0(index[id], ".csv"))
  }
  }
  #return(re)
  png(file = paste0(index[id], ".png"))
  plot(2:ss,thres1[-1],type='l',col='red')
  points(2:ss,nfails[1,-1],type = 'h')
  dev.off()
  print(id)
}

setwd("D:/hw/V2/")

lapply(1:length(index), funx)

  

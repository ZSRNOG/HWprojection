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
  print(id)
  dt <- dat[dat$案例名称==index[id],-1]
  #plot(dt$FailNum,type='l')
  nfix=50
  if(all(dt$BoardNum)<nfix){
    dt=dt
  }else{
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
  }
  nfails=rbind(dt$FailNum,dt$BoardNum)
  nfailsx=nfails
  nfails[2,]=nfailsx[2,]-nfailsx[1,]
  
  ##可信水平
  alpha=0.995
  
  ##beta-binomial分布的参数值1:nb-5, 2:nb-4,3:nb-3,4:nb-2
  
  
  ##beta-binomial分布的参数值
  shap1=1+cumsum(nfails[1,])
  shap2=1+cumsum(nfails[2,])
  ##ss:组的个数
  ss=ncol(nfails);ns=apply(nfails,2,sum);trains=24;decre=168;incre=336
  ##thres： 可信区间上限
  thres=rep(0,ss)
  ##前trains个观测值作为训练集，把门限值降低
  for(i in 2:trains){
    thres[i]=qbb(alpha,ns[i],shap1[i-1],shap2[i-1])
    alarm=which(nfails[1,2:i]>thres[1:(i-1)])
    nfailsi=nfails[1,2:i]
    if(length(alarm)==0){
      mnf=max(nfailsi)
      thres[i]=max(thres[i],mnf,2)
    } else{
      mnf=max(nfailsi[-alarm])
      thres[i]=max(thres[i],mnf,2)
    }
    
  }
  #
  for(i in (trains+1):ss){
    thres[i]=qbb(alpha,ns[i],shap1[i-1],shap2[i-1])
    alarm=which(nfails[1,2:i]>thres[1:(i-1)])
    nfailsi=nfails[1,2:i]
    retro=ifelse(i>decre,which(nfails[1,(i-decre+1):i]>thres[(i-decre):(i-1)]),0)
    if(length(alarm)==0){
      mnf=max(nfailsi)
      #看最近两周失效数达到最大值的次数
      #numnf=sum(nfailsi[max(i-336,1):i]==mnf)
      numnf=sum(nfailsi[nfailsi[(max(i-incre,1)):(i-1)]==mnf]==thres[i-1])
      if(numnf>1){
        thres[i]=max(thres[i],mnf+1,thres[i-1])
      } else{
        thres[i]=max(thres[i],mnf,thres[i-1])
      }
    } else{
      mnf=max(nfailsi[-alarm])
      #numnf=sum(nfailsi[(max(i-336,1)):i]==mnf)
      numnf=sum(nfailsi[nfailsi[(max(i-incre,1)):(i-1)]==mnf]==thres[i-1])
      if(numnf>1){
        thres[i]=max(thres[i],mnf+1,thres[i-1])
      } else{
        thres[i]=max(thres[i],mnf,thres[i-1])
      }
    }
    if(length(retro)==0 & thres[i-1]-mnf>=2){
      thres[i]=thres[i-1]-1
    }
  }
  alarm=which(nfails[1,-1]>thres[-ss])
  if(length(alarm)>0){
    re=dt[alarm,]
    write.csv(re,paste0(index[id], ".csv"))
  }
  thres[thres>20]=20
  #return(re)
  png(file = paste0(index[id], ".png"))
  plot(2:ss,thres[-1],type='l',col='red',ylim=c(0,max(c(thres[-1],nfails[1,-1]))+5))
  points(2:ss,nfails[1,-1],type = 'h')
  dev.off()
  #print(id)
}

setwd("D:/hw/V2/")

lapply(1:length(index), funx)



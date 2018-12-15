library(VGAM)
library(TailRank)
#dat=read.csv("C:/z/博士生涯/项目/华为项目/徐老师/hw.csv")
#dat=read.csv("F:/快盘/FangCloudSync/温大/华为项目/Test3_0717_CM5T1.csv")
dat=read.csv("C:/z/博士生涯/项目/华为项目/徐老师/0409.csv")
dat=dat[!duplicated(dat$te_ft_result123_emily.serial_number),]
#dat=read.csv("/Users/mac/FangCloudSync/温大/华为项目/Test3_0717_CM5T1.csv")
#导入检测时间
ctime=strptime(dat[,1],"%Y/%m/%d %H:%M")
#时间间隔：fixtime
#dctime：后面与第一次检测的时间间隔；dctime0：以fixtime为区间分组
fixtime=60;nfix=30
dctime=difftime(ctime,ctime[1],units = "mins")
dctime0=floor(dctime/fixtime)
###nlag:组数
lag1=unique(dctime0);nlag=length(lag1)
nfails=matrix(0,2,nlag)
####每组中检测数量不得少于nfix,若小，则合并到前一项
ii=1;k=1
while(ii<=nlag && k<=nlag){
  id=which(dctime0==lag1[k])
  sum1=sum(dat[id,3])
  if(sum1<=1){
    nfails[,ii]=c(sum1,length(id)-sum1)
  }else{
    #数据清洗，同一个产品检测多次自动标记为一次
    iden=which(dat[id,3]==1)
    iden1=dat[id,2]
    nfails[1,ii]=length(unique(iden1[iden]))
    nfails[2,ii]=length(id)-sum1+nfails[1,ii]
  }
  ###
  if(sum(nfails[,ii])<=nfix){
    nfails[,ii-1]=nfails[,ii]+nfails[,ii-1]
  }else{ii=ii+1}
  k=k+1
}
####





####看每组中观测值是否小于10，若小于，则合并到前一项中去；这样组数将减少
nfails=nfails[,1:(ii-1)]
##
#nfails[,1:20]
##可信水平
alpha=0.95

##beta-binomial分布的参数值1:nb-5, 2:nb-4,3:nb-3,4:nb-2
block=5
#nb=ncol(nfails)
#shape1var=0
#for(i in 1:block) shape1var=shape1var+nfails[1,i:(nb-(block-i))]
#shape1var=c(cumsum(nfails[1,1:block]),shape1var)
#shap1=1+shape1var


#shape2var=0
#for(i in 1:block) shape2var=shape2var+nfails[2,i:(nb-(block-i))]
#shape2var=c(cumsum(nfails[2,1:block]),shape2var)
#shap2=1+shape2var

#shap1=1+cumsum(nfails[1,])
#shap2=1+cumsum(nfails[2,])
##ss:组的个数
nnfails=nfails
ss=ii-1;ns=apply(nnfails,2,sum)
##thres： 可信区间上限
thres=thres1=rep(0,ss)
for(i in (block+1):(ss-1)){
  #id=which(pbetabinom.ab(0:ns[i+1],ns[i+1],shap1[i],shap2[i])>alpha)
  shap1=sum(nnfails[1,(i-5):(i-1)])+1
  shap2=sum(nnfails[2,(i-5):(i-1)])+1
  thres[i]=qbb(alpha,ns[i],shap1,shap2)+1
  thres1[i]=floor(mean(thres[i:(i-4)]))
  alarm <- nfails[1,i]>thres[i]
  nnfails[1,i]=ifelse(alarm,median(nnfails[1,(i-5):(i-1)]),nnfails[1,i])
}



plot(thres1[-ss],type="l",col=2,lwd=2,ylab="门限值",ylim=c(0,15))
points(1:(ss-1),nfails[1,-1],type ="l")
legend("topleft","门限值",col=2,lwd=2,lty=1,bty="n")


alarm=which(nfails[1,-1]>thres[-ss])
re=NULL
for(i in 1:length(alarm)){
  alarmtime=dat[sum(ns[1:(alarm[i]+1)]),1]
  #print(data.frame(alarmtime))
  
  re=rbind(re,data.frame(time=alarmtime,
                         num_fail=nfails[1,alarm[i]+1],
                         num_trial=nfails[2,alarm[i]+1]))
}
re
#ctime[dctime0==alarm[1]

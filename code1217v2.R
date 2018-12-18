library(VGAM)
library(RODBC)
library(TailRank)
#library(dplyr)

dbhandle <- odbcDriverConnect('driver={SQL Server}; server=10.22.97.90; uid=sa; pwd=!QAZ2wsx; 
                              database=R123DATA') #TESTdatabase为数据库
dat <- sqlQuery(dbhandle, "SELECT all 
                [start_datetime]
                ,[serial_number]
                ,[r3_result], [cm_name]
                FROM [R123DATA].[dbo].[CharacteristicExtraction_Result01] where  
                案例名称='08_0311手机产品长城开发Figo的CBT工位制程问题'
                order by start_datetime asc")


#12_0118手机产品长城开发Figo项目MT工位物料问题
#13_0425 手机产品华为本部CLT的CAT工位物料问题
#06_0120手机产品珠海伟创力Vicky的BC工位物料问题
#14_0505手机产品华为本部Emily的CBT工位器件不良(没有失效)


#ctime=strptime(dat[,1],"%Y/%m/%d %H:%M")
#ctime=dat[,1]
#因子变量全部转换为字符串
dat$serial_number=as.character(dat$serial_number)
dat$cm_name=as.character(dat$cm_name)

#提取所有r3_result=1的行指标idx
idx=which(dat$r3_result==1)
#提取所有r3_result=1的行组成的数据框datnx
datnx=dat[idx,]
#构造序列号serial_number频数表dxn
dxn=as.data.frame(table(datnx$serial_number),stringsAsFactors = F)
#提取dxn中serial_number频数小于3的行指标
xid=dxn$Var1[(dxn$Freq<3)]
#把serial_number频数小于3的serial_number的r3_result设置为0，只有不小于3的频数才认为是真实坏掉。
datnx$r3_result[datnx$serial_number %in% xid]=0
#把datnx中非重复的serial_number赋值给dat中对应的指标
dat[idx[!duplicated(datnx$serial_number)],]=datnx[!duplicated(datnx$serial_number),]

#dat=dat[!duplicated(datnx$start_datetime),]
dat=dat[!duplicated(dat$serial_number),]
#dat[dat$r3_result==1,]

#查看是否还存在重复序列号
all(!duplicated(dat$serial_number))

#导入检测时间
ctime=dat[,1]


#时间间隔：fixtime
#dctime：后面与第一次检测的时间间隔；dctime0：以fixtime为区间分组
fixtime=60;nfix=50


dctime=difftime(ctime,ctime[1],units = "mins")
dctime0=floor(dctime/fixtime)
###nlag:组数
lag1=unique(dctime0);nlag=length(lag1)
nfails=matrix(0,2,nlag)

#dt=as.data.frame(table(dctime0))
#ntm=cumsum(dt$Freq)
#ctime[ntm]

#plot(diff(ctime[ntm]))

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

#plot(nfails[1,])
####看每组中观测值是否小于10，若小于，则合并到前一项中去；这样组数将减少
nfails=nfails[,1:(ii-1)]

#查看失效数不为0的组
#t(nfails)[which(nfails[1,]>0),]



##可信水平
alpha=0.95

##beta-binomial分布的参数值1:nb-5, 2:nb-4,3:nb-3,4:nb-2

blocfun=function(x){
  block=x
  
  ##ss:组的个数
  nnfails=nfails
  ss=ii-1;ns=apply(nnfails,2,sum)
  ##thres： 可信区间上限
  thres=thres1=rep(0,ss)
  #plot(1,type="n",col=2,lwd=2,ylab="门限值",ylim=c(0,15),xlim=c(0,ss))
  for(i in (block+1):(ss-1)){
    #id=which(pbetabinom.ab(0:ns[i+1],ns[i+1],shap1[i],shap2[i])>alpha)
    shap1=sum(nnfails[1,(i-block):(i-1)])+1
    shap2=sum(nnfails[2,(i-block):(i-1)])+1
    thres[i]=qbb(alpha,ns[i],shap1,shap2)+1
    thres1[i]=floor(mean(thres[(i):(i-1)]))
    alarm <- nfails[1,i]>thres[i]
    nnfails[1,i]=ifelse(alarm,mean(nnfails[1,(i-block):(i-1)]),nnfails[1,i])
    #points(i,thres1[i],col='red')
    #points(i,nfails[1,i])
  }
  
  alarm=which(nfails[1,-1]>thres1[-ss])
  re=NULL
  for(i in 1:length(alarm)){
    alarmtime=dat[sum(ns[1:(alarm[i]+2)]),1]
    re=rbind(re,data.frame(time=alarmtime,
                           num_fail=nfails[1,alarm[i]+1],
                           num_trial=nfails[2,alarm[i]+1],level=1))
  }
  return(re[re$num_fail>2,])
}
b24=blocfun(24)
#b24
b5=blocfun(5)
#b5
b=rbind(b24,b5)
rownames(b)=NULL


b$time=as.character(b$time)
idxtime=as.data.frame(table(b$time), stringsAsFactors = F)
#idxtime$Var1=as.Date(idxtime$Var1)

bn=b[!duplicated(b$time),]
bn$level=ifelse(bn$time %in% idxtime$Var1[idxtime$Freq>1],2,1)
bn[order(bn$time),]



#b%>%group_by(time)%>%mutate(levels=sum(level))

#b=aggregate(level~.,data=b,sum)
#as.Date.default(b$time)
#b[order(b$time),]
#rownames(b)=NULL
#b












R version 3.5.1 (2018-07-02) -- "Feather Spray"
Copyright (C) 2018 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R是自由软件，不带任何担保。
在某些条件下你可以将其自由散布。
用'license()'或'licence()'来看散布的详细条件。

R是个合作计划，有许多人为之做出了贡献.
用'contributors()'来看合作者的详细情况
用'citation()'会告诉你如何在出版物中正确地引用R或R程序包。

用'demo()'来看一些示范程序，用'help()'来阅读在线帮助文件，或
用'help.start()'通过HTML浏览器来看帮助文件。
用'q()'退出R.

> library(VGAM)
载入需要的程辑包：stats4
载入需要的程辑包：splines
> library(RODBC)
> 
> dbhandle <- odbcDriverConnect('driver={SQL Server}; server=10.22.97.90; uid=sa; pwd=!QAZ2wsx;
+                               database=R123DATA')
> setwd("D:/hw/code/Graph")
> while(1){
+   name <- sqlQuery(dbhandle,
+                    'SELECT ALL [id]
+                    ,[案例名称]
+                    ,[stat]
+                    FROM [R123DATA].[dbo].[stat]')
+   index <- which(name$stat == 0)
+   if(length(index) == 0) break
+   
+   mingcheng <- as.character(name[index[1],][1,2])
+   name[index[1],][1,3] <- 1
+   sqlUpdate(channel = dbhandle, dat = name, tablename = "stat",  index = "id")
+   
+   dat <- sqlQuery(dbhandle, paste0("SELECT all [start_datetime], [serial_number], [r3_result], [cm_name] FROM [R123DATA].[dbo].[CharacteristicExtraction_Result01] where 案例名称='", mingcheng, "' order by start_datetime asc"))
+   
+   
+   #12_0118手机产品长城开发Figo项目MT工位物料问题
+   
+   
+   #ctime=strptime(dat[,1],"%Y/%m/%d %H:%M")
+   #ctime=dat[,1]
+   #因子变量全部转换为字符串
+   dat$serial_number=as.character(dat$serial_number)
+   dat$cm_name=as.character(dat$cm_name)
+   
+   #提取所有r3_result=1的行指标idx
+   
+   
+   
+   
+   idx=which(dat$r3_result==1)
+   #提取所有r3_result=1的行组成的数据框datnx
+   datnx=dat[idx,]
+   #构造序列号serial_number频数表dxn
+   dxn=as.data.frame(table(datnx$serial_number),stringsAsFactors = F)
+   #提取dxn中serial_number频数小于3的行指标
+   xid=dxn$Var1[(dxn$Freq<3)]
+   #把serial_number频数小于3的serial_number的r3_result设置为0，只有不小于3的频数才认为是真实坏掉。
+   datnx$r3_result[datnx$serial_number %in% xid]=0
+   #把datnx中非重复的serial_number赋值给dat中对应的指标
+   dat[idx[!duplicated(datnx$serial_number)],]=datnx[!duplicated(datnx$serial_number),]
+   
+   #dat=dat[!duplicated(datnx$start_datetime),]
+   dat=dat[!duplicated(dat$serial_number),]
+   dat[dat$r3_result==1,]
+   
+   #查看是否还存在重复序列号
+   all(!duplicated(dat$serial_number))
+   
+   #导入检测时间
+   ctime=dat[,1]
+   
+   
+   #时间间隔：fixtime
+   #dctime：后面与第一次检测的时间间隔；dctime0：以fixtime为区间分组
+   fixtime=60;nfix=50
+   
+   
+   dctime=difftime(ctime,ctime[1],units = "mins")
+   dctime0=floor(dctime/fixtime)
+   ###nlag:组数
+   lag1=unique(dctime0);nlag=length(lag1)
+   nfails=matrix(0,2,nlag)
+   as.data.frame(table(dctime0))
+   
+   
+   
+   ####每组中检测数量不得少于nfix,若小，则合并到前一项
+   ii=1;k=1
+   while(ii<=nlag && k<=nlag){
+     id=which(dctime0==lag1[k])
+     sum1=sum(dat[id,3])
+     if(sum1<=1){
+       nfails[,ii]=c(sum1,length(id)-sum1)
+     }else{
+       #数据清洗，同一个产品检测多次自动标记为一次
+       iden=which(dat[id,3]==1)
+       iden1=dat[id,2]
+       nfails[1,ii]=length(unique(iden1[iden]))
+       nfails[2,ii]=length(id)-sum1+nfails[1,ii]
+     }
+     ###
+     if(sum(nfails[,ii])<=nfix){
+       nfails[,ii-1]=nfails[,ii]+nfails[,ii-1]
+     }else{ii=ii+1}
+     k=k+1
+   }
+   
+   #plot(nfails[1,])
+   ####看每组中观测值是否小于10，若小于，则合并到前一项中去；这样组数将减少
+   nfails=nfails[,1:(ii-1)]
+   
+   #查看失效数不为0的组
+   t(nfails)[which(nfails[1,]>0),]
+   
+   ##
+   #nfails[,1:20]
+   ##可信水平
+   alpha=0.995
+   
+   ##beta-binomial分布的参数值
+   shap1=1+cumsum(nfails[1,])
+   shap2=1+cumsum(nfails[2,])
+   ##ss:组的个数
+   ss=ii-1;ns=apply(nfails,2,sum)
+   ##thres： 可信区间上限
+   thres=rep(0,ss)
+   for(i in 1:(ss-1)){
+     id=which(pbetabinom.ab(0:ns[i+1],ns[i+1],shap1[i],shap2[i])>alpha)
+     thres[i]=min(id[1]-1)
+   }
+   
+   for(i in 1:(ss-1)){
+     id=which(pbetabinom.ab(0:ns[i+1],ns[i+1],shap1[i],shap2[i])>alpha)
+     thres[i]=min(id[1]-1)
+   }
+   
+   png(file = paste0(mingcheng, ".png"))
+   plot(thres[-ss],type="l",col=2,lwd=2,ylab="门限值",ylim=c(0,15))
+   points(1:(ss-1),nfails[1,-1],type ="l")
+   legend("topleft","门限值",col=2,lwd=2,lty=1,bty="n")
+   dev.off()
+   
+   re=NULL
+   alarm=which(nfails[1,-1]>thres[-ss])
+   for(i in 1:length(alarm)){
+     alarmtime=dat[sum(ns[1:(alarm[i]+1)]),1]
+     #index=dat[sum(ns[1:(alarm[i]+1)]),2]
+     #print(data.frame(alarmtime))
+     re=rbind(re,data.frame("案例名称"=mingcheng, STATER_TIME=alarmtime, num_fail=nfails[1,alarm[i]+1], num_trial=nfails[2,alarm[i]+1]))
+   }
+   re
+   
+   sqlSave(channel=dbhandle, dat=re, 'A', tablename="Rresult", rownames=FALSE, append=TRUE)
+   
+   name[index[1],][1,3] <- 2
+   sqlUpdate(channel = dbhandle, dat = name, tablename = "stat",  index = "id")
+   
+   #re$time[1]< re$time[2]
+   
+   
+   #ctime[dctime0==alarm[1]]
+ }
> 
> odbcClose(dbhandle)
> 
> 
> 
> proc.time()
用户 系统 流逝 
0.84 0.12 0.96 

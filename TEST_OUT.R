
R version 3.5.1 (2018-07-02) -- "Feather Spray"
Copyright (C) 2018 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R�����������������κε�����
��ĳЩ����������Խ�������ɢ����
��'license()'��'licence()'����ɢ������ϸ������

R�Ǹ������ƻ�����������Ϊ֮�����˹���.
��'contributors()'���������ߵ���ϸ���
��'citation()'�����������ڳ���������ȷ������R��R�������

��'demo()'����һЩʾ��������'help()'���Ķ����߰����ļ�����
��'help.start()'ͨ��HTML��������������ļ���
��'q()'�˳�R.

> library(VGAM)
������Ҫ�ĳ̼�����stats4
������Ҫ�ĳ̼�����splines
> library(RODBC)
> 
> dbhandle <- odbcDriverConnect('driver={SQL Server}; server=10.22.97.90; uid=sa; pwd=!QAZ2wsx;
+                               database=R123DATA')
> setwd("D:/hw/code/Graph")
> while(1){
+   name <- sqlQuery(dbhandle,
+                    'SELECT ALL [id]
+                    ,[��������]
+                    ,[stat]
+                    FROM [R123DATA].[dbo].[stat]')
+   index <- which(name$stat == 0)
+   if(length(index) == 0) break
+   
+   mingcheng <- as.character(name[index[1],][1,2])
+   name[index[1],][1,3] <- 1
+   sqlUpdate(channel = dbhandle, dat = name, tablename = "stat",  index = "id")
+   
+   dat <- sqlQuery(dbhandle, paste0("SELECT all [start_datetime], [serial_number], [r3_result], [cm_name] FROM [R123DATA].[dbo].[CharacteristicExtraction_Result01] where ��������='", mingcheng, "' order by start_datetime asc"))
+   
+   
+   #12_0118�ֻ���Ʒ���ǿ���Figo��ĿMT��λ��������
+   
+   
+   #ctime=strptime(dat[,1],"%Y/%m/%d %H:%M")
+   #ctime=dat[,1]
+   #���ӱ���ȫ��ת��Ϊ�ַ���
+   dat$serial_number=as.character(dat$serial_number)
+   dat$cm_name=as.character(dat$cm_name)
+   
+   #��ȡ����r3_result=1����ָ��idx
+   
+   
+   
+   
+   idx=which(dat$r3_result==1)
+   #��ȡ����r3_result=1������ɵ����ݿ�datnx
+   datnx=dat[idx,]
+   #�������к�serial_numberƵ����dxn
+   dxn=as.data.frame(table(datnx$serial_number),stringsAsFactors = F)
+   #��ȡdxn��serial_numberƵ��С��3����ָ��
+   xid=dxn$Var1[(dxn$Freq<3)]
+   #��serial_numberƵ��С��3��serial_number��r3_result����Ϊ0��ֻ�в�С��3��Ƶ������Ϊ����ʵ������
+   datnx$r3_result[datnx$serial_number %in% xid]=0
+   #��datnx�з��ظ���serial_number��ֵ��dat�ж�Ӧ��ָ��
+   dat[idx[!duplicated(datnx$serial_number)],]=datnx[!duplicated(datnx$serial_number),]
+   
+   #dat=dat[!duplicated(datnx$start_datetime),]
+   dat=dat[!duplicated(dat$serial_number),]
+   dat[dat$r3_result==1,]
+   
+   #�鿴�Ƿ񻹴����ظ����к�
+   all(!duplicated(dat$serial_number))
+   
+   #������ʱ��
+   ctime=dat[,1]
+   
+   
+   #ʱ������fixtime
+   #dctime���������һ�μ���ʱ������dctime0����fixtimeΪ�������
+   fixtime=60;nfix=50
+   
+   
+   dctime=difftime(ctime,ctime[1],units = "mins")
+   dctime0=floor(dctime/fixtime)
+   ###nlag:����
+   lag1=unique(dctime0);nlag=length(lag1)
+   nfails=matrix(0,2,nlag)
+   as.data.frame(table(dctime0))
+   
+   
+   
+   ####ÿ���м��������������nfix,��С����ϲ���ǰһ��
+   ii=1;k=1
+   while(ii<=nlag && k<=nlag){
+     id=which(dctime0==lag1[k])
+     sum1=sum(dat[id,3])
+     if(sum1<=1){
+       nfails[,ii]=c(sum1,length(id)-sum1)
+     }else{
+       #������ϴ��ͬһ����Ʒ������Զ����Ϊһ��
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
+   ####��ÿ���й۲�ֵ�Ƿ�С��10����С�ڣ���ϲ���ǰһ����ȥ����������������
+   nfails=nfails[,1:(ii-1)]
+   
+   #�鿴ʧЧ����Ϊ0����
+   t(nfails)[which(nfails[1,]>0),]
+   
+   ##
+   #nfails[,1:20]
+   ##����ˮƽ
+   alpha=0.995
+   
+   ##beta-binomial�ֲ��Ĳ���ֵ
+   shap1=1+cumsum(nfails[1,])
+   shap2=1+cumsum(nfails[2,])
+   ##ss:��ĸ���
+   ss=ii-1;ns=apply(nfails,2,sum)
+   ##thres�� ������������
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
+   plot(thres[-ss],type="l",col=2,lwd=2,ylab="����ֵ",ylim=c(0,15))
+   points(1:(ss-1),nfails[1,-1],type ="l")
+   legend("topleft","����ֵ",col=2,lwd=2,lty=1,bty="n")
+   dev.off()
+   
+   re=NULL
+   alarm=which(nfails[1,-1]>thres[-ss])
+   for(i in 1:length(alarm)){
+     alarmtime=dat[sum(ns[1:(alarm[i]+1)]),1]
+     #index=dat[sum(ns[1:(alarm[i]+1)]),2]
+     #print(data.frame(alarmtime))
+     re=rbind(re,data.frame("��������"=mingcheng, STATER_TIME=alarmtime, num_fail=nfails[1,alarm[i]+1], num_trial=nfails[2,alarm[i]+1]))
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
�û� ϵͳ ���� 
0.84 0.12 0.96 
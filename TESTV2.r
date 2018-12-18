library(VGAM)
library(RODBC)
library(TailRank)

dbhandle <- odbcDriverConnect('driver={SQL Server}; server=10.22.97.90; uid=sa; pwd=!QAZ2wsx;
                              database=R123DATA')
setwd("D:/hw/code/Graph")

while(1){
  name <- sqlQuery(dbhandle,
  'SELECT ALL [id]
      ,[��������]
      ,[stat]
      ,[����ʱ��]
      ,[��λ]
      ,[FACTORY_NAME]
      ,[�Ƿ�����]
      ,[��Чʱ��]
    FROM [R123DATA].[dbo].[stat1]')
  index <- which(name$stat == 0)
  if(length(index) == 0) break
  
  mingcheng <- as.character(name[index[1],][1,2])
  mingcheng <- chartr("/:*?<>|", "_______", mingcheng)
  name[index[1],][1,3] <- 1
  sqlUpdate(channel = dbhandle, dat = name, tablename = "stat1",  index = "id")
  
  dat <- sqlQuery(dbhandle, paste0("SELECT all [start_datetime], [serial_number], [r3_result], [cm_name] FROM [R123DATA].[dbo].[CharacteristicExtraction_Result01] where ��������='", mingcheng, "' order by start_datetime asc"))
  
  #12_0118�ֻ���Ʒ���ǿ���Figo��ĿMT��λ��������

  #���ӱ���ȫ��ת��Ϊ�ַ���
  dat$serial_number=as.character(dat$serial_number)
  dat$cm_name=as.character(dat$cm_name)

  #��ȡ����r3_result=1����ָ��idx
  idx=which(dat$r3_result==1)
  #��ȡ����r3_result=1������ɵ����ݿ�datnx
  datnx=dat[idx,]
  #�������к�serial_numberƵ����dxn
  dxn=as.data.frame(table(datnx$serial_number),stringsAsFactors = F)
  #��ȡdxn��serial_numberƵ��С��3����ָ��
  xid=dxn$Var1[(dxn$Freq<3)]
  #��serial_numberƵ��С��3��serial_number��r3_result����Ϊ0��ֻ�в�С��3��Ƶ������Ϊ����ʵ������
  datnx$r3_result[datnx$serial_number %in% xid]=0
  #��datnx�з��ظ���serial_number��ֵ��dat�ж�Ӧ��ָ��
  dat[idx[!duplicated(datnx$serial_number)],]=datnx[!duplicated(datnx$serial_number),]
  
  #dat=dat[!duplicated(datnx$start_datetime),]
  dat=dat[!duplicated(dat$serial_number),]
  #dat[dat$r3_result==1,]
  
  #�鿴�Ƿ񻹴����ظ����к�
  all(!duplicated(dat$serial_number))
  
  #������ʱ��
  ctime=dat[,1]
  
  
  #ʱ������fixtime
  #dctime���������һ�μ���ʱ������dctime0����fixtimeΪ�������
  fixtime=60;nfix=50
  
  
  dctime=difftime(ctime,ctime[1],units = "mins")
  dctime0=floor(dctime/fixtime)
  ###nlag:����
  lag1=unique(dctime0);nlag=length(lag1)
  nfails=matrix(0,2,nlag)

  ##ÿ���м��������������nfix,��С����ϲ���ǰһ��
  ii=1;k=1
  while(ii<=nlag && k<=nlag){
    id=which(dctime0==lag1[k])
    sum1=sum(dat[id,3])
    if(sum1<=1){
      nfails[,ii]=c(sum1,length(id)-sum1)
    }else{
      #������ϴ��ͬһ����Ʒ������Զ����Ϊһ��
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
  ####��ÿ���й۲�ֵ�Ƿ�С��10����С�ڣ���ϲ���ǰһ����ȥ����������������
  nfails=nfails[,1:(ii-1)]
  
  #�鿴ʧЧ����Ϊ0����
  #t(nfails)[which(nfails[1,]>0),]
  
  
  ##����ˮƽ
  alpha=0.95
  
  
  
  blocfun=function(x){
    block=x
    ##ss:��ĸ���
    nnfails=nfails
    ss=ii-1;ns=apply(nnfails,2,sum)
    ##thres�� ������������
    thres=thres1=rep(0,ss)
    #plot(1,type="n",col=2,lwd=2,ylab="����ֵ",ylim=c(0,15),xlim=c(0,ss))
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
    if(x==24){
    png(file = paste0(mingcheng, ".png"))
    plot(1,type="n",col=2,lwd=2,ylab="����ֵ",ylim=c(0,max(nfails[1,])+10),xlim=c(0,ss))
    lines(1:(ss-block),thres1[(block+1):ss],col='red')
    lines(1:ss,nfails[1,])
    dev.off()
    }else{
    NULL
    }
    alarm=which(nfails[1,-1]>thres1[-ss])
    if(length(alarm)!=0){
      re=NULL
      for(i in 1:length(alarm)){
        alarmtime=dat[sum(ns[1:(alarm[i]+2)]),1]
        re=rbind(re,data.frame(time=alarmtime,
                               num_fail=nfails[1,alarm[i]+1],
                               num_trial=nfails[2,alarm[i]+1],level=1))
      }
      return(re)
      #return(re[re$num_fail>2,])
    }else{
      return(NULL)
    }
  }
  
  if(any(nfails[1,]!=0)){
    if(ncol(nfails)>24){
      b24=blocfun(24)
      if(is.null(b24) | sum(b24$num_fail<=2)==length(b24$num_fail)){
        b24=NULL
      }else {
        b24=b24[b24$num_fail>2,]
      }
    }else{
      b24=NULL
    }
      b5=blocfun(5)
      if(is.null(b5) | sum(b5$num_fail<=2)==length(b5$num_fail)){
        b5=NULL
      }else {
        b5=b5[b5$num_fail>2,]
      }
      #b5
      b=rbind(b24,b5)
      rownames(b)=NULL
  }else{
      b=NULL
    }
    
    
  
  if(is.null(b)){
    print("���쳣")
  } else{
  b$time=as.character(b$time)
  idxtime=as.data.frame(table(b$time), stringsAsFactors = F)
  #idxtime$Var1=as.Date(idxtime$Var1)
  
  bn=b[!duplicated(b$time),]
  bn$level=ifelse(bn$time %in% idxtime$Var1[idxtime$Freq>1],2,1)
  re=bn[order(bn$time),]
  re
  
  result <- NULL
  subresult <- name[index[1],]
  for(i in 1:length(re[,1])){
    if(is.na(subresult[1,4])){
      tql <- NA
    } else{
      tql <- round(as.numeric(difftime(as.POSIXlt(subresult[1,4]), as.POSIXlt(re[i,1]), units="hours")), 4)
    }
    subre <- data.frame(STATER_TIME=re[i,1], "��ǰʱ��"=tql, num_fail=re[i,2], num_trial=re[i,3])
    result <- rbind(result, cbind(subresult, subre))
  }
  result[,3] <- 2
  
  sqlSave(channel=dbhandle, dat=result, tablename="finalresult", rownames=FALSE, append=TRUE)
  
  name[index[1],][1,3] <- 2
  sqlUpdate(channel = dbhandle, dat = name, tablename = "stat1",  index = "id")
  }
}

odbcClose(dbhandle)









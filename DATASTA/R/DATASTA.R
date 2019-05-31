# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

##制作获取数据集各项基础统计指标的函数
##区分离散型变量和连续型变量
get_statistics_info<-function(data){
  library(moments)
  id_char=which(sapply(data,is.character))
  id_numeric=which(sapply(data,is.numeric))

  data_char=data[,id_char]
  data_numeric=data[,id_numeric]

  getmode <- function(v) {
    ##自定义一个取众数的一个函数
    uniqv <- unique(na.omit(v))
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  mystats_numeric=function(x){
    ##自定义数据描述统计函数  针对连续性变量开发
    n<-length(x)  #样本总数
    nn<-sum(is.na(x))  #空值样本数
    nm<-n-nn  #非空样本数
    nper<-round(mean(is.na(x))*100,2)  #空值缺失率
    num_unique=length(unique(x[!is.na(x)]))  #非空去重后数值个数
    mean=round(mean(x,na.rm=T),4)  #均值
    median=round(median(x,na.rm=T),4)  #中位数
    mode=getmode(x)  #众数  自定义函数  默认只取一个，取首先出现的
    sd=round(sd(x,na.rm=T),4)  #标准差
    var=round(var(x,na.rm=T),4)  #方差
    se=round(sd/sqrt(nm),4)  #均值标准误
    skewness=round(skewness(x,na.rm=T),4)  #偏度系数
    kurtosis=round(kurtosis(x,na.rm=T),4)  #峰度系数  正态分布的峰度为3
    min=min(x,na.rm=T)  #最小值
    max=max(x,na.rm=T)  #最大值
    range=max-min  #极差
    cv=round(sd/mean,4)  #变异系数
    up_limit=mean+3*sd  #异常上限 服从正态分布的假定
    qq_bin=as.numeric(quantile(x,prob=c(0.01,0.05,0.1,0.25,0.5
                                        ,0.75,0.9,0.95,0.99),na.rm=T))  #各百分位数
    q99<-as.numeric(quantile(x,0.99,na.rm=T))
    t1=round((max-q99)/median*100,2)  #(最大值-q_0.99)/中位数
    t2=round((max-q99)/max*100,2)  #(最大值-q_0.99)/最大值


    return(c(n,nn,nm,nper,num_unique,mean,median,mode,sd,var,se,
             skewness,kurtosis,min,max,range,cv,up_limit,qq_bin,t1,t2))

  }

  mystats_char=function(x){
    ##自定义数据描述统计函数  针对离散型变量开发
    n<-length(x)  #样本总数
    nn<-sum(is.na(x))  #空值样本数
    nm<-n-nn  #非空样本数
    nper<-round(mean(is.na(x))*100,2)  #空值缺失率
    num_unique=length(unique(x[!is.na(x)]))  #非空去重后数值个数
    mean=NA  #均值
    median=NA  #中位数
    mode=getmode(x)  #众数  自定义函数  默认只取一个，取首先出现的
    sd=NA  #标准差
    var=NA  #方差
    se=NA  #均值标准误
    skewness=NA  #偏度系数
    kurtosis=NA  #峰度系数  正态分布的峰度为3
    min=NA  #最小值
    max=NA  #最大值
    range=NA  #极差
    cv=NA  #变异系数
    up_limit=NA  #异常上限 服从正态分布的假定
    qq_bin=rep(NA,9)  #各百分位数
    t1=NA  #(最大值-q_0.99)/中位数
    t2=NA  #(最大值-q_0.99)/最大值

    return(c(n,nn,nm,nper,num_unique,mean,median,mode,sd,var,se,
             skewness,kurtosis,min,max,range,cv,up_limit,qq_bin,t1,t2))

  }

  if(ncol(data_char)>0 & ncol(data_numeric)>0){
    sta_results<-rbind(data.frame('顺序号'=id_numeric,'字段'=names(id_numeric),'数据类型'='连续型',
                                  t(apply(data_numeric,2,mystats_numeric))),
                       data.frame('顺序号'=id_char,'字段'=names(id_char),'数据类型'='离散型',
                                  t(apply(data_char,2,mystats_char))))
  }
  else if(ncol(data_char)==0 & ncol(data_numeric)>0){
    sta_results<-data.frame('顺序号'=id_numeric,'字段'=names(id_numeric),'数据类型'='连续型',
                            t(apply(data_numeric,2,mystats_numeric)))
  }
  else if(ncol(data_char)>0 & ncol(data_numeric)==0){
    sta_results<-data.frame('顺序号'=id_char,'字段'=names(id_char),'数据类型'='离散型',
               t(apply(data_char,2,mystats_char)))
  }
  else {print("error")}

  sta_results=sta_results[order(sta_results$顺序号),]
  names(sta_results)[4:32]<-c('样本总数','空值样本数','非空样本数','空值缺失率','非空去重后值个数',
                              '均值','中位数','众数','标准差','方差',
                              '均值标准误','偏度系数','峰度系数','最小值','最大值',
                              '极差','变异系数','异常上限','分位数1per','分位数5per',
                              '分位数10per','分位数25per','分位数50per','分位数75per','分位数90per',
                              '分位数95per','分位数99per','异常比例1','异常比例2')
  return(sta_results)
}


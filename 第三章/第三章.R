"
    ____            __                    __  ___    _             _                                       _    __     __             ____ 
   / __ \  ____ _  / /_  ____ _          /  |/  /   (_)   ____    (_)   ____    ____ _        _      __   (_)  / /_   / /_           / __ \
  / / / / / __ `/ / __/ / __ `/         / /|_/ /   / /   / __ \  / /   / __ \  / __ `/       | | /| / /  / /  / __/  / __ \         / /_/ /
 / /_/ / / /_/ / / /_  / /_/ /         / /  / /   / /   / / / / / /   / / / / / /_/ /        | |/ |/ /  / /  / /_   / / / /        / _, _/ 
/_____/  \__,_/  \__/  \__,_/         /_/  /_/   /_/   /_/ /_/ /_/   /_/ /_/  \__, /         |__/|__/  /_/   \__/  /_/ /_/        /_/ |_|  
                                                                             /____/                                                        


" 
#GitHub:https://github.com/binyoucai/Data_Ming_with_R

#课程名称:数据挖掘与R语言
#章节与标题:第3章预测股票市场收益
#代码整理人：挖掘机大王子
#未注释说明：可结合课本理解
#最后更新时间：2018 04 10




###################################################
### 3.2 可用的数据
### 课本页码： 67
###################################################
library(DMwR) #加载本书包
data(GSPC)  #加载GSPC数据框，包含标准普尔500指数



###################################################
### 3.2.1 在R中处理与时间相关的数据
### 课本页码：68
###################################################
library(zoo) #加载zoo包
library(xts) #加载xts包
#单元时间序列
x1 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by="day")) #创建时间相关的数据，xts函数第一个参数接收时间序列数据，第二个参数是时间标签。rnorm用于生成100正态分布数 seq用于生成100个时间序列(按天算)
x1[1:5]
x2 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len=100,by="min"))#创建时间相关的数据，xts函数第一个参数接收时间序列数据，第二个参数是时间标签。rnorm用于生成100正态分布数 seq用于生成100个时间序列(按小时分钟算)
x2[1:4]
x3 <- xts(rnorm(3),as.Date(c('2005-01-01','2005-01-10','2005-01-12')))  #as.Date生成时间的序列
x3

#以下读取特定的时间序列列子
x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["20000105"]
x1["2000-04"]
x1["2000-03-27/"]
x1["2000-02-26/2000-03-03"]
x1["/20000103"]

#处理多元时间序列例子
mts.vals <- matrix(round(rnorm(25),2),5,5) #新建一个5*5的矩阵
colnames(mts.vals) <- paste('ts',1:5,sep='') #给新建的矩阵的列命名
mts <- xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04',
                    '2003-01-05','2003-01-06','2003-02-16')))
mts
mts["2003-01",c("ts2","ts5")]#读取2013-01时间段ts2和ts5列
index(mts) #获取任意xts对象的时间标签和time(mts)效果一样
coredata(mts) #获取时间序列的观测值


###################################################
### 从 CSV 文件读取数据
### 课本页码：71
###################################################
GSPC <- as.xts(read.zoo('sp500.csv',header=T))
#假如CSV数据文件的第一列卫时间标签，则zoo里的read.zoo()函数可以读取该CSV数据文件并把数据转换为zoo对象，函数as.xts()把读取的结果对象转换为xts对象


###################################################
### 从网站上获取数据
### 课本页码：72
###################################################
library(tseries)#加载tseries包
GSPC <- as.xts(get.hist.quote("^GSPC",start="1970-01-02",
          quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))  #用get.hist.quote函数下载数据G，此处下载了60132条数据

head(GSPC) #显示前6条

GSPC <- as.xts(get.hist.quote("^GSPC",
          start="1970-01-02",end='2009-09-15',
          quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))  #为了和课本一样，指定下载范围。


#另一种包下载数据方法
library(quantmod)
getSymbols('^GSPC') #获取数据，默认下载全部



getSymbols('^GSPC',from='1970-01-01',to='2009-09-15') #指定范围下载
colnames(GSPC) <- c("Open", "High", "Low", "Close","Volume","AdjClose") #重命名下载的列名

#指定站点下载数据。
setSymbolLookup(IBM=list(name='IBM',src='yahoo'),
                USDEUR=list(name='USD/EUR',src='oanda',
                            from=as.Date('2009-01-01')))#IBM数指定从yahoo站点下载，USDEUR指定从oanda站点下载
getSymbols(c('IBM','USDEUR'))#开始获取数据
head(IBM) #显示前6行
head(USDEUR) #显示前6行



###################################################
### 从MySQL数据库读取数据
### 课本页码：73
###################################################
#以下是在windows系统下的
library(RODBC)
ch <- odbcConnect("QuotesDSN",uid="myusername",pwd="mypassword")#登陆 输入参数，数据库名，用户名，密码
allQuotes <- sqlFetch(ch,"gspc")
GSPC <- xts(allQuotes[,-1],order.by=as.Date(allQuotes[,1]))
head(GSPC)
odbcClose(ch)


#在Linux和mac系统上加载数据
library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL") #加载mysql数据库驱动
ch <- dbConnect(drv,dbname="Quotes","myusername","mypassword")#登陆 输入参数，数据库名，用户名，密码
allQuotes <- dbGetQuery(ch,"select * from gspc")#从数据库查询数据并存储
GSPC <- xts(allQuotes[,-1],order.by=as.Date(allQuotes[,1])) #将存储的数据转换为xts对象。
head(GSPC)
dbDisconnect(ch) #注销用户
dbUnloadDriver(drv)#退出mysql

#另一种，从数据库读取数据的方法
setSymbolLookup(GSPC=list(name='gspc',src='mysql',
         db.fields=c('Index','Open','High','Low','Close','Volume','AdjClose'),
         user='xpto',password='ypto',dbname='Quotes'))#指定读取的信息。mysql登陆信息
getSymbols('GSPC')#开始读取

#定义预测任务               
###################################################
### 预测什么
### 课本页码：76
###################################################

#p76页方法实现函数
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)

  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))

  for(x in 1:n.days) r[,x] <- Next(Delt(Cl(quotes),v,k=x),x)

  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

#可视化
library(quantmod)
candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()


#p79方法函数的实现
myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]

#训练随机森林模型
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
       myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
       myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
       CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
       myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
       mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
             training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
             ntree=50, importance=T)

#建模
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM),k=1:3))
data <- modelData(ex.model,data.window=c('2009-01-01','2009-08-10'))



#检查变量的重要性
varImpPlot(rf@fitted.model,type=1)

#设定界限值10
imp <- importance(rf@fitted.model,type=1)
rownames(imp)[which(imp > 10)]

#建立数据集
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1) + myATR(GSPC) + myADX(GSPC) +    myEMV(GSPC) + myVolat(GSPC)  + myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)) )






###################################################
### 预测任务
### 课本页码：82
###################################################

#模型数据结构
Tdata.train <- as.data.frame(modelData(data.model,
                       data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                       data.window=c('2000-01-01','2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')


###################################################
### 建模工具
### 课本页码：87
###################################################
#神经网络
set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn,norm.data[1001:2000,])
preds <- unscale(norm.preds,norm.data)

#评估神经网络
sigs.nn <- trading.signals(preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.nn,true.sigs)

#使用nnet函数
set.seed(1234)
library(nnet)
signals <- trading.signals(Tdata.train[,'T.ind.GSPC'],0.1,-0.1)
norm.data <- data.frame(signals=signals,scale(Tdata.train[,-1]))
nn <- nnet(signals ~ .,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
preds <- predict(nn,norm.data[1001:2000,],type='class')

#精准预测模型
sigs.PR(preds,norm.data[1001:2000,1])

#回归分析
library(e1071)
sv <- svm(Tform,Tdata.train[1:1000,],gamma=0.001,cost=100)
s.preds <- predict(sv,Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.svm,true.sigs)

#考虑分类任务
library(kernlab)
data <- cbind(signals=signals,Tdata.train[,-1])
ksv <- ksvm(signals ~ .,data[1:1000,],C=10)
ks.preds <- predict(ksv,data[1001:2000,])
sigs.PR(ks.preds,data[1001:2000,1])

#应用earth()回归代码
library(earth)
e <- earth(Tform,Tdata.train[1:1000,])
e.preds <- predict(e,Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.e,true.sigs)


#从预测到实践
###################################################
### 如何应用预测模型
### 课本页码：91
###################################################
#定义第一个交易策略
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
                     )
  {
    d <- NROW(market) 
    orders <- NULL
    nOs <- NROW(opened.pos)

    if (!nOs && signals[d] == 'h') return(orders)


    if (signals[d] == 'b' && !nOs) {
      quant <- round(bet*money/market[d,'Close'],0)
      if (quant > 0) 
        orders <- rbind(orders,
              data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1+exp.prof),
                                 market[d,'Close']*(1-max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )

 
    } else if (signals[d] == 's' && !nOs) {

      need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                 "N.stocks"])*market[d,'Close']
      quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
      if (quant > 0)
        orders <- rbind(orders,
              data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1-exp.prof),
                                 market[d,'Close']*(1+max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )
    }
    

    if (nOs) 
      for(i in 1:nOs) {
        if (d - opened.pos[i,'Odate'] >= hold.time)
          orders <- rbind(orders,
                data.frame(order=-opened.pos[i,'pos.type'],
                           order.type=1,
                           val = NA,
                           action = 'close',
                           posID = rownames(opened.pos)[i]
                          )
                         )
      }

    orders
  }


#定义第二个交易策略
policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
                    )
  {
    d <- NROW(market) 
    orders <- NULL
    nOs <- NROW(opened.pos)

    if (!nOs && signals[d] == 'h') return(orders)

    if (signals[d] == 'b') {
      quant <- round(bet*money/market[d,'Close'],0)
      if (quant > 0) 
        orders <- rbind(orders,
              data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1+exp.prof),
                                 market[d,'Close']*(1-max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )


    } else if (signals[d] == 's') {

      need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                 "N.stocks"])*market[d,'Close']
      quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
      if (quant > 0)
        orders <- rbind(orders,
              data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1-exp.prof),
                                 market[d,'Close']*(1+max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )
    }

    orders
  }



# 向量支持机来获得交易结果
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)


data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep='')][1:len.ts]


library(e1071)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)

#检查返回对象
t1 <- trading.simulator(market,sig,
             'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))



t1
summary(t1)


tradingEvaluation(t1) #交易效果的经济指标


plot(t1,market,theme='white',name='SP500')


#测试第二个交易策略
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)

#改变训练和测试时间
start <- 2000
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)



#模型选择和评价
###################################################
### 蒙特卡罗估计
### 课本页码：99
###################################################
#训练+测试+评估过程的实现
MC.svmR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  t <- svm(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.svmC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- svm(form,train,...)
  p <- predict(t,test)
  factor(p,levels=c('s','h','b'))
}
MC.nnetR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  t <- nnet(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.nnetC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- nnet(form,train,...)
  p <- predict(t,test,type='class')
  factor(p,levels=c('s','h','b'))
}
MC.earth <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(earth)
  t <- earth(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
singleModel <- function(form,train,test,learner,policy.func,...) {
  p <- do.call(paste('MC',learner,sep='.'),list(form,train,test,...))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
slide <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- slidingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
grow <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- growingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}

#利用eval.stats函数获得估计模型的评价指标统计量
eval.stats <- function(form,train,test,preds,b.t=0.1,s.t=-0.1,...) {

  tgtName <- all.vars(form)[1]
  test[,tgtName] <- trading.signals(test[,tgtName],b.t,s.t)
  st <- sigs.PR(preds,test[,tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),
                     c('s','b','sb'),sep='.')
  

  date <- rownames(test)[1]
  market <- GSPC[paste(date,"/",sep='')][1:length(preds),]
  trade.res <- trading.simulator(market,preds,...)

  c(st,tradingEvaluation(trade.res))
}

#三个衍生策略的实现
pol1 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.025,max.loss=0.05,hold.time=10)

pol2 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.05,max.loss=0.05,hold.time=20)

pol3 <- function(signals,market,op,money)
  policy.2(signals,market,op,money,
           bet=0.5,exp.prof=0.05,max.loss=0.05)


#开始运行蒙特卡罗实验
TODO <- c('svmR','svmC','earth','nnetR','nnetC')

# The data sets used in the comparison
DSs <- list(dataset(Tform,Tdata.train,'SP500'))

# Monte Carlo (MC) settings used
MCsetts <- mcSettings(20,     # 20 repetitions of the MC exps
                      2540,   # ~ 10 years for training
                      1270,   # ~ 5 years for testing
                      1234)   # random number generator seed

# Variants to try for all learners
VARS <- list()
VARS$svmR   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$svmC   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetR  <- list(linout=T,maxit=750,size=c(5,10),
                    decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))
VARS$nnetC  <- list(maxit=750,size=c(5,10),decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))

# main loop
for(td in TODO) {
  assign(td,
         experimentalComparison(
           DSs,         
           c(
             do.call('variants',
                     c(list('singleModel',learner=td),VARS[[td]],
                       varsRootName=paste('single',td,sep='.'))),
             do.call('variants',
                     c(list('slide',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('slide',td,sep='.'))),
             do.call('variants',
                     c(list('grow',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('grow',td,sep='.')))
             ),
            MCsetts)
         )

  # save the results
  save(list=td,file=paste(td,'Rdata',sep='.'))
}

#结束蒙特卡罗实验

#载入蒙特卡罗实验得到的数据文件
load('svmR.Rdata')
load('svmC.Rdata')
load('earth.Rdata')
load('nnetR.Rdata')
load('nnetC.Rdata')


#对应的模型和评价指标
tgtStats <- c('prec.sb','Ret','PercProf',
              'MaxDD','SharpeRatio')
allSysRes <- join(subset(svmR,stats=tgtStats),
                  subset(svmC,stats=tgtStats),
                  subset(nnetR,stats=tgtStats),
                  subset(nnetC,stats=tgtStats),
                  subset(earth,stats=tgtStats),
                  by = 'variants')
rankSystems(allSysRes,5,maxs=c(T,T,T,F,T))


summary(subset(svmC,
               stats=c('Ret','RetOverBH','PercProf','NTrades'),
               vars=c('slide.svmC.v5','slide.svmC.v6')))

#检查约束条件是否满足
fullResults <- join(svmR,svmC,earth,nnetC,nnetR,by='variants')
nt <- statScores(fullResults,'NTrades')[[1]]
rt <- statScores(fullResults,'Ret')[[1]]
pp <- statScores(fullResults,'PercProf')[[1]]
s1 <- names(nt)[which(nt > 20)]
s2 <- names(rt)[which(rt > 0.5)]
s3 <- names(pp)[which(pp > 40)]
namesBest <- intersect(intersect(s1,s2),s3)

#对结果进行显著性统计分析
compAnalysis(subset(fullResults,
                    stats=tgtStats,
                    vars=namesBest))

#可视化20次分布情况
plot(subset(fullResults,
            stats=c('Ret','PercProf','MaxDD'),
            vars=namesBest))

#检查特定交易系统的设置情况
getVariant('single.nnetR.v12',nnetR)






#交易系统
###################################################
### 评估最终测试数据
### 课本页码：110
###################################################
#开始评估
data <- tail(Tdata.train,2540)
results <- list()
for(name in namesBest) {
  sys <- getVariant(name,fullResults)
  results[[name]] <- runLearner(sys,Tform,data,Tdata.eval)
}
results <- t(as.data.frame(results))



#检车性能指标
results[,c('Ret','RetOverBH','MaxDD','SharpeRatio','NTrades','PercProf')]

#最优模型
getVariant('grow.nnetR.v12',fullResults)

#评估交易记录
model <- learner('MC.nnetR',list(maxit=750,linout=T,trace=F,size=10,decay=0.001))
preds <- growingWindowTest(model,Tform,data,Tdata.eval,relearn.step=120)
signals <- factor(preds,levels=1:3,labels=c('s','h','b'))
date <- rownames(Tdata.eval)[1]
market <- GSPC[paste(date,"/",sep='')][1:length(signals),]
trade.res <- trading.simulator(market,signals,policy.func='pol2')

#绘制最优交易系统的交易记录
plot(trade.res,market,theme='white',name='SP500 - final test')

#策略回报
library(PerformanceAnalytics)
rets <- Return.calculate(trade.res@trading$Equity)

#累计收益率
chart.CumReturns(rets,main='Cumulative returns of the strategy',ylab='returns')

#计算年收益
yearlyReturn(trade.res@trading$Equity)

#绘制年收益信息图形
plot(100*yearlyReturn(trade.res@trading$Equity),
     main='Yearly percentage returns of the trading system')
abline(h=0,lty=2)

#月收益百分比
table.CalendarReturns(rets)

#交易策略风险分析
table.DownsideRisk(rets)



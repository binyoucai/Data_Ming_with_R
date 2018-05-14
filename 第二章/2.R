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
#章节与标题:第2章预测海藻数量
#代码整理人：挖掘机大王子
#未注释说明：可结合课本理解
#最后更新时间：2018 03 21



###################################################
### 2.3数据加载到R
###课本页码：29
###################################################
#方式1：通过R包
library(DMwR)
head(algae)

#方式2:本地文件加载
algae <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))


###################################################
### 2.4数据可视化和摘要
###课本页码：30
###################################################
summary(algae)#统计特征概览



#绘制变量mxPH的直方图、平滑直方图、Q-Q图
library(car)#加载car包
par(mfrow=c(1,2))#设置R图形输出，设置为1行2列
hist(algae$mxPH, prob=T, xlab='',
      main='Histogram of maximum pH value',ylim=0:1)#绘制mxPH的直方图
lines(density(algae$mxPH,na.rm=T))#绘制平滑直方图
rug(jitter(algae$mxPH))#在X轴附近绘制变量的实际值，便于识别离群点
qq.plot(algae$mxPH,main='Normal QQ plot of maximum pH')#绘制Q-Q图
par(mfrow=c(1,1))#设置R图形输出，设置为1行1列



#绘制变量oPO4的箱图
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')#绘制箱图
rug(jitter(algae$oPO4),side=2)#在y轴附近绘制变量的实际值
abline(h=mean(algae$oPO4,na.rm=T),lty=2)#绘制水平线



#绘制NH4图
plot(algae$NH4,xlab='')#绘制所有样本点图
#调用abline绘制三条直线
abline(h=mean(algae$NH4,na.rm=T),lty=1)#均值线
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)#均值+标准差线
abline(h=median(algae$NH4,na.rm=T),lty=3)#中位数线
identify(algae$NH4)#鼠标选点

#检查algae数据框中相对于图形中的离群值的观测记录
plot(algae$NH4,xlab='')#绘制所有样本点图
clicked.lines <- identify(algae$NH4)#鼠标点击获取点击样本的信息
algae[clicked.lines,]#返回鼠标点击的结果


#数据框检索离群值
algae[algae$NH4>19000,]#方式1
algae[!is.na(algae$NH4)&algae$NH4>19000,]#方式2 ，更精确范围，把NA去掉。


#绘制lattice箱图
library(lattice)#加载lattice画图包
print(bwplot(size ~ a1, data=algae,ylab='River Size',xlab='Algal A1'))#画箱图

#绘制分位箱图
library(Hmisc)#加载Hmisc包
print(bwplot(size ~ a1, data=algae,panel=panel.bpplot, 
        probs=seq(.01,.49,by=.01), datadensity=TRUE,
        ylab='River Size',xlab='Algal A1'))

#变量a3的条件绘图
minO2 <- equal.count(na.omit(algae$mnO2),
                     number=4,overlap=1/5)
stripplot(season ~ a3|minO2,
          data=algae[!is.na(algae$mnO2),])


###################################################
### 数据缺失
###课本页码：37
###################################################
#将数据重新读取
library(DMwR)
data(algae)

#将数据部分剔除
algae[!complete.cases(algae),]#返回含有缺失值的水样记录


nrow(algae[!complete.cases(algae),])#返回含有缺失值的水样记录的行数


algae <- na.omit(algae)#将数据框中的缺失样本去除

#将数据重新读取
library(DMwR)
data(algae)

algae <- algae[-c(62,199),]#将部分数据剔除


apply(algae,1,function(x) sum(is.na(x)))#统计每一行的缺失数据个数的和

#用包自带的函数找缺失值
data(algae)
manyNAs(algae,0.2)#找出缺失值的个数大于20%的行。


algae <- algae[-manyNAs(algae),]#剔除缺失超过20%的行。默认参数为0.2


#用最高频率值来填补缺失值。
algae[48,'mxPH'] <- mean(algae$mxPH,na.rm=T)#将目标的缺失值用平均值来代替


algae[is.na(algae$Chla),'Chla'] <- median(algae$Chla,na.rm=T)#将缺失值用中位数来代替

#使用一些中心趋势值来代替缺失值
data(algae)
algae <- algae[-manyNAs(algae),]
algae <- centralImputation(algae)#包提供的函数。


#通过变量的相关关系来填补缺失值
cor(algae[,4:18],use="complete.obs")
symnum(cor(algae[,4:18], use = 'complete.obs'))#改善输出结果

#填补缺失值
data(algae)#重新读取数据
algae <- algae[-manyNAs(algae),]#将缺失值个数大于列数20%的行去掉。
lm(PO4 ~ oPO4,data=algae)#求线性模型


algae[28,'PO4'] <- 42.897 + 1.293 * algae[28,'oPO4']#通过线性关系函数来填补缺失值

#批量填补缺失值
data(algae)#重新读取数据
algae <- algae[-manyNAs(algae),]
#定义模型线性函数模型
fillPO4 <- function(oP) {
   if (is.na(oP)) return(NA)
   else return(42.897 + 1.293 * oP)
}
#用自定义的函数批量填补缺失值
algae[is.na(algae$PO4),'PO4'] <- 
    sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)


histogram(~ mxPH | season,data=algae)#绘制season条件下的mxPh的直方图


algae$season <- factor(algae$season,levels=c('spring','summer','autumn','winter'))#指定季节顺序,即‘春夏秋冬。


histogram(~ mxPH | size*speed,data=algae) #绘制条件size*peed下的mxPH的直方图


stripplot(size ~ mxPH | speed, data=algae, jitter=T)#绘制在词流大小和速度条件下的mxPH散点图


#通过探索案列直接的相似性来填补缺失值
data(algae)#重新读取数据
algae <- algae[-manyNAs(algae),]#将缺失值个数大于列数20%的行去掉。


algae <- knnImputation(algae,k=10)#默认用众数来填补缺失值。


algae <- knnImputation(algae,k=10,meth='median')#用中位数来填补缺失值。



###################################################
### 获取预测模型
###课本页码：43 
###################################################
data(algae)#重新读取数据
algae <- algae[-manyNAs(algae), ]#将缺失值个数大于列数20%的行去掉。
clean.algae <- knnImputation(algae, k = 10)#数据清洗， 用包提供的通过案例之间的相似性来填补缺失值，得到的数据将不会含有缺失值。


lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12])#建立线性回归模型，用所有的变量来预测变量a1。


summary(lm.a1)#线性模型，a1的，统计特征概览


anova(lm.a1)#对线性模型进行拟合方差序惯分析


lm2.a1 <- update(lm.a1, . ~ . - season)#由于season对减少拟合误差的贡献最小，将它剔除


summary(lm2.a1)#线性模型，a1的，新的统计特征概览


anova(lm.a1,lm2.a1)#对两个模型进行比较


final.lm <- step(lm.a1)#不断重复模型之间的比较。


summary(final.lm)#得到最佳模型的踪迹概览。



###################################################
### 回归树
###课本页码：49 
###################################################
library(rpart)#加载rpart归树包
data(algae)#重新加载数据
algae <- algae[-manyNAs(algae), ]#将缺失值个数大于列数20%的行去掉。
rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])#建立回归树模型


rt.a1#显示回归树对象


prettyTree(rt.a1)#绘制回归树图

#下面开始对回归树进行修剪
printcp(rt.a1)#计算cp值


rt2.a1 <- prune(rt.a1,cp=0.08)#重新建树
rt2.a1#显示回归树对象


set.seed(1234) # 确保和书中结果一样
(rt.a1 <- rpartXse(a1 ~ .,data=algae[,1:12]))#迭代重新建树


first.tree <- rpart(a1 ~ .,data=algae[,1:12])#重新建立回归树模型
snip.rpart(first.tree,c(4,7))#指定修剪节点号



prettyTree(first.tree)#绘制回归树图
snip.rpart(first.tree)#交互式修建回归树


###################################################
### 模型的评价和选择
###课本页码：53
###################################################
lm.predictions.a1 <- predict(final.lm,clean.algae)#获取线性回归模型的预测
rt.predictions.a1 <- predict(rt.a1,algae)#获取回归树的预测
#以上得预测到结果后，以下就可以计算其平均绝对误差

(mae.a1.lm <- mean(abs(lm.predictions.a1-algae[,'a1'])))#线性回归模型的平均绝对误差
(mae.a1.rt <- mean(abs(rt.predictions.a1-algae[,'a1'])))#回归树模型的平均绝对误差

#另一种计算误差度量的是均方误差，但由于单位不统一，故此不采用
(mse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2))#线性回归模型的均方误差
(mse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2))#回归树模型的均方误差

#本案例采用标准后的平均绝对误差
(nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))#线性回归模型的标准后的平均绝对误差
(nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))#回归树模型的标准后的平均绝对误差


regr.eval(algae[,'a1'],rt.predictions.a1,train.y=algae[,'a1'])#包提供的regr.eval函数来衡量线性模型的性能度量指标函数

#两种模型的可视化模型预测值
old.par <- par(mfrow=c(1,2))#设置R图形输出，设置为1行2列
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")#绘制预测图
abline(0,1,lty=2)#画虚线
plot(rt.predictions.a1,algae[,'a1'],main="Regression Tree",
     xlab="Predictions",ylab="True Values")#绘制预测图
abline(0,1,lty=2)#画虚线
par(old.par)#设置R图形输出，设置为1行2列


#交互式检查预测值特别差的样本点
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")#绘制预测图
abline(0,1,lty=2)#画虚线
algae[identify(lm.predictions.a1,algae[,'a1']),]#查看图中特别差的点所在的样本


#优化线性回归模型，
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0,0,lm.predictions.a1)
regr.eval(algae[,'a1'],lm.predictions.a1,stats=c('mae','mse'))
regr.eval(algae[,'a1'],sensible.lm.predictions.a1,stats=c('mae','mse'))

#评价两个模型的性能评估值函数算法
cv.rpart <- function(form,train,test,...) {
  m <- rpartXse(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

#训练、测试、评估=交叉验证
res <- experimentalComparison(
            c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
            c(variants('cv.lm'), 
              variants('cv.rpart',se=c(0,0.5,1))),
            cvSettings(3,10,1234))


summary(res)#交叉验证比较结果概要


plot(res)#交叉验证的可视化


getVariant('cv.rpart.v1',res)#显示标志参数

#对所有7个预测任务进行与上面相似的比较试验
DSs <- sapply(names(clean.algae)[12:18],
         function(x,names.attrs) { 
           f <- as.formula(paste(x,"~ ."))
           dataset(f,clean.algae[,c(names.attrs,x)],x) 
         },
         names(clean.algae)[1:11])
#开始交叉验证比较，等待验证完成
res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1))
                   ),
                  cvSettings(5,10,1234))


plot(res.all)#绘制交叉验证图，可以看出只有a1预测好点


bestScores(res.all)#预测结果对应的最优模型

#随机森林模型
library(randomForest)#加载随机森林包
#评价模型的性能评估值函数算法，里包含随机森林函数
cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
#开始交叉验证比较，等待验证完成,过程几分钟左右，根据自己电脑性能
res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1)),
                    variants('cv.rf',ntree=c(200,500,700))
                   ),
                  cvSettings(5,10,1234))


bestScores(res.all)#预测结果对应的最优模型

#比较结果的统计显著性分析
compAnalysis(res.all,against='cv.rf.v3',
               datasets=c('a1','a2','a4','a6'))



###################################################
### 预测7类海藻的频率
###课本页码:64
###################################################
#一下代码获取海藻a1-a7的模型
bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse','system'])
learners <- c(rf='randomForest',rpart='rpartXse') 
funcs <- learners[sapply(strsplit(bestModelsNames,'\\.'),
                        function(x) x[2])]
parSetts <- lapply(bestModelsNames,
                   function(x) getVariant(x,res.all)@pars)

bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a],'~ .'))
  bestModels[[a]] <- do.call(funcs[a],
          c(list(form,clean.algae[,c(1:11,11+a)]),parSetts[[a]]))
}

#填补缺失值
clean.test.algae <- knnImputation(test.algae,k=10,distData=algae[,1:11])

#获取整个测试数据集的预测矩阵
preds <- matrix(ncol=7,nrow=140)
for(i in 1:nrow(clean.test.algae)) 
  preds[i,] <- sapply(1:7,
                      function(x) 
                        predict(bestModels[[x]],clean.test.algae[i,])
                     )

#计算模型的nmse值
avg.preds <- apply(algae[,12:18],2,mean)
apply( ((algae.sols-preds)^2),           2,mean) / 
apply( (scale(algae.sols,avg.preds,F)^2),2,mean)


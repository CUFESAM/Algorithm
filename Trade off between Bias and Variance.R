
############################################################

mlm <- function(TR, TE){
    yn <- names(dataTR)[1]
    xn <- names(dataTR)[-1]                        #分别取得各个变量的变量名
    mp <- length(xn)                               #记录自变量个数

    ypr <- NULL
    tm <- paste(yn,xn[1],sep="~")            
    fam <- formula(tm)                             #生成回归方程

    cp <- 1                                        #初始化cp 

    repeat{
        lm1 <- lm(fam,TR)
        ypr <- c(ypr,predict(lm1,TE))              #记录预测值
        if(cp >= mp) break
        cp <- cp + 1
        tm <- paste(tm,xn[cp],sep="+")             #迭代生成回归方程并做训练预测
        fam <- formula(tm)
      }

  as.vector(ypr)                                   #以向量返回预测值ypr
}
  
#############################################################

BV <- function(p, sigma, N, n, k){
    #TEST
    Testx <- runif(N,-1,1)                         #生成X
    Testy <- 2 * exp(Testx) + rnorm(N,0,sigma)     #基于X构建真实值Y
    Testz <- matrix(Testx,N,p)               
    
    for(i in 2:p)
        Testz[,i] <- 10*Testz[,i]*Testz[,i-1]      #逐列生成X^p
      
    dataTE <- data.frame(y=Testy,z=Testz)          #写入TEST数据框
    
    #TRAIN
    Trainx <- runif(n * k,-1,1)
    Trainy <- 2 * exp(Trainx) + rnorm(n*k,0,sigma)
    Trainz <- matrix(Trainx,n*k,p)
    
    for(i in 2:p) 
        Trainz[,i] <- 10*Trainz[,i]*Trainz[,i-1]

    dataTR <- data.frame(y=Trainy,z=Trainz)        #同理生成TRAIN数据集

    
    index <- rep(1:k,rep(n,k))                     #创建index来对n*k行的数据进行分组
    PRE <- daply(dataTR, .(index),       
                 mlm, TE = dataTE)                 #将训练数据进行分组后依次带入自定义函数

    b <- (apply(PRE,2,mean) - rep(2*exp(nx),p))^2  #
    b <- matrix(b, nrow=N, byrow=F)                #
    b <- apply(b, 2, mean)                         #
                                                   #
    v <- apply(PRE, 2, var)                        #
    v <- matrix(v, nrow=N, byrow=F)                #
    v <- apply(v,2,mean)                           #计算每个p以及sigma下的Bias和Variance

    mse <- b+v
    
    list(mse=mse, bias=b, var=v)                   #返回MSE、Bias和Variance
    
}

############################################################
library(plyr)                                      #通过plyr包调用daply

N <- 2000                                          #测试集数量
k <- 2000                                          #循环次数
n <- 500                                           #训练集数量

set.seed(1)                                        #设置随机种子

msigma=c(1,5,10)                                   #设置不同的sigma
NL=length(msigma)
ps=cbind(3,msigma)                                 #设置p值

timestart <- Sys.time()

for(nl in 1:NL) 
    print(BV(ps[nl,1],ps[nl,2], N, n, k))          #运算并做时间记录

timeend <- Sys.time()
runningtime <- timeend-timestart

print(runningtime)

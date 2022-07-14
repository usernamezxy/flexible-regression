#1
data<-read.csv("nectar.csv")
plot(data$temp,data$weight,xlab = "temperature",ylab = "weight",main = "temperature VS weight")

#2
library(splines)
#(a)
data.cs.bs=bs(data$temp,degree = 3,knots = c(7.5,15,22.5,30),intercept = T)
#(b)
data.cs.fit=lm(data$weight~-1+data.cs.bs, data=data)
summary(data.cs.fit)
#(c)
plot(x=data$temp,y=data$weight,xlab = "temperature",ylab = "weight")
lines(x=sort(data$temp),y=fitted(data.cs.fit)[order(data$temp)],col="red",lwd=3.5)
#(d)
pre1<-predict(data.cs.fit,newdata = data,interval = "confidence")
#(e)
lines(x=sort(data$temp),y=pre1[,2][order(data$temp)],col="blue")
lines(x=sort(data$temp),y=pre1[,3][order(data$temp)],col="blue")

##3
#(a)
h1=data$temp
h2=data$temp^2
h3=data$temp^3
h4=(data$temp-7.5)
h4[h4<0]=0
h4=h4^3
h5=(data$temp-15)
h5[h5<0]=0
h5=h5^3
h6=(data$temp-22.5)
h6[h6<0]=0
h6=h6^3
h7=(data$temp-30)
h7[h7<0]=0
h7=h7^3
#(b)
data.tpsb.fit=lm(data$weight~h1+h2+h3+h4+h5+h6+h7,data = data)
summary(data.tpsb.fit)
#(c)
par(mar=c(5,5,2,2)+0.2,las=1)
plot(x=fitted(data.cs.fit),y=fitted(data.tpsb.fit),
     xlab = "cubic spline with 4 knots(B-spline basis)",
     ylab = "cubic spline with 4 knots\n(truncated power series basis)")
abline(0,1,col="red",lwd=2)


##4
#(a)
#6 internal knots
#(b)
data.ns=ns(x=data$temp,knots = c(10,13,16,19,22,25),
           Boundary.knots = c(7,28),intercept = T)
#(c)
data.ns.fit=lm(data$weight~ -1+data.ns, data=data)
summary(data.ns.fit)
#(d)
plot(x=data$temp,y=data$weight,xlab = "temperature",ylab = "weight")
lines(x=sort(data$temp),y=fitted(data.ns.fit)[order(data$temp)],col="red",lwd=3.5)
#(e)
pre2<-predict(data.ns.fit,newdata = data,interval = "confidence")
#(f)
lines(x=sort(data$temp),y=pre2[,2][order(data$temp)],col="blue")
lines(x=sort(data$temp),y=pre2[,3][order(data$temp)],col="blue")



##5
#(a)
data.sm.5df=smooth.spline(x=data$temp,y=data$weight,df=5)
data.sm.5df
#(b)
data.sm.10df=smooth.spline(x=data$temp,y=data$weight,df=10)
data.sm.10df
#(c)
data.sm.20df=smooth.spline(x=data$temp,y=data$weight,df=20)
data.sm.20df
#(d)
plot(x=data$temp,y=data$weight,xlab="temperature",ylab="weight")
lines(x=sort(data$temp),y=fitted(data.sm.5df)[order(data$temp)],col="red",lwd=3.5)
plot(x=data$temp,y=data$weight,xlab="temperature",ylab="weight")
lines(x=sort(data$temp),y=fitted(data.sm.10df)[order(data$temp)],col="red",lwd=3.5)
plot(x=data$temp,y=data$weight,xlab="temperature",ylab="weight")
lines(x=sort(data$temp),y=fitted(data.sm.20df)[order(data$temp)],col="red",lwd=3.5)
#(e)
plot(x=data$temp,y=data$weight,xlab="temperature",ylab="weight")

lines(x=sort(data$temp),y=fitted(data.sm.5df)[order(data$temp)],col="red",lwd=2)
lines(x=sort(data$temp),y=fitted(data.sm.10df)[order(data$temp)],col="green",lwd=2)
lines(x=sort(data$temp),y=fitted(data.sm.20df)[order(data$temp)],col="blue",lwd=2)
legend(0,14,lty=c(1,1,1),lwd = c(2,2,2),col = c("red","green","blue"),legend = c("df=5","df=10","df=20"))



#6
library(splines)
data<-read.csv("nectar.csv")
cverror<-c()
for (j in 1:10) {
  kn=seq(1,34,length.out=2+j)
  kn=kn[-1]; kn=kn[-length(kn)]
  cve<-c()
  for (i in 1:10) {
    x<-NULL
    k<-NULL
    x<-data.frame("x"=data$temp[data$group==i])
    y<-data.frame("y"=data$weight[data$group==i])
    k<-data.frame("x"=data$temp[data$group!=i],"y"=data$weight[data$group!=i])  
    colnames(x) <- c('xx')
    colnames(k) <- c('xx','yy')
    data.k1.ns.fit=lm(yy~ -1+ns(x=xx,knots = kn,
                                Boundary.knots = c(1,34),intercept = T), data=k)
    
    pre.k<-predict(data.k1.ns.fit,newdata = x)
    cve[i]<-sum((y-pre.k)^2)
  }
  cverror[j]<-sum(cve)
}
CVerror=cverror/length(data$temp)
plot(1:10,CVerror,xlab="number of knots", ylab="overall cv error")
which.min(CVerror)
CVerror







#2a)
nectar.6k = bs(data$temp, df = 8, degree = 3,
               knots = c(7.5, 15, 22.5,30), intercept = T)
#2b)
nectar.6k.fit = lm(data$weight ~ -1 + nectar.6k,
                   data = data)
summary(nectar.6k.fit)



#2c)
plot(x = data$temp, y = data$weight,
     pch = 16, col = "#00000077", las = 1,
     xlab = "Temperature", ylab = "weight")
lines(x = sort(data$temp),y = fitted(nectar.6k.fit)[order(data$temp)],
      col = "red", lwd = 2)




#2d)
preds<-predict(nectar.6k.fit, newdata = data, interval = "confidence")



predsd<-predict(nectar.6k.fit, newdata = data, interval = "prediction")



#2e)
# intervals
lines(x=sort(data$temp), y=preds[ ,2][order(data$temp)], col = 'blue')
lines(x=sort(data$temp), y=preds[ ,3][order(data$temp)], col = 'blue')


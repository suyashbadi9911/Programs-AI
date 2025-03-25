
install.packages("rpart.plot")	#	install	package	rpart.plot
library("rpart")	#	load	libraries
library("rpart.plot")

play_decision	<-	read.table("C:/Users/DELL/Documents/DSV/datasets/bank-sample.csv",header=TRUE,sep=",")
play_decision

summary(play_decision)

x<- sort(runif(1000))
y<-data.frame(x=x,y=-x*log2(x)-(1-x)*log2(1-x))
plot(y,type="l",xlab="P(X=1)",ylab=expression("H"["X"]))
grid()

fit	<-	rpart(subscribed~job+marital+education+default+housing+loan+contact+poutcome,
             method="class",
             data=play_decision,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))

summary(fit)


rpart.plot(fit,	type=4,	extra=2,	clip.right.labs=FALSE,
           varlen=0,	faclen=3)

newdata	<-	data.frame(job="retired",
                      marital="married",
                      education="secondary",
                      default="no",
                      housing="yes",
                      loan="no",
                      contact="cellular",
                      duration=598,
                      poutcome="unknown"
                      )
newdata

predict(fit,newdata=newdata,type=c("class"))

library("rpart")	#	load	libraries
library("rpart.plot")


income_input	=	as.data.frame(	read.csv("C:/Users/DELL/Documents/DSV/datasets/income.csv")	)
income_input[1:10,]
summary(income_input)

library(lattice)

splom(~income_input[c(2:5)],	groups=NULL,	data=income_input,
       axis.line.tck	=	0,
       axis.text.alpha	=	0)

results	<- lm (Income ~ Age	+	Education	+	Gender,	income_input)
summary(results)

results2 <- lm (Income	~ Age	+	Education, income_input)
summary(results2)

results3	<-	lm(Income ~ Age	+	Education,
               +	Alabama,
               +	Alaska,
               +	Arizona,
               ,               
               ,
               ,
               +	WestVirginia,
               +	Wisconsin,
               income_input)

confint(results2,	level	=	.95)
Age	<-	41
Education	<-	12
new_pt	<-	data.frame(Age,	Education)
conf_int_pt	<-	predict(results2,new_pt,level=.95,interval="confidence")
conf_int_pt

pred_int_pt	<-	predict(results2,new_pt,level=.95,interval="prediction")
pred_int_pt	

with(results2,	{
plot(fitted.values,	residuals,ylim=c(-40,40)	)
  points(c(min(fitted.values),max(fitted.values)),
    c(0,0),	type	=	"1")})


hist(results2$residuals,	main="")

qqnorm(results2$residuals,	ylab="Residuals",	main="")
qqline(results2$residuals)


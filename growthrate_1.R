mtemp <- read.table("mt1master1.txt", header=TRUE, dec=".")
attach(mtemp)
#attempt with glm
glm.out = glm(col_diam_mm ~ dpi, family=binomial(link="logit"), data=mtemp)
#using grofit
library(grofit)
growth <- gcFitSpline(dpi,col_diam_mm)
print(summary(growth))
plot(growth)
grtable<-NULL
temp<-unique(temp_C)
is<-levels(isolate)
for (i in seq_along(is)){
	for (t in 1:6){
		growth <- gcFitSpline(mtemp$dpi[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]],
		mtemp$col_diam_mm[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]])
		print(summary(growth)[1])
		plot(growth)
		grtable_i=data.frame(cbind(is[[i]],
		temp[[t]], summary(growth)[1]))
		grtable=rbind(grtable, grtable_i)
	}
}
print(grtable)
class(grtable)
colnames(grtable)=c("isolate", "temp", "grate")
print(grtable)
lmgr=lm(grtable$grate~grtable$temp+grtable$isolate)
summary(lmgr)
aov=aov(grtable$grate~as.factor(grtable$temp))
summary(aov)
TukeyHSD(aov)
box=boxplot(grtable$grate~as.factor(grtable$temp))
boxis=boxplot(grtable$grate~grtable$temp+grtable$isolate)

#test if formula is working
growth=gcFitSpline(mtemp$dpi[mtemp$isolate=="Dm13"&mtemp$temp_C==15],
mtemp$col_diam_mm[mtemp$isolate=="Dm13"&mtemp$temp_C==15])	
print(summary(growth))
plot(growth)

#calculate growth rate for each rep
mtemp <- read.table("mt1master3.txt", header=TRUE, dec=".")
attach(mtemp)
library(grofit)
growth <- gcFitSpline(dpi,col_diam_mm)
print(summary(growth))
plot(growth)
grtable<-NULL
temp<-unique(temp_C)
is<-levels(isolate)
rep<-unique(replicate)
for (i in seq_along(is)){
	for (t in 1:6){
		for (r in 1:2){
		growth <- gcFitSpline(mtemp$dpi[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]&mtemp$replicate==rep[[r]]],
		mtemp$col_diam_mm[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]&mtemp$replicate==rep[[r]]])
		print(summary(growth)[1])
		plot(growth)
		x11()
		#par(mfrow=c(2,2))
		grtable_i=data.frame(cbind(is[[i]],temp[[t]],rep[[r]],summary(growth)[1]))
		grtable=rbind(grtable, grtable_i)
		}
	}
}
print(grtable)
class(grtable)
colnames(grtable)=c("isolate", "temp", "rep", "grate")
print(grtable)
#lm
lmgr=lm(grtable$grate~grtable$temp+grtable$isolate)
summary(lmgr)
aovt=aov(grtable$grate~as.factor(grtable$temp))
summary(aovt)
TukeyHSD(aovt)
aovi=aov(grtable$grate~as.factor(grtable$isolate))
summary(aovi)
TukeyHSD(aovi)
box=boxplot(grtable$grate~as.factor(grtable$temp))
boxis=boxplot(grtable$grate~grtable$temp+grtable$isolate)
boxt=boxplot(grtable$grate~grtable$isolate)
#boxplot grate~isolate each temp
tempgr=unique(temp)
for (t in 1:6){
	boxt=boxplot(grtable$grate[grtable$temp==tempgr[[t]]]~grtable$isolate[grtable$temp==tempgr[[t]]])
}
#grate~isolate each temp
tempgr=unique(temp)
for (t in 1:6){
	lmgrt=lm(grtable$grate[grtable$temp==tempgr[[t]]]~grtable$isolate[grtable$temp==tempgr[[t]]])
	print(summary(lmgrt))
	plot(lmgrt)
	boxplot(lmgrt)
}
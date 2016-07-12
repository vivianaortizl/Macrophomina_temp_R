mtemp <- read.csv("mt2master.csv")
attach(mtemp)
#attempt with glm
glm.out = glm(col_diam_mm ~ dpi, family=binomial(link="logit"), data=mtemp)
#using grofit
library(grofit)
growth <- gcFitSpline(hpi_corr,col_diam_mm_corr)
print(summary(growth))
plot(growth)
grtable<-NULL
temp<-unique(temp_C)
is<-levels(isolate)
for (i in seq_along(is)){
	for (t in 1:6){
		growth <- gcFitSpline(mtemp$hpi_corr[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]],
		mtemp$col_diam_mm_corr[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]])
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
mtemp <- read.csv("mt2master.csv")
attach(mtemp)
library(grofit)
growth <- gcFitSpline(hpi,col_diam_mm_corr)
print(summary(growth))
plot(growth)
grtable<-NULL
temp<-unique(temp_C)
is<-levels(isolate)
rep<-unique(replicate)
for (i in seq_along(is)){
	for (t in 1:2){
		for (r in 1:3){
		growth <- gcFitSpline(mtemp$hpi[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]&mtemp$replicate==rep[[r]]],
		mtemp$col_diam_mm_corr[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]&mtemp$replicate==rep[[r]]])
		print(summary(growth)[1])
		plot(growth)
		region=mtemp$region[mtemp$isolate==is[[i]]&mtemp$temp_C==temp[[t]]&mtemp$replicate==rep[[r]]]
		grtable_i=data.frame(cbind(is[[i]],temp[[t]],rep[[r]],summary(growth)[1], region))
		grtable=rbind(grtable, grtable_i)
		}
	}
}
print(grtable)
class(grtable)
colnames(grtable)=c("isolate", "temp", "rep", "grate", "region")
print(grtable)
#lm
lmgr=lm(grate~region*isolate, data = grtable[grtable$temp==10,])
summary(lmgr)
plot(lmgr)
aovt=aov(grate~as.factor(temp), data= grtable[grtable$temp==10,])
summary(aovt)
anova(lmgr)
lsmeans_fit <- lsmeans(lmgr,"temp", by="isolate")
contrast(lsmeans_fit, "pairwise", adjust="tukey")


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
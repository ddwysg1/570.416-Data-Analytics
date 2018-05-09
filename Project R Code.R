EPL<- read.csv(file="EPL.csv",header=TRUE,sep=",")
#Part 1 lm for Pts and Rank
EPL<-EPL[,3:11]
pts.fit<-lm(Pts~.-Rank-Championship,data=EPL)
summary(pts.fit)
rank.fit<-lm(Rank~.-Championship,data=EPL)
summary(rank.fit)
rank.fit2<-lm(Rank~.-Championship-Pts,data=EPL)
summary(rank.fit2)
#Part 2 logistic regression for Championship
champion.fit=glm(Championship~.-Rank,data=EPL,family=binomial)
summary(champion.fit)
coef(champion.fit)
summary(champion.fit)$coef
champion.probs=predict(champion.fit,type="response")
champion.probs[1:10]
champion.pred=rep(0,200)
champion.pred[champion.probs>.5]=1
table(champion.pred,EPL$Championship)
champion.fit2=glm(Championship~.-Rank-Pts,data=EPL,family=binomial)
summary(champion.fit2)
coef(champion.fit2)
summary(champion.fit2)$coef
champion.probs2=predict(champion.fit2,type="response")
champion.probs2[1:10]
champion.pred2=rep(0,200)
champion.pred2[champion.probs2>.5]=1
table(champion.pred2,EPL$Championship)
#Part 3 Subsetting
library(leaps)
Pts.full=regsubsets(Pts~.-Rank-Championship,data=EPL,nvmax=6)
summary(Pts.full)
Pts.summary=summary(Pts.full)
names(Pts.summary)
Pts.summary$rsq
Pts.fwd=regsubsets(Pts~.-Rank-Championship,data=EPL,nvmax=6,method="forward")
summary(Pts.fwd)
Pts.bwd=regsubsets(Pts~.-Rank-Championship,data=EPL,nvmax=6,method="backward")
summary(Pts.bwd)
coef(Pts.full,6)
coef(Pts.fwd,6)
coef(Pts.bwd,6)
library(MASS)
null=lm(Pts~1, data=EPL)
full=lm(Pts~.-Rank-Championship, data=EPL)
forward.fit<-stepAIC(null, scope=list(lower=null, upper=full), scale=0, direction="forward")
backward.fit<-stepAIC(full, data=rawdata, direction="backward")
twoway.fit<-stepAIC(null, scope = list(upper=full), data=rawdata, direction="both")
#Part 4 Clustering
km.out<-kmeans(EPL,3,nstart=100)
km.out$cluster
hc.complete=hclust(dist(EPL), method="complete")
hc.average=hclust(dist(EPL), method="average")
cutree(hc.complete, 3)
cutree(hc.average, 3)
#Part 5 Decision Tree
new<-matrix(,200,1)
EPLclass<-data.frame(EPL,Class=new)
for (i in 1:200){if ((EPLclass[i,8]>6)&&(EPLclass[i,8]<18)){EPLclass[i,10]<-"Normal"} 
else if (EPLclass[i,8]<7){EPLclass[i,10]<-"Europe"}
else if (EPLclass[i,8]>17){EPLclass[i,10]<-"Relegation"}}
EPLclass$Class <- factor(EPLclass$Class,levels=c("Normal", "Europe", "Relegation"))
library(tree)
tree.EPL=tree(Class~.-Rank-Championship,EPLclass)
summary(tree.EPL)
plot(tree.EPL)
text(tree.EPL,pretty=0)
tree.EPL
set.seed(1)
train=sample(200, 200*0.6)
EPLtree.test=EPLclass[-train,]
tree.EPL.test=tree(Class~.-Rank-Championship,data=EPLclass,subset=train)
tree.pred=predict(tree.EPL.test,EPLtree.test,type="class")
table(tree.pred,EPLtree.test$Class)
set.seed(1)
cv.EPLtree=cv.tree(tree.EPL.test,FUN=prune.misclass)
names(cv.EPLtree)
cv.EPLtree
par(mfrow=c(1,2))
plot(cv.EPLtree$size,cv.EPLtree$dev,type="b")
plot(cv.EPLtree$k,cv.EPLtree$dev,type="b")
prune.EPLtree=prune.misclass(tree.EPL.test,best=7)
plot(prune.EPLtree)
text(prune.EPLtree,pretty=0)
tree.pred2=predict(prune.EPLtree,EPLtree.test,type="class")
table(tree.pred2,EPLtree.test$Class)
#Part 6 Random Forest
library(caret)
model_rf <- train(Class~.-Rank-Championship, data=EPLclass, method="rf",trControl=trainControl(method='cv',number=10),na.action=na.exclude)
model_rf
#modeling
set.seed(1)
train<-sample(200,200*0.6)
EPL.test = EPLclass[-train]
bag.EPL=randomForest(Class~.-Rank-Championship, data=EPLclass,subset=train,mtry=4,importance=TRUE)
bag.EPL
predata<- predict(bag.EPL,newdata=EPLclass[-train,])
pre<-matrix(,80,1)
for (i in 1:80){pre[i,1]<-predata[i]}
dim(pre)
obs<-matrix(,80,1)
i<-1
for (j in 1:200)
{if (is.element(j,train)==FALSE) 
{obs[i,1]<-EPLclass[j,10]
i<-i+1
}
}
dim(obs)
table(pre,obs)
EPLbinomial<-EPL
EPLbinomial$Championship <- as.factor(EPLbinomial$Championship)
#Find best mtry
model_rf2 <- train(Championship~.-Rank, data=EPLbinomial, method="rf",trControl=
trainControl(method='cv',number=10),na.action=na.exclude)
model_rf2
#modeling
set.seed(1)
train2<-sample(200,200*0.6)
EPL.test2 = EPLbinomial[-train2]
bag.EPL2=randomForest(Championship~.-Rank,data=EPLbinomial,subset=train2,mtry=2,importance=TRUE)
bag.EPL2
preEPLbinomial<- predict(bag.EPL2,newdata=EPLbinomial[-train2,])
pre2<-matrix(,80,1)
for (i in 1:80){pre2[i,1]<-preEPLbinomial[i]}
dim(pre2)
obs2<-matrix(,80,1)
i<-1
for (j in 1:200) 
{if (is.element(j,train2)==FALSE) 
{obs2[i,1]<-EPLbinomial[j,9]
i<-i+1
} 
}
dim(obs2)
prediction2<-pre2[,1]
observation2<-obs2[,1]
table(prediction2,observation2)
library(ROCR)
pred <- prediction(prediction2,observation2)
pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve - Random Forest [test]")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,label=paste("AUC =", round(au, 2)))
print(p)
#Part 7 EPL vs. La Liga
LaLiga<- read.csv(file="La Liga.csv",header=TRUE,sep=",")
LaLiga<-LaLiga[,3:11]
pts2.fit<-lm(Pts~.-Rank-Championship,data=LaLiga)
summary(pts2.fit)
rank2.fit<-lm(Rank~.-Championship,data=LaLiga)
summary(rank2.fit)
rank2.fit2<-lm(Rank~.-Championship-Pts,data=LaLiga)
summary(rank2.fit2)
champion2.fit=glm(Championship~.-Rank,data=LaLiga,family=binomial)
summary(champion2.fit)
coef(champion2.fit)
summary(champion2.fit)$coef
champion2.probs=predict(champion2.fit,type="response")
champion2.probs[1:10]
champion2.pred=rep(0,100)
champion2.pred[champion2.probs>.5]=1
table(champion2.pred,LaLiga$Championship)
new<-matrix(,100,1)
LaLigaclass<-data.frame(LaLiga,Class=new)
for (i in 1:100){if ((LaLigaclass[i,8]>6)&&(LaLigaclass[i,8]<18)){LaLigaclass[i,10]<-"Normal"} 
else if (LaLigaclass[i,8]<7){LaLigaclass[i,10]<-"Europe"}
else if (LaLigaclass[i,8]>17){LaLigaclass[i,10]<-"Relegation"}}
LaLigaclass$Class <- factor(LaLigaclass$Class,levels=c("Normal", "Europe", "Relegation"))
library(tree)
tree.LaLiga=tree(Class~.-Rank-Championship,LaLigaclass)
summary(tree.LaLiga)
plot(tree.LaLiga)
text(tree.LaLiga,pretty=0)
tree.LaLiga
set.seed(1)
train=sample(100, 100*0.6)
LaLigatree.test=LaLigaclass[-train,]
tree.LaLiga.test=tree(Class~.-Rank-Championship,data=LaLigaclass,subset=train)
tree.pred=predict(tree.LaLiga.test,LaLigatree.test,type="class")
table(tree.pred,LaLigatree.test$Class)
set.seed(1)
cv.LaLigatree=cv.tree(tree.LaLiga.test,FUN=prune.misclass)
names(cv.LaLigatree)
cv.LaLigatree
par(mfrow=c(1,2))
plot(cv.LaLigatree$size,cv.LaLigatree$dev,type="b")
plot(cv.LaLigatree$k,cv.LaLigatree$dev,type="b")
prune.LaLigatree=prune.misclass(tree.LaLiga.test,best=5)
plot(prune.LaLigatree)
text(prune.LaLigatree,pretty=0)
tree.pred2=predict(prune.LaLigatree,LaLigatree.test,type="class")
table(tree.pred2,LaLigatree.test$Class)
#Part 8 Sir Alex Ferguson vs. Wenger
SAF<-read.csv(file="EPL.csv",header=TRUE,sep=",")
SAF<-SAF[which(SAF$Team == "Manchester United"),]
Wenger<-read.csv(file="EPL.csv",header=TRUE,sep=",")
Wenger<-Wenger[which(Wenger$Team == "Arsenal"),]
SAF<-SAF[,3:10]
Wenger<-Wenger[,3:10]
SAFPts<-lm(Pts~.-Rank,data=SAF)
SAFRank<-lm(Rank~.-Pts,data=SAF)
WengerPts<-lm(Pts~.-Rank,data=Wenger)
WengerRank<-lm(Rank~.-Pts,data=Wenger)
summary(SAFPts)
summary(WengerPts)
summary(SAFRank)
summary(WengerRank)
#Part 9 Effect of Important Addition
EPL_addtion<- read.csv(file="EPL.csv",header=TRUE,sep=",")
EPL_addtion<-EPL_addtion[,3:13]
EPL_addtion<-na.omit(EPL_addtion)
EPL_addtion<-data.frame(EPL_addtion,Rankchange=(EPL_addtion[,10]-EPL_addtion[,8]),Valuechange=EPL_addtion[,11]/(EPL_addtion[,4]-EPL_addtion[,11]))
Rankchange.fit<-lm(Rankchange~Last.year.s.rank+Most.Important.Addition.Depart,data=EPL_addtion)
summary(Rankchange.fit)
Rankchange2.fit<-lm(Rankchange~Last.year.s.rank+Valuechange,data=EPL_addtion)
summary(Rankchange2.fit)
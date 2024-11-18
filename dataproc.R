library(qualtRics)
library(ggplot2)
library(stringr)
library(dplyr)
library(brms)
library(car)

fn<-choose.files()
dat<-qualtRics::read_survey(fn)

scores <- apply(dat, 1, function(ln){
  as.numeric(str_sub(ln[12:44],1,1))
}
) %>% t() %>% data.frame() 

scores<-cbind(scores, as.numeric(dat$QID14_1))
scores<-cbind(scores, as.factor(dat$QID13))
colnames(scores)<-append(append(colnames(dat[,12:44]),'total'),'dep')

nacheck<-rowMeans(scores[,1:33])
scores<-subset(scores, !(is.na(nacheck)))

duty<-scores[,grep('職務上の',colnames(scores))]
person<-scores[,grep('対人',colnames(scores))]
clean<-scores[,grep('清潔',colnames(scores))]
sincery<-scores[,grep('誠実',colnames(scores))]
organ<-scores[,grep('組織',colnames(scores))]

scores$duty<-apply(scores[,grep('職務上の',colnames(scores))], 1, function(scs){
  rowMeans(t(scs))
})

scores$person<-apply(scores[,grep('対人',colnames(scores))], 1, function(scs){
  rowMeans(t(scs))
})

scores$clean<-apply(scores[,grep('清潔',colnames(scores))], 1, function(scs){
  rowMeans(t(scs))
})

scores$sincery<-apply(scores[,grep('誠実',colnames(scores))], 1, function(scs){
  rowMeans(t(scs))
})

scores$organ<-apply(scores[,grep('組織',colnames(scores))], 1, function(scs){
  rowMeans(t(scs))
})

# modeling

fit1<-brm(total ~ duty + person + duty + sincery + organ, 
          data = scores,
          iter = 2000, 
          warmup = 1000, 
          chain = 4)

fit2<-brm(
  formula = total ~ duty + person + duty + sincery + organ + (1|dep),
  data = scores,
  iter = 2000, 
  warmup = 1000, 
  chain = 4)

summary(fit2)

fit3<-brm(
  formula = total ~ organ * dep,
  data = scores,
  iter = 2000, 
  warmup = 1000, 
  chain = 4)

summary(fit3)
conditions<-data.frame(dep=c(unique(scores$dep)))
eff3<-conditional_effects(fit3, effects='organ', conditions=conditions)
plot(eff3, points=T)

# multiple regression
lm1<-lm(total ~ duty + person + duty + sincery + organ, scores)
model<-step(lm1)
summary(model)

# 各因子の箱ひげ図
gduty<-ggplot(data=scores, aes(x=dep, y=duty))+geom_boxplot()
plot(gduty)

gperson<-ggplot(data=scores, aes(x=dep, y=person))+geom_boxplot()
plot(gperson)

gclean<-ggplot(data=scores, aes(x=dep, y=clean))+geom_boxplot()
plot(gclean)

gsincery<-ggplot(data=scores, aes(x=dep, y=sincery))+geom_boxplot()
plot(gsincery)

gorgan<-ggplot(data=scores, aes(x=dep, y=organ))+geom_boxplot()
plot(gorgan)

gtotal<-ggplot(data=scores, aes(x=dep, y=total))+geom_boxplot()
plot(gtotal)

# 部署ごとに色分けした散布図
gpoint<-ggplot(data=scores, aes(x=organ, y=total, color=dep))+geom_point()+geom_smooth(method='lm', se=F)
plot(gpoint)

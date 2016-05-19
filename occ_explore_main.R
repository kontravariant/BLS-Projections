#clear env
rm(list=ls())
library(ggplot2)
library(reshape2)
library(Hmisc)
library(pastecs)
setwd("~/Projects/bls_proj/")
#read in previous (2002-2012) and current (2012-2022) BLS projections
prev = read.csv('~/Projects/bls_proj/data/bls_occ_proj_2012.csv',header=TRUE,check.names = FALSE)
curr = read.csv('~/Projects/bls_proj/data/bls_occ_proj_2022.csv',header=TRUE,check.names = FALSE)
#convert all counts from thousands to millions
prev[,3:5] = prev[,3:5]/1000
curr[,3:5] = curr[,3:5]/1000
#melt both data sets to long format
curr.rs = melt(curr,id.vars='Title')
prev.rs = melt(prev,id.vars='Title')

#pare down data sets to get just 2012 values (predicted then actual)
pred12 = prev.rs[which(prev.rs$variable=='pred.count12'),]
count12 = curr.rs[which(curr.rs$variable=='count12'),]
#make data sets wide
pred12.wide = reshape(pred12,idvar = 'Title',direction = 'wide',timevar = 'variable')
count12.wide = reshape(count12,idvar = 'Title',direction = 'wide',timevar = 'variable')
#merge wide sets
emp12.interm = merge(pred12.wide,count12.wide,by='Title')
emp12 = emp12.interm[-1,]
#counts to numeric
emp12$value.pred.count12 = as.numeric(emp12$value.pred.count12)
emp12$value.count12 = as.numeric(emp12$value.count12)
#difference in thousands
emp12$diff = emp12$value.pred.count12 - emp12$value.count12
#proportional difference (%)
emp12$pr.err = (emp12$value.pred.count12/emp12$value.count12 - 1) * 100
emp12 = emp12[order(emp12$pr.err),]
#get new table of just 2022 predictions
emp22 = curr[,1:4]
#add
emp12.errs = subset(emp12,select = c("Title","pr.err"))
emp12.errs = emp12.errs[emp12.errs$Title != 'All occupations',]
emp22.full = merge(emp22,emp12.errs,by='Title')
#multipliers, mean proportional error in 2012 prediction +/- 95% CI of mean
avg.mult = stat.desc(emp12$pr.err)[9]
#low.mult = (1+((stat.desc(emp12$pr.err)[9]-stat.desc(emp12$pr.err)[11])/100))
#high.mult = (1+((stat.desc(emp12$pr.err)[9]+stat.desc(emp12$pr.err)[11])/100))
#emp22$pred.max = round(emp22$pred.count22/low.mult)
#emp22$pred.min = round(emp22$pred.count22/high.mult)
emp22.full$pred.adj = emp22.full$pred.count22*(1-emp22.full$pr.err/100)
emp22.melt = melt(emp22.full,id.vars = 'Title')
emp22.melt = emp22.melt[grep('pred',emp22.melt$variable),]
emp22.melt = (emp22.melt[order(emp22.melt$value),])
emp22.melt$value = as.numeric(emp22.melt$value)

emp22.sig = emp22.full[which(abs(emp22.full$pr.err)>15),]
emp22.sig = melt(emp22.sig,id.vars = 'Title')
emp22.sig = emp22.sig[grep('pred',emp22.sig$variable),]
emp22.sig$value = as.numeric(emp22.sig$value)

p = ggplot(data = emp12,aes(x=reorder(Title, -pr.err),y=pr.err)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=20,hjust=0.9,vjust=0.5))+
  ggtitle('Percent Difference By Occupation, 2012 Predictions')

a = 1:10
b = a*20000
p2 = ggplot(data=emp22.melt,aes(x=Title,y=value)) + 
  geom_bar(position = 'dodge',stat='identity',aes(fill=factor(variable))) +
  theme(axis.text.x=element_text(angle=20,hjust=0.9,vjust=0.5)) +
  ggtitle('Prediction Bounds For 2022 Based on 2012 Prediction Error')+
  scale_y_continuous(breaks = round(seq(from=0, to=30000, by = 2500),1))
print(p2)

p3 = ggplot(data=emp22.sig,aes(x=Title,y=value)) + 
  geom_bar(position = 'dodge',stat='identity',aes(fill=factor(variable))) +
  theme(axis.text.x=element_text(angle=20,hjust=0.9,vjust=0.5)) +
  ggtitle('Top Erroneous Predictions, for 2022')+
  scale_y_continuous(breaks = round(seq(from=0, to=100, by = 1),1))
print(p3)
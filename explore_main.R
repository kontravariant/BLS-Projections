library(ggplot2)
library(reshape2)
setwd("~/Projects/bls_proj/")
prev = read.csv('~/Projects/bls_proj/data/bls_proj_2012_clean.csv',header=TRUE,check.names = FALSE)
curr = read.csv('~/Projects/bls_proj/data/bls_proj_2022_clean.csv',header=TRUE,check.names = FALSE)
tops = subset(prev,prev$count92>15000)
counts = tops[order(tops$count92),]
barplot(counts$count92,names.arg = counts$Sector,horiz=FALSE,las=2)
ggplot(data = tops,aes(x=Sector,y=count92)) + geom_bar(stat = "identity")
rm(counts)

tops.rs = melt(tops,id.vars = c('Sector'))
tops.new = tops.rs[which(tops.rs$variable=='count92'|tops.rs$variable=='count02'|tops.rs$variable=='count12'),]
tops.new = tops.new[order(tops.new$value),]
ggplot(tops.new,aes(Sector,value,fill=as.factor(variable))) + 
  geom_bar(position = 'dodge',stat='identity') +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=0.5))
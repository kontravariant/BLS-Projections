#clear env
rm(list=ls())
library(ggplot2)
library(reshape2)
library(Hmisc)
library(pastecs)
setwd("~/Projects/bls_proj/")
#read in previous (2002-2012) and current (2012-2022) BLS projections
prev = read.csv('~/Projects/bls_proj/data/bls_proj_2012_clean.csv',header=TRUE,check.names = FALSE)
curr = read.csv('~/Projects/bls_proj/data/bls_proj_2022_clean.csv',header=TRUE,check.names = FALSE)
#melt both data sets to long format
curr.rs = melt(curr,id.vars='Sector')
prev.rs = melt(prev,id.vars='Sector')

#subset to get employment count tables
counts.prev = prev.rs[which(prev.rs$variable=='count92'|prev.rs$variable=='count02'|prev.rs$variable=='pred.count12'),]
counts.curr = curr.rs[which(curr.rs$variable=='count02'|curr.rs$variable=='count12'|curr.rs$variable=='pred.count22'),]
#reshape both count tables to wide
c.curr.wide = reshape(counts.curr,idvar = 'Sector',direction = 'wide',timevar = 'variable')
c.prev.wide = reshape(counts.prev,idvar = 'Sector',direction = 'wide',timevar = 'variable')
#merge wide count tables by industry, including those with no match
counts = merge(c.prev.wide,c.curr.wide,by='Sector',all.x = TRUE)
#remove commas from character column, transmute to numeric
counts$value.count12 = as.numeric(gsub(",", "", counts$value.count12))
#error in predicted 2012 employment, percent difference
counts$err.count12 <- 100*(counts$value.pred.count12 - counts$value.count12)/counts$value.count12
#proportional difference in actual 2012 employment versus projection, normalized around 0.
counts$pr.count12 = (counts$value.count12/counts$value.pred.count12) - 1
#data frame of industries with proportional difference greater than 10%
counts.top.err = counts[which(abs(counts$pr.count12)>.10),]

#Plot of all industries percent difference
p = ggplot(data = counts,aes(x=counts$Sector,y=counts$err.count12)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=20,hjust=0.9,vjust=0.5))+
  ggtitle('Percent Difference By Industry, 2012 Predictions')
print(p)
#Plot of top proportional differences
p2 = ggplot(data = counts.top.err,aes(x=counts.top.err$Sector,y=counts.top.err$pr.count12)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=20,hjust=0.9,vjust=0.5)) +
  ggtitle('Highest Proportional Differences by Industry, 2012 Predictions')
print(p2)


library("readxl", lib.loc="~/R/win-library/3.4")

setwd("z:/UBUNTU/00 - Личные папки сотрудников/13 - Маркин/РПЖ/Сводные таблицы/r")
pc <- read.csv("z:/UBUNTU/00 - Личные папки сотрудников/13 - Маркин/РПЖ/Сводные таблицы/r/PC Serum.csv", header = T, sep = ",", check.names = F)
ph <- read.csv("z:/UBUNTU/00 - Личные папки сотрудников/13 - Маркин/РПЖ/Сводные таблицы/r/PH Serum.csv", header = T, sep = ",", check.names = F)

tt <- sapply(colnames(pc)[-1:-1], function(name) t.test(pc[name], ph[name]))
# ttl <- lapply(colnames(pc)[-1:-2], function(name) t.test(pc[name], ph[name]))
fdr_tt <-p.adjust(tt["p.value",], method = "BH")


mwt <- sapply(colnames(pc)[-1:-1], function(name) wilcox.test(pc[[name]], ph[[name]]))
# mwl <- lapply(colnames(pc)[-1:-2], function(name) wilcox.test(pc[[name]], ph[[name]]))
fdr_mwt <-p.adjust(mwt["p.value",], method = "BH")


write.csv(tt, "tt.csv")
write.csv(mwt, "mwt.csv")
write.csv(fdr_tt, "fdr_tt.csv")
write.csv(fdr_mwt, "fdr_mwt.csv")

tt[,1:3]
colnames(tt)
tt[1,]$"Pyruvic acid"

ttl[1:3]
ttl[[1]]$statistic

###################################################
# setwd("E:/4. METABOLOMICS/Manuscript/manuscript thesis/statistics")
# 
# pre <- read.csv("pre.csv")
# post <- read.csv("post.csv")
# 
# prepost=merge(pre, post, by.x="id",by.y="id",all.x=T)
# prepost_target=prepost[,c(1:172,429:591)]
# prepost_untarget=prepost[,c(1:9,174:420,593:839)]
# 
# ttestpvalue_target=vector(length=163)
# tteststat_target=vector(length=163)
# metabolite_target=vector(length=163)
# wilcoxpvalue_target=vector(length=163)
# 
# 
# for(i in 10:172){
# 
# a=t.test(prepost_target[,i],prepost_target[,i+163],paired=T) 
# ttestpvalue_target[i-9]=a$p.value 
# tteststat_target[i-9]=a$statistic #if >0 pre > post, otherwise post > pre
# 
# b=wilcox.test(prepost_target[,i],prepost_target[,i+163], paired=T)
# 
# wilcoxpvalue_target[i-9]=b$p.value
# 
# metabolite_target[i-9]=names(prepost_target)[i]
# }
# ttestpvaluedata_target=data.frame(ttestpvalue_target,tteststat_target,metabolite_target)
# wilcoxpvaluedata_target=data.frame(wilcoxpvalue_target,metabolite_target)
# 
# ttestpvaluedata_target.o=ttestpvaluedata_target[order(ttestpvaluedata_target$ttestpvalue_target),]
# wilcoxpvaluedata_target.o=wilcoxpvaluedata_target[order(wilcoxpvaluedata_target$wilcoxpvalue_target),]
# 
# ttestpvaluedata_target.o$k=seq(1:163) 
# wilcoxpvaluedata_target.o$k=seq(1:163)
# 
# ttestpvaluedata_target.o$fdr=(ttestpvaluedata_target.o$k*0.05)/163
# wilcoxpvaluedata_target.o$fdr=(wilcoxpvaluedata_target.o$k*0.05)/163
# 
# ttestpvaluedata_target.o$diff=ttestpvaluedata_target.o$ttestpvalue_target-ttestpvaluedata_target.o$fdr
# wilcoxpvaluedata_target.o$diff=wilcoxpvaluedata_target.o$wilcoxpvalue_target-wilcoxpvaluedata_target.o$fdr
# 
# table(is.element(ttestpvaluedata_target.o$metabolite_target[ttestpvaluedata_target.o$diff < 0],wilcoxpvaluedata_target.o$metabolite_target[wilcoxpvaluedata_target.o$diff<0]))
# ttest_targetmetabolites=ttestpvaluedata_target.o$metabolite_target[ttestpvaluedata_target.o$diff<0]
# wilcox_targetmetabolites=wilcoxpvaluedata_target.o$metabolite_target[wilcoxpvaluedata_target.o$diff<0]
# 
# table(is.element(ttest_targetmetabolites,wilcox_targetmetabolites))
# table(is.element(wilcox_targetmetabolites,ttest_targetmetabolites))
# 
# ttest_targetmetabolites[!is.element(ttest_targetmetabolites,wilcox_targetmetabolites)]
# wilcox_targetmetabolites[!is.element(wilcox_targetmetabolites,ttest_targetmetabolites)]
# 
# write.csv(ttestpvaluedata_target.o,"ttestpvaluedata_target.csv",row.names=FALSE)
# write.csv(wilcoxpvaluedata_target.o,"wilcoxpvaluedata_target.csv",row.names=FALSE)



#untargeted metabolite

# ttestpvalue_untarget=vector(length=247)
# tteststat_untarget=vector(length=247)
# metabolite_untarget=vector(length=247)
# wilcoxpvalue_untarget=vector(length=247)
# 
# for(i in 10:256){
# 
# a=t.test(prepost_untarget[,i],prepost_untarget[,i+247],paired=T)
# ttestpvalue_untarget[i-9]=a$p.value
# tteststat_untarget[i-9]=a$statistic # if >0 pre>post, otherwise post>pre
# 
# b=wilcox.test(prepost_untarget[,i],prepost_untarget[,i+247], paired=T)
# 
# wilcoxpvalue_untarget[i-9]=b$p.value
# 
# metabolite_untarget[i-9]=names(prepost_untarget)[i]
# }
# ttestpvaluedata_untarget=data.frame(ttestpvalue_untarget,tteststat_untarget,metabolite_untarget)
# wilcoxpvaluedata_untarget=data.frame(wilcoxpvalue_untarget,metabolite_untarget)
# 
# ttestpvaluedata_untarget.o=ttestpvaluedata_untarget[order(ttestpvaluedata_untarget$ttestpvalue_untarget),]
# wilcoxpvaluedata_untarget.o=wilcoxpvaluedata_untarget[order(wilcoxpvaluedata_untarget$wilcoxpvalue_untarget),]
# 
# ttestpvaluedata_untarget.o$k=seq(1:247)
# wilcoxpvaluedata_untarget.o$k=seq(1:247)
# 
# ttestpvaluedata_untarget.o$fdr=(ttestpvaluedata_untarget.o$k*0.05)/247
# wilcoxpvaluedata_untarget.o$fdr=(wilcoxpvaluedata_untarget.o$k*0.05)/247
# 
# ttestpvaluedata_untarget.o$diff=ttestpvaluedata_untarget.o$ttestpvalue_untarget-ttestpvaluedata_untarget.o$fdr
# wilcoxpvaluedata_untarget.o$diff=wilcoxpvaluedata_untarget.o$wilcoxpvalue_untarget-wilcoxpvaluedata_untarget.o$fdr

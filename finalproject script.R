library(yacca)
library(foreign)

studentperformance.data<-read.spss("C:\\Users\\johnh\\Documents\\EDPS 742\\studentperformancedataset.sav", to.data.frame=TRUE, use.value.labels=FALSE)

#PREPARE DATA FILES#
studentperformance.data.nomiss<-na.omit(studentperformance.data)
yvar.list<-c("anxiety_level", "self_esteem", "depression")
studentperformance.yvars<-studentperformance.data.nomiss[yvar.list]

xvar.list<-c("academic_performance", "stress_level")
studentperformance.xvars<-studentperformance.data.nomiss[xvar.list]

#CANONICAL CORRELATION#
studentperformance.canonical<-cca(studentperformance.yvars,studentperformance.xvars)
studentperformance.canonical$corr
studentperformance.canonical$corrsq
studentperformance.canonical$ystructcorr
studentperformance.canonical$xstructcorr
studentperformance.canonical$xcrosscorr
studentperformance.canonical$ycrosscorr
studentperformance.canonical$xvrd
studentperformance.canonical$yvrd
studentperformance.canonical$xrd
studentperformance.canonical$yrd
studentperformance.canonical$xcanvad
studentperformance.canonical$ycanvad
F.test.cca(studentperformance.canonical)


#MULTIVARIATE REGRESSION#
studentperformance.reg<-lm(cbind(academic_performance,stress_level)~anxiety_level+self_esteem+depression, data=studentperformance.data.nomiss)
anova(studentperformance.reg)
summary(studentperformance.reg)



dotplot(ranef(studentperformance.reg))

qqmath(ranef(studentperformance.reg))
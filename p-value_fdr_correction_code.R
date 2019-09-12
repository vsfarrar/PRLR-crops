cr.pval<-summary(cr.rob)$coefficients[,4] 
cr.p<-round(p.adjust(cr.pval, method="fdr"), 3)

cr$Stage<-relevel(cr$Stage, ref="Inc_D3")
m1.cr<-lmRob(log.exp.neg~Stage+Sex, data=cr)
m1.cr.p<-summary(m1.cr)$coefficients[,4] 
cr.p.d3<-round(p.adjust(m1.cr.p, method="fdr"), 3)

cr$Stage<-relevel(cr$Stage, ref="Inc_D9")
m2.cr<-lmRob(log.exp.neg~Stage+Sex, data=cr)
m2.cr.p<-summary(m2.cr)$coefficients[,4] 
cr.p.d9<-round(p.adjust(m2.cr.p, method="fdr"), 3)

cr$Stage<-relevel(cr$Stage, ref="Hatch")
m3.cr<-lmRob(log.exp.neg~Stage+Sex, data=cr)
m3.cr.p<-summary(m3.cr)$coefficients[,4] 
cr.p.htch<-round(p.adjust(m3.cr.p, method="fdr"), 3)

print(cr.p)
print(cr.p.d3)
print(cr.p.d9)
print(cr.p.htch)
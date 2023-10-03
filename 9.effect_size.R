################################################################################
################################################################################
#                            SCRIPT 9 - EFFECT SIZE
################################################################################
################################################################################

#FMestre
#03-10-2023

#Loading packages
library(effectsize)

#using
#fractions_top_intermediate_basal_nodes

dados <- fractions_top_intermediate_basal_nodes

##calculating effect size, base don Cohen's D

#Top Level
effectsize::cohens_d(dados$AFTER__top_level,dados$BEFORE_top_level,na.rm=T)

#Intermediate Level
effectsize::cohens_d(dados$AFTER_Interm_level,dados$BEFORE_Interm_level,na.rm=T)

#Basal Level
effectsize::cohens_d(dados$AFTER_basal_level,dados$BEFORE_basal_level,na.rm=T)

## ploting effect size (95%CI)
par(mar=c(4,8,1,1))
x=c(1,2,3)

#Values for point estimate and CI
avg=c(0.03,0.04,-0.04)
lower=c(-0.01,-0.01,-0.08)
upper=c(0.07,0.08,-0.00)

#Plot
plot(1, type="n", ylab="", yaxt="n", xlab="Cohen's d (IC95%)", xlim=c(-0.5, .5), ylim=c(0, 4), main="")
points(avg,x, pch=16,cex=1)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Top","Intermediate", "Basal"),cex.lab=14, las=2,xlim=c(0.67,0.83))


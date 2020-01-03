library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

# Cutoff.
breaks = c(0, median(bilcirr$alb), 5)
bilcirr$albgrp = cut(bilcirr$alb, breaks=breaks, labels=1:(length(breaks) - 1))
boxplot(alb~albgrp, data=bilcirr, xlab="Albumin group", ylab="Albumin level")

##### Nelson-Aalen plots #####

surv.albgrp = survfit(coxph(Surv(time=years, event=status==1)~strata(albgrp), data=bilcirr))
# Plot cumulative hazard.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/nelson_aalen_albugrp_marginal.pdf")
plot(
  surv.albgrp, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:3,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("topleft", c("Low-level", "High-level"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

##### Kaplan-Meier plots #####

fit.albgrp <- survfit(Surv(time=years, event=status)~strata(albgrp), data=bilcirr)
# Plot survival curve.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/kapmaier_albugrp_marginal.pdf")
plot(
  fit.albgrp, xlab="Years since treatment", ylab="Survival",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomleft", c("Low-level", "High-level"), lty=1:2, lwd=2, 
       col=c("black", "grey"))
dev.off()

summary(fit.albgrp)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                     time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# < 3.55 albu (5y):   5.270     51       1    0.539 0.04266        0.462        0.630
# < 3.55 albu (10y):  10.511      7       1    0.195 0.05736        0.110        0.347
# >= 3.55 albu (5y):  5.002    106       1    0.861 0.02910        0.806        0.920
# >= 3.55 albu (10y): 10.300     22       1    0.592 0.06355        0.479        0.730

##### Median survival time with CI #####
fit.albgrp
#                   n events median 0.95LCL 0.95UCL
# strata(albgrp)=1 157     85   6.18    4.63    7.79
# strata(albgrp)=2 155     40  11.47   10.30      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~albgrp, data=bilcirr)
#           N Observed Expected (O-E)^2/E (O-E)^2/V
# albgrp=1 157       85     49.2      26.1      44.4
# albgrp=2 155       40     75.8      16.9      44.4
# Chisq= 44.4  on 1 degrees of freedom, p= 3e-11 

library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

##### Nelson-Aalen plots #####

surv.asc = survfit(coxph(Surv(time=years, event=status==1)~strata(asc), data=bilcirr))
# Plot cumulative hazard.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/nelson_aalen_asc_marginal.pdf")
plot(
  surv.asc, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:2,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("topleft", c("Absent ascites","Present ascites"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

##### Kaplan-Meier plots #####

fit.asc <- survfit(Surv(time=years, event=status)~strata(asc), data=bilcirr)
# Plot survival curve.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/kapmaier_asc_marginal.pdf")
plot(
  fit.asc, xlab="Years since treatment", ylab="Survival",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomleft", c("Absent","Present"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

summary(fit.asc)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# Absent (5y):  5.002    156       1    0.752 0.02682        0.701        0.807
# Absent (10y): 10.300     30       1    0.469 0.04705        0.386        0.571
# Present (5y):    5.697      3       1   0.1111  0.0680      0.03346        0.369
# Present (10y):   NA       

##### Median survival time with CI #####
#                     n events median 0.95LCL 0.95UCL
# strata(sex)=sex=0 276    103   9.39    8.68      NA
# strata(sex)=sex=1  36     22   6.53    3.55      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~asc, data=bilcirr)
#                 N Observed Expected (O-E)^2/E (O-E)^2/V
# asc=0 288      102   121.28      3.06       104
# asc=1  24       23     3.72     99.91       104
# Chisq= 104  on 1 degrees of freedom, p=<2e-16 

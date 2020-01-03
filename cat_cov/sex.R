library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

##### Nelson-Aalen plots #####

surv.sex = survfit(coxph(Surv(time=years, event=status==1)~strata(sex), data=bilcirr))
# Plot cumulative hazard.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/nelson_aalen_sex_marginal.pdf")
plot(
  surv.sex, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:2,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomright", c("Males", "Females"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

##### Kaplan-Meier plots #####

fit.sex <- survfit(Surv(time=years, event=status)~strata(sex), data=bilcirr)
# Plot survival curve.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/kapmaier_sex_marginal.pdf")
plot(
  fit.sex, xlab="Years since treatment", ylab="Survival",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomleft", c("Males", "Females"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

summary(fit.sex)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# Females (5y):  5.002    145       1    0.730 0.02794        0.677        0.787
# females (10y): 10.300     24       1    0.430 0.04983        0.343        0.540
# Males (5y):    6.533     12       1    0.479  0.0904       0.3306        0.693
# Males (10y):   11.168      5       1    0.274  0.0964       0.1376        0.546

##### Median survival time with CI #####
#                     n events median 0.95LCL 0.95UCL
# strata(sex)=sex=0 276    103   9.39    8.68      NA
# strata(sex)=sex=1  36     22   6.53    3.55      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~sex, data=bilcirr)
#         N Observed Expected (O-E)^2/E (O-E)^2/V
# sex=0 276      103    110.4     0.494      4.27
# sex=1  36       22     14.6     3.728      4.27
# Chisq= 4.3  on 1 degrees of freedom, p= 0.04 

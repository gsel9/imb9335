library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

# Cutoff: Low and high risk groups.
bilcirr$agegrp = as.numeric(bilcirr$age > 30 & bilcirr$age < 60)

##### Nelson-Aalen plots #####

surv.agegrp = survfit(coxph(Surv(time=years, event=status==1)~strata(agegrp), data=bilcirr))
# Plot cumulative hazard.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/nelson_aalen_agegrp_marginal.pdf")
plot(
  surv.agegrp, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:2,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("topleft", c("Low-risk", "High-risk"), lty=1:2, lwd=2, 
       col=c("black", "grey"))
dev.off()

##### Kaplan-Meier plots #####

fit.agegrp <- survfit(Surv(time=years, event=status==1)~strata(agegrp), data=bilcirr)
# Plot survival curve.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/kapmaier_agegrp_marginal.pdf")
plot(
  fit.agegrp, xlab="Years since treatment", ylab="Survival",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomleft", c("Low-risk", "High-risk"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

summary(fit.agegrp)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                   time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# Low-risk (5y):   5.722     22       1    0.520  0.0686       0.4013        0.673
# Low-risk (10y):  NA
# High-risk (5y):  5.002    136       1    0.746 0.02869        0.691        0.804
# High-risk (10y):  10.300     28       1    0.482 0.04882        0.395        0.588

##### Median survival time with CI #####
fit.agegrp
#                          n events median 0.95LCL 0.95UCL
# strata(agegrp)=agegrp=0  60     34   6.85    4.07    9.81
# strata(agegrp)=agegrp=1 252     91   9.43    8.88      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status==1)~agegrp, data=bilcirr)
#                     N Observed Expected (O-E)^2/E (O-E)^2/V
# agegrp=0 115       47     41.1     0.835      1.26
# agegrp=1 197       78     83.9     0.410      1.26
# Chisq= 1.3  on 1 degrees of freedom, p= 0.3 

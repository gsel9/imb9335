library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

##### Nelson-Aalen plots #####

surv.treat = survfit(coxph(Surv(time=years, event=status)~strata(treat), 
                           data=bilcirr))
# Plot cumulative hazard.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/nelson_aalen_treat_marginal.pdf")
plot(
  surv.treat, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:2,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomright", c("D-penicillamine","Placebo"), lty=1:2, lwd=2, 
       col=c("black", "grey"))
dev.off()

##### Kaplan-Meier plots #####

fit.treat <- survfit(Surv(time=years, event=status)~strata(treat), data=bilcirr)
# Plot survival curve.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/kapmaier_treat_marginal.pdf")
plot(
  fit.treat, xlab="Years since treatment", ylab="Survival",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE,
  cex.lab=1.6, cex.axis=1.6, cex.sub=1.6
)
legend("bottomleft", c("D-penicillamine","Placebo"), lty=1:2, lwd=2, col=c("black", "grey"))
dev.off()

summary(fit.treat)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                        time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# D-penicillamine (5y):  5.002     82       1    0.699 0.03845        0.628        0.779       0.787
# D-penicillamine (10y): 11.168      8       1    0.372 0.07247        0.254        0.545
# Placebo (5y):          5.057     76       1    0.705 0.03829        0.634        0.784
# Placebo (10y):         10.300     15       1    0.427 0.06427        0.318        0.573

##### Median survival time with CI #####
fit.treat
#                        n events median 0.95LCL 0.95UCL
# strata(treat)=treat=1 158     65   8.99    7.07      NA
# strata(treat)=treat=2 154     60   9.39    8.46      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~treat, data=bilcirr)
#                   N Observed Expected (O-E)^2/E (O-E)^2/V
# treat=1 158       65     63.2    0.0502     0.102
# treat=2 154       60     61.8    0.0513     0.102
# Chisq= 0.1  on 1 degrees of freedom, p= 0.7 

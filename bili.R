# NOTE: Rising level of bilirubin is an indicator of the prognosis of PBC.
# REF: https://www.medicinenet.com/primary_biliary_cirrhosis_pbc/article.htm#what_are_the_signs_and_symptoms_for_primary_biliary_cirrhosis

library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

# Cutoff.
breaks = c(0, quantile(bilcirr$bil, 0.75), Inf)
bilcirr$bilgrp = cut(bilcirr$bil, breaks=breaks, labels=1:(length(breaks) - 1))
boxplot(bil~bilgrp, data=bilcirr, xlab="Bilirubin group", ylab="Bilirubin level")

# lower quartile corresponds 0.75 %.
plot(sort(bilcirr$bil))
abline(h=quantile(bilcirr$bil, 0.75))

##### Nelson-Aalen plots #####

surv.bilgrp = survfit(coxph(Surv(time=years, event=status==1)~strata(bilgrp), data=bilcirr))
# Plot cumulative hazard.
plot(
  surv.bilgrp, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:3
)
legend("topleft", c("Low-level", "High-level"), lty=1:2, lwd=2, col=c("black", "grey"))

##### Kaplan-Meier plots #####

fit.bilgrp <- survfit(Surv(time=years, event=status)~strata(bilgrp), data=bilcirr)
# Plot survival curve.
plot(
  fit.bilgrp, xlab="Years since treatment", ylab="Cumulative hazard",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE
)
legend("bottomleft",  c("Low-level", "High-level"), lty=1:2, lwd=2, 
       col=c("black", "grey"))

summary(fit.bilgrp)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# Low-level (5y):   5.057    143       1    0.839 0.02522        0.791        0.890
# Low-level (10y):  10.300     29       1    0.529 0.05261        0.436        0.643
# High-level (5y):  5.002     14       1   0.2647  0.0561       0.1746        0.401
# High-level (10y): NA

##### Median survival time with CI #####
fit.bilgrp
#                   n events median 0.95LCL 0.95UCL
# strata(bilgrp)=1 234     67   11.2    9.39      NA
# strata(bilgrp)=2  78     58    3.2    2.58    3.91

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~bilgrp, data=bilcirr)
#           N Observed Expected (O-E)^2/E (O-E)^2/V
# bilgrp=1 234       67    107.9      15.5       119
# bilgrp=2  78       58     17.1      97.9       119
# Chisq= 119  on 1 degrees of freedom, p= <2e-16 

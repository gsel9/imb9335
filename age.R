# NOTE: Primary biliary cholangitis is most likely to occur at age 40 to 60.
# REF: https://www.medicinenet.com/primary_biliary_cirrhosis_pbc/article.htm
# REF: https://www.liver.ca/patients-caregivers/liver-diseases/primary-biliary-cholangitis/

library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

# Cutoff: Low and high risk groups.
bilcirr$agegrp = as.numeric(bilcirr$age > 40 & bilcirr$age < 60)

##### Nelson-Aalen plots #####

surv.agegrp = survfit(coxph(Surv(time=years, event=status==1)~strata(agegrp), data=bilcirr))
# Plot cumulative hazard.
plot(
  surv.agegrp, fun="cumhaz", mark.time=F, col=c("black", "grey"), lwd=2,
  xlab="Years since treatment", ylab="Cumulative hazard", lty=1:2
)
legend("topleft", c("Low-risk", "High-risk"), lty=1:2, lwd=2, 
       col=c("black", "grey"))

##### Kaplan-Meier plots #####

fit.agegrp <- survfit(Surv(time=years, event=status)~strata(agegrp), data=bilcirr)
# Plot survival curve.
plot(
  fit.agegrp, xlab="Years since treatment", ylab="Cumulative hazard",
  col=c("black", "grey"), lty=1:2, lwd=2, mark.time=FALSE
)
legend("bottomleft", c("Low-risk", "High-risk"), lty=1:2, lwd=2, col=c("black", "grey"))

summary(fit.agegrp)
# Estimates and 95 % confidence intervals for the survival after 5 and 10 years.
#                time n.risk n.event survival std.err lower 95% CI upper 95% CI           
# Low-risk (5y):   5.057     49       1    0.662 0.04779        0.575        0.763
# Low-risk (10y):  NA
# High-risk (5y):  5.002    110       1    0.723 0.03300        0.661        0.790
# High-risk (10y): 10.300     23       1    0.475 0.05226        0.383        0.590

##### Median survival time with CI #####
fit.agegrp
#                          n events median 0.95LCL 0.95UCL
# strata(agegrp)=agegrp=0 115     47   8.82    6.95      NA
# strata(agegrp)=agegrp=1 197     78   9.43    8.68      NA

##### Logrank test #####

# H0: the two groups have identical hazard functions. Keep H0 if p > alpha.
survdiff(Surv(time=years, event=status)~agegrp, data=bilcirr)
#                     N Observed Expected (O-E)^2/E (O-E)^2/V
# agegrp=0 115       47     41.1     0.835      1.26
# agegrp=1 197       78     83.9     0.410      1.26
# Chisq= 1.3  on 1 degrees of freedom, p= 0.3 

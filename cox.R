# NOTE: The Cox model assumes that each variable makes a linear contribution to the model.
library(survival)

bilcirr = read.table("http://folk.uio.no/borgan/IMB9335/bilcirr.txt", header=T)

# Representing after event in years. 
bilcirr$years = bilcirr$days / 365.25

# Effect of all covariates. Covariates where p < alpha are significant.
cox.fit = coxph(Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + age + bil + alb,
                data=bilcirr)
summary(cox.fit)


######## Log-linearity assumption for numeric covariates ######## 
# Check if non-linear parts are insignificant (i.e p-value > alpha)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_ageterm.pdf")
cox.spage = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + pspline(age) + bil + alb,
  data=bilcirr
)
termplot(cox.spage, se=T, terms=4, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()
print(cox.spage)

bilcirr$log2age = log2(bilcirr$age)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_log2ageterm.pdf")
cox.splog2age = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + pspline(log2age) + bil + alb,
  data=bilcirr
)
termplot(cox.splog2age, se=T, terms=4, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()
print(cox.splog2age)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_bilterm.pdf")
cox.spbil = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + age + pspline(bil) + alb,
  data=bilcirr
)
termplot(cox.spbil, se=T, terms=5, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()
print(cox.spbil)

bilcirr$log2bil = log2(bilcirr$bil)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_log2bilterm.pdf")
cox.splog2bil = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + pspline(log2bil) + age + alb,
  data=bilcirr
)
termplot(cox.splog2bil, se=T, terms=4, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()
print(cox.splog2bil)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_albterm.pdf")
cox.spalb = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + age + bil + pspline(alb),
  data=bilcirr
)
termplot(cox.spalb, se=T, terms=6, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
print(cox.spalb)
dev.off()



######## The proportional hazards assumption ######## 
# Schoenfeld residuals, a chi-square, and the two-sided p-value.
cox.fit = coxph(Surv(years, status)~factor(treat) + factor(sex) + log2bil + log2age + alb + factor(asc), 
                data=bilcirr)
cox.zph(cox.fit, transform='log')

# p-value >= alpha test is not statistically significant for each of the 
# covariates, and the global test is also not statistically significant. 
# Therefore, we can assume the proportional hazards.
# If p-value < alpha for covariate: may have time-dependent effects.



######## Backwards elimination ######## 
# P-value < alpha => covariate is significant.
cox.fit = coxph(Surv(years, status)~factor(treat) + factor(sex) + log2bil + log2age + alb + factor(asc),
                data=bilcirr)
summary(cox.fit)
# not significant: treat, sex

cox.fit = coxph(Surv(years, status)~factor(treat) + log2bil + log2age + alb + factor(asc),
                data=bilcirr)
summary(cox.fit)
# not significant: treat

cox.fit = coxph(Surv(years, status)~log2bil + log2age + alb + factor(asc), data=bilcirr)
summary(cox.fit)



######## survival curves ######## 

surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(asc == 0),
    data=bilcirr
  )
)
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival")


surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(asc == 1),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival")

surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(log2age > 5.5),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival")

surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(log2bil > 2),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival")

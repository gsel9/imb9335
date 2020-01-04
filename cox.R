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
print(cox.spage)
dev.off()

bilcirr$log2age = log2(bilcirr$age)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_log2ageterm.pdf")
cox.splog2age = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + pspline(log2age) + bil + alb,
  data=bilcirr
)
termplot(cox.splog2age, se=T, terms=4, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
print(cox.splog2age)
dev.off()


pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_bilterm.pdf")
cox.spbil = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + age + pspline(bil) + alb,
  data=bilcirr
)
termplot(cox.spbil, se=T, terms=5, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
print(cox.spbil)
dev.off()


bilcirr$log2bil = log2(bilcirr$bil)

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/cox_log2bilterm.pdf")
cox.splog2bil = coxph(
  Surv(years, status)~factor(treat) + factor(sex) + factor(asc) + pspline(log2bil) + age + alb,
  data=bilcirr
)
termplot(cox.splog2bil, se=T, terms=4, cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
print(cox.splog2bil)
dev.off()


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

# Most likely to occur in individuals from 30 to 60 years old. Compare age
# in [30, 60] to otherwise.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_age30to60.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(2 ** log2age  >= 30 & 2 ** log2age <= 60),
    data=bilcirr
  )
)
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_not_age30to60.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=!(2 ** log2age  >= 30 & 2 ** log2age <= 60),
    data=bilcirr
  )
)
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()


# A decreasing level of albumin is associated with worse prognosis in advanced PBC.
# Compare high- and low-levels of albumin.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_low_alb.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(alb < 3.55),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_high_alb.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(alb > 3.55),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()


# A rising level of bilirubin is an indicator of the prognosis of PBC.
pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_high_bili.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(2 ** log2bil > 3.55),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()

pdf("/Users/severinlangberg/Desktop/phd/survival_analysis/exam/figures/survcurve_low_bili.pdf")
surv.ss = survfit(
  coxph(
    Surv(years, status)~log2bil + log2age + alb + factor(asc), 
    subset=(2 ** log2bil <= 3.55),
    data=bilcirr
  )
) 
plot(surv.ss, mark.time=FALSE, xlab="Years",  ylab="Survival", 
     cex.lab=1.6, cex.axis=1.6, cex.sub=1.6)
dev.off()

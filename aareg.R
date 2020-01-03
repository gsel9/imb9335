# Aalen additive hazards regression.
# SEE: https://en.wikipedia.org/wiki/Survival_function


# Center the numeric covariates.
bilcirr$albCent = bilcirr$alb - mean(bilcirr$alb)
bilcirr$log2ageCent = bilcirr$log2age - mean(bilcirr$log2age)
bilcirr$log2bilCent = bilcirr$log2bil - mean(bilcirr$log2bil)

aalen <- aareg(Surv(years, status)~log2bilCent + log2ageCent + albCent + factor(asc),
               data=bilcirr)
print(aalen)

par(mfrow=c(2, 3))
plot(aalen)
par(mfrow=c(1, 1))

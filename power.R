### time to event sample size calculation

library(gsDesign)

control.convert = c(0.25, 0.2, 0.17)
treat.convert = c(0.02, 0.05, 0.08)

lambda.c = -log(1-control.convert)
lambda.t = -log(1-treat.convert)

lambda.t %*% t(1/lambda.c)

ss <- nSurvival(lambda1=lambda.t[1], lambda2=lambda.c[1],
                eta = 0, # drop out rate
                Ts = 2, # enrollment + follow up
                Tr = 1, # enrollment
                sided = 2, alpha = 0.05,  beta=0.2)
ss$n

for(i in lambda.c) for(j in lambda.t){
  cat(i, j,'\n')
  samp = nSurvival(lambda1=j, lambda2=i,
            eta = 0, # drop out rate
            Ts = 2, # enrollment + follow up
            Tr = 1, # enrollment
            sided = 2, alpha = 0.05,  beta=0.2)$n
  cat("sample", samp,"\n")
}


### two sample t test
power.t.test(n=91, # sample size per group
             power=0.9, sig.level = 0.05, type="two.sample")


power.t.test(delta=0.5, power=0.8, sig.level = 0.05, type="two.sample")


library(pwr)
pwr.t2n.test(n1=64, # two sample different sizes t tes
             n2=128, 
             power=0.8, sig.level = 0.05)

power.prop.test(power=0.9, p1=0.4, p2=0.55, sig.level=0.05, alternative = "two.sided")


pwr.2p2n.test(h=0.15, n1=780, power=0.9, sig.level = 0.05,alternative="two.sided")


library(EnvStats)
propTestN(p.or.p1 = 0.4, p0.or.p2 = 0.55, alpha=0.05, power=0.9, ratio = 1.5, sample.type = "two.sample", alternative = "two.sided")



## calculate sample size using hazard ratio

alpha=0.05
beta = 0.1
hr = 1.5 # hazard ratio

zalpha=qnorm(alpha)
zbeta=qnorm(beta)
num=4*(zalpha + zbeta)^2
denom = (log(hr))^2

num/denom



## 

hr=(-log(0.4)/5)/(-log(0.6)/5)



power.t.test(delta=5, sd=17, power=0.9, sig.level = 0.01, type="two.sample")

power.t.test(n=100, delta=5, sd=17, sig.level = 0.01, type="two.sample")

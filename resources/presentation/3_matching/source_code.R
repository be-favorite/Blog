#####################################
#IPTW; liver data
# 간암환자 1128명을 대상으로 새로운 치료법과 기존 치료법이 사망율에 미치는 영향을 비교하는 연구 수행
# Confounding factor 보정 후, 치료법의 효과 비교를 위해 IPTW 수행
library(tidyverse)
library(WeightIt)
library(survival)
library(cobalt)

dat.t <- read_csv('./3_matching/data/ps_matching_dat2.csv') |> 
    janitor::clean_names()
glimpse(dat.t)

# treatment: gr (1 = PBT, 0 = TACE)
# covariate:
# age	male	dm	ht	ctp_score	log_dna
# alt	log_alt	plt	alb	tbi inr	cr
# response variables: death, death_yrs (Survival data)
# 그외: `in-hospital mortality`
dat.t <- dat1
glimpse(dat.t)

# Balancing covariates between treatment groups (binary)
W1 <- weightit(gr ~ . -death-death_yrs-in_hospital_mortality, data = dat.t,
               method = "ps", estimand = "ATE",stabilize = TRUE)
W1
dat.t$iptw2 <- W1$weights

# Modeling
f1 <- coxph(Surv(death_yrs,death)~gr, weight=iptw2, data=dat.t)
summary(f1)
f2 <- glm(in_hospital_mortality ~ gr,weight=iptw2,data=dat.t,family=binomial)
summary(f2)


# Balace check-1
bal.fit <- bal.tab(W1,stats="mean.diffs", un = TRUE, m.threshold = .2)
bal.fit

unadj.bal <- bal.fit$Balance[2:13, 2]
adj.bal <- bal.fit$Balance[2:13, 3]
plot(c(1,2),c(-1,1),type='n',xlab='',ylab='Standardized Mean Difference',
     main='Covariate Balance',xaxt='n')
pp <- length(unadj.bal)
points(rep(1.2,pp),unadj.bal)
points(rep(1.8,pp),adj.bal)
for ( jj in 1:pp) {
    lines(c(1.2,1.8),c(unadj.bal[jj],adj.bal[jj]),type='l')
}
abline(h=0.2,col=2,lty=2)
abline(h=-0.2,col=2,lty=2)
axis(1, at=c(1.2,1.8), labels=c('Unweighted','Weighted'))

#balace check-2
bal.plot(W1,"age", which = "both")
bal.plot(W1,"dm", which = "both")

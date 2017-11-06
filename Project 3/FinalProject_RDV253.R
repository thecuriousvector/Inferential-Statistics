
midata <- read.csv("magnesium.csv")[3:8]


############################# FREQUENTIST ANALYSIS #########################################

##### FIXED EFFECTS ####
library(metafor)
library(rmeta)

trialnames8 <- c("Morton","Rasmussen","Smith","Abraham","Feldstedt","Schechter",
                 "Ceremuzynski","LIMIT-2")
fixed8 <- midata[midata$trialnam %in% trialnames8, 
                 c('trialnam','dead1','tot1','dead0','tot0')]
peto8 <- rma.peto(ai=dead1, ci=dead0, n1i=tot1, n2i=tot0, data=fixed8)



trialnames14 <- c("Morton","Rasmussen","Smith","Abraham","Feldstedt","Schechter",
           "Ceremuzynski","LIMIT-2","Bertschat","Singh","Pereira", "Golf",
           "Thogersen","Schechter 2" )
fixed14 <- midata[midata$trialnam %in% trialnames14,
                  c('trialnam','dead1','tot1','dead0','tot0')]
peto14 <- rma.peto(ai=dead1, ci=dead0, n1i=tot1, n2i=tot0, data=fixed14)



trialnames15 <- c("Morton","Rasmussen","Smith","Abraham","Feldstedt","Schechter",
           "Ceremuzynski","LIMIT-2","Bertschat","Singh","Pereira", "Golf",
           "Thogersen","Schechter 2","ISIS-4")
fixed15 <- midata[midata$trialnam %in% trialnames15,
                  c('trialnam','dead1','tot1','dead0','tot0')]
peto15 <- rma.peto(ai=dead1, ci=dead0, n1i=tot1, n2i=tot0, data=fixed15)


fixed8
fixed14
fixed15

### Random Effects ###
random8 <- meta.DSL(ntrt=tot1, nctrl=tot0, ptrt=dead1, pctrl=dead0,  data=fixed8)
random14 <- meta.DSL(ntrt=tot1, nctrl=tot0, ptrt=dead1, pctrl=dead0,  data=fixed14)
random15 <- meta.DSL(ntrt=tot1, nctrl=tot0, ptrt=dead1, pctrl=dead0,  data=fixed15)

#### Table 2 #####
library(xtable)

print(xtable(fixed8),include.rownames=F)
paste("Fixed effect (Peto) meta-analysis of above eight trials: OR = ",
      round(exp(peto8$b),3), "(95% CI:",
      round(exp(peto8$ci.lb),3),",",round(exp(peto8$ci.ub),3),")")
paste("Random effect (D-L) meta-analysis of above eight trials: OR = ",
      round(exp(random8$logDSL),3), "(95% CI:",
      round(exp(random8$logDS - 1.96*random8$selogDSL),3),",",
      round(exp(random8$logDS + 1.96*random8$selogDSL),3),")")

print(xtable(fixed14[c(8:12,14),]),include.rownames=F)
paste("Fixed effect (Peto) meta-analysis of above fourteen trials: OR = ",
      round(exp(peto14$b),3), "(95% CI:",
      round(exp(peto14$ci.lb),3),",",round(exp(peto14$ci.ub),3),")")
paste("Random effect (D-L) meta-analysis of above fourteen trials: OR = ",
      round(exp(random14$logDSL),3), "(95% CI:",
      round(exp(random14$logDS - 1.96*random14$selogDSL),3),",",
      round(exp(random14$logDS + 1.96*random14$selogDSL),3),")")

print(xtable(fixed15[15,]),include.rownames=F)
paste("Fixed effect (Peto) meta-analysis of above fifteen trials: OR = ",
      round(exp(peto15$b),3), "(95% CI:",
      round(exp(peto15$ci.lb),3),",",round(exp(peto15$ci.ub),3),")")
paste("Random effect (D-L) meta-analysis of above fifteen trials: OR = ",
      round(exp(random15$logDSL),3), "(95% CI:",
      round(exp(random15$logDS - 1.96*random15$selogDSL),3),",",
      round(exp(random15$logDS + 1.96*random15$selogDSL),3),")")


### FOREST PLOTS ###
par(mfrow=c(1,3))
forest(peto8, main="Fixed Effect Model - 8 Trials", transf=exp)
forest(peto14, main="Fixed Effect Model - 14 Trials", transf=exp)
forest(peto15, main="Fixed Effect Model - 15 Trials", transf=exp)


############################### BAYESIAN ANALYSIS ###################################
library(rstan)

## Reference Prior ##

reference = "data{
int<lower=0> k;
int<lower=0> nc[k];
int<lower=0> nm[k];
int<lower=0> rc[k];
int<lower=0> rm[k];
}
parameters{
real<lower=0, upper=1> pc[k];
vector[k] delta;
real mu;
real<lower=0> sigma;
real deltanew;
}
transformed parameters{
real<lower=0, upper=1> pm[k];
for (i in 1:k) {
pm[i] = exp(log(pc[i]/(1-pc[i])) + delta[i]) / (1+exp(log(pc[i]/(1-pc[i])) + delta[i]));
}
}
model {
## Models ##
rc ~ binomial(nc,pc);
rm ~ binomial(nm,pm);
delta ~ normal(mu, sigma);
deltanew ~ normal(mu, sigma);
## Priors ##
pc ~ uniform(0,1);
mu ~ normal(0,100);
sigma ~ uniform(0,100);
}"


k <- nrow(dta)
nc <- dta$tot0
nm <- dta$tot1
rc <- dta$dead0
rm <- dta$dead1
refmodel <- stan(model_code = ref_string, data=c("k","nc","nm","rc","rm"),
               pars = "deltanew", iter=500000, chains=3, warmup=500, cores=3)


## Skeptical Prior ##

skeptical = "data{
int<lower=0> k;
int<lower=0> nc[k];
int<lower=0> nm[k];
int<lower=0> rc[k];
int<lower=0> rm[k];
}
parameters{
real<lower=0, upper=1> pc[k];
vector[k] delta;
real mu;
real<lower=0> sigma;
real deltanew;
}
transformed parameters{
real<lower=0, upper=1> pm[k];
for (i in 1:k) {
pm[i] = exp(log(pc[i]/(1-pc[i])) + delta[i]) / (1+exp(log(pc[i]/(1-pc[i])) + delta[i]));
}
}
model {
## Models ##
rc ~ binomial(nc,pc);
rm ~ binomial(nm,pm);
delta ~ normal(mu, sigma);
deltanew ~ normal(mu, sigma);
## Priors ##
pc ~ uniform(0,1);
mu ~ normal(0,0.175);
sigma ~ uniform(0,100);
}"

k <- nrow(dta)
nc <- dta$tot0
nm <- dta$tot1
rc <- dta$dead0
rm <- dta$dead1
skepmodel <- stan(model_code = skep_string, data=c("k","nc","nm","rc","rm"),
                pars = "deltanew", iter=500000, chains=3, warmup=500, cores=3)

refmodel
skepmodel


## Traceplots ##
par(mfrow=c(1,2))
traceplot(refmodel)
traceplot(skepmodel)


## Histograms ##
ref <- extract(refmodel)
skep <- extract(skepmodel)

par(mfrow=c(1,2))
hist(exp(refExt$deltanew)[exp(ref$deltanew)<4], 
     xlim=c(0,4), xlab = "Odds Ratio", main="Reference Prior")
hist(exp(skepExt$deltanew)[exp(skep$deltanew)<4], 
     xlim=c(0,4), xlab = "Odds Ratio", main="Skeptical Prior")


## Posterior Probabilities ##
mean(exp(ref$deltanew) < 1)
mean(exp(skep$deltanew) < 1)
mean(exp(ref$deltanew) < .9)
mean(exp(skep$deltanew) < .9)






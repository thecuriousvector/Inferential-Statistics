                           ## Tortoise and Hare Racing Problem ##

## 1b) Differnce in sample mean calculation
givenData <- read.csv("race.csv")
smeanHare <- mean(givenData$Hare)
smeanTortoise <- mean(givenData$Tortoise)
smeanDifference <- smeanHare - smeanTortoise

## 1d) Pooled Variance
varianceHare <- var(givenData$Hare)
varianceTortoise <- var(givenData$Tortoise)
n1 <- length(givenData$Hare)
n2 <- length(givenData$Tortoise)
pooledVariance <- ((varianceHare*(n1 - 1) + varianceTortoise*(n2 - 1))/(n1 + n2 - 2))*((1/n1)+ (1/n2))

## 1e) (i) Test Statistic and P Value
stdError <- sqrt(pooledVariance)
t <- (smeanHare - smeanTortoise)/(stdError)
pval <- 2*pt(t, (n1 + n2 - 2), lower.tail=TRUE)

########################################################################################################

## 2c) (ii) U Statistics
U_Hare <- 0
U_Tortoise <- 0
for (i in 1:n1) {
  for (j in 1:n2) {
    U_Hare <- U_Hare + (givenData$Hare[i] < givenData$Tortoise[j])
    U_Tortoise <- U_Tortoise + (givenData$Tortoise[j] < givenData$Hare[i])
  }
}
sigma <- sqrt((n1*n2*(n1 + n2 +1))/12)
mu <- (n1*n2)/2
z <- (U_Hare - mu)/sigma
pval_U <- 2*(1-(pnorm(z, lower.tail = TRUE)))

## 2c (iii) Wilcoxon Rank Sum Test ##
wilcox.test(givenData$Hare, givenData$Tortoise, exact = F, correct = F)


#########################################################################################################


## 3a) Generate 3000 Permuted Datasets ##
totalData <- c(givenData$Hare,givenData$Tortoise)
##Rows 1 to 10 contain the finishing times of Hares and 
##Rows 11 to 20 contain the finishing times of Tortoises


## 3b) Evaluation Of Difference In Means And Test Staticts ##
perm.totalData <- data.frame(n = NULL, Hare = NULL, Tortoise = NULL)
for (n in 1:3000) {
  HareSample <- sample(c(1:20),10,replace=F)
  TortSample <- c(1:20)[!(c(1:20) %in% HareSample)] 
  ##Includes only those data that are not included in HareSample
  
  perm.datasets <- data.frame(n = n, Hare = totalData[HareSample], Tortoise = totalData[TortSample])
  perm.totalData <- rbind(perm.totalData, perm.datasets)
  ##Combining the header with the dataset
}
rm(perm.datasets)
## As we have all the permuted datasets and header in perm.totalData delete the perm.datasets

## Evaluation Of Difference In Means And Test Statistics ##
perm.statistics <- data.frame(n = rep(NA, 3000), smeandiff = rep(NA, 3000), 
                              t = rep(NA, 3000), U_Hare = rep(NA, 3000), 
                              U_Tort = rep(NA, 3000), z = rep(NA, 3000), 
                              W_Hare = rep(NA, 3000), W_Tort = rep(NA, 3000))
for (n in 1:3000) {
  rowindex = perm.totalData$n
  tempdata <- perm.totalData[which(rowindex == n),]
  ## Assigning the permuted dataset for particular value of n in tempdata
  
  ##Reading the value of nth iteration into the column n of dataset
  perm.statistics$n[n] <- n
  
  ## Calculation of the mean difference
  perm.statistics$smeandiff[n] <- mean(tempdata$Hare) - mean(tempdata$Tortoise)
  
  ## Calculation of pooled variance for particular n value
  tempvar_Hare <- var(tempdata$Hare)
  tempvar_Tort <- var(tempdata$Tortoise)
  n1 <- length(tempdata$Hare)
  n2 <- length(tempdata$Tortoise)
  pooledvar <- ((tempvar_Hare*(n1 - 1) + tempvar_Tort*(n2 - 1))/(n1 + n2 - 2))*((1/n1)+ (1/n2))
  
  ## Calculation of t statistic
  perm.statistics$t[n] = perm.statistics$smeandiff[n]/sqrt(pooledvar)
  
  ## Calculation of U statistic
  perm.U_Hare <- 0
  perm.U_Tort <- 0
  for (i in 1:length(tempdata$Hare)){
    for (j in 1:length(tempdata$Tortoise)) {
      perm.U_Hare <- perm.U_Hare + (tempdata$Hare[i] < tempdata$Tortoise[j])
      perm.U_Tort <- perm.U_Tort + (tempdata$Tortoise[j] < tempdata$Hare[i])
    }
  }
  perm.statistics$U_Hare[n] <- perm.U_Hare
  perm.statistics$U_Tort[n] <- perm.U_Tort
  
  ## Calculation of z statistic
  perm.statistics$z[n] <- (perm.U_Hare - mu)/sigma
  
  ## Calculation of rank sum statistics 
  perm.statistics$W_Hare[n] <- sum(rank(c(tempdata$Hare,tempdata$Tortoise))[1:10])
  perm.statistics$W_Tort[n] <- sum(rank(c(tempdata$Hare,tempdata$Tortoise))[11:20])
}


## Shapiro-Wilk Test For Each Of The Above Distributions ##

## (i) Differnece In Sample Means
shapiro.test(perm.statistics$smeandiff)
        ## data:  perm.statistics$smeandiff
        ## W = 0.96409, p-value < 2.2e-16
        ## As p < 0.05 this distribution does not approximate to a normal one
Exp.SMeanDiff <- 0
Obs.SMeanDiff <- mean(perm.statistics$smeandiff)
varianceDis <- var(perm.statistics$smeandiff)
statistic.SMeanDiff <- (Obs.SMeanDiff - Exp.SMeanDiff)/sqrt(varianceDis/length(perm.statistics$smeandiff))
pval_SMeanDiff <- 2*pt(statistic.SMeanDiff, (length (perm.statistics$smeandiff)), lower.tail=FALSE)
## (ii) T Distribution
shapiro.test(perm.statistics$t)
        ## data:  perm.statistics$t
        ## W = 0.97498, p-value < 2.2e-16
        ## As p < 0.05 this distribution does not approximate to a normal one



## (iii) U_Hare Distribution
shapiro.test(perm.statistics$U_Hare)
        ## data:  perm.statistics$U_Hare
        ## W = 0.99902, p-value = 0.09157
        ## As p > 0.05 This distribution can be approximated to a normal one
Exp.UMean = 50
Obs.UMean = mean(perm.statistics$U_Hare)
statistic.Udis = (Obs.UMean -Exp.UMean)/sigma
pval_Udis = 2*(1-(pnorm(statistic.Udis, lower.tail = TRUE)))
        ## statistic.Udis
        ## [1] -0.0138083
        ## pval_Udis
        ## [1] 1.011017


## (iv) Z Distribution
shapiro.test(perm.statistics$z)
        ## data:  perm.statistics$z
        ## 0.99902, p-value = 0.09157
        ## As p > 0.05 As expected, this distribution is a normal one
Exp.ZMean = 0
Obs.Zmean = mean(perm.statistics$z)
Variance.ZMean = var(perm.statistics$z)
statisticz.Zdis = (Obs.Zmean -Exp.Zmean)/sqrt(Variance.ZMean)
pval_Zdis = 2*(1-(pnorm(statistic.Zdis, lower.tail = TRUE)))
##pval_Udis
##[1] 1.011017


## (v) W Statistic Distribution
shapiro.test(perm.statistics$W_Hare)
## data:  perm.statistics$W_Hare
## 0.99902, p-value = 0.09157
## As p > 0.05 This distribution is a normal one.

## Calculation of p values ##
totalData <- c(givenData$Hare,givenData$Tortoise, seq(1:20))
datamatrix <- matrix(totalData, nrow=20, ncol=2)
set.seed(12345)
pmat <- matrix(NA, nrow=3000, ncol=7)
for (n in 1:3000) {
  index = sample(datamatrix[,2])
  new.datamatrix = datamatrix[index]
  mean.di = mean(new.datamatrix[1:10])-mean(new.datamatrix[11:20])
  
  s1 = sd(new.datamatrix[1:10])
  s2 = sd(new.datamatrix[11:20])
  std <- sqrt((s1^2/10) + (s2^2/10))
  t.statis = mean.di/std
  
  new.datamatrix = matrix(new.datamatrix, nrow=10, ncol=2)
  ind.0 = matrix(NA, nrow=10, ncol=10)
  ind.1 = matrix(NA, nrow=10, ncol=10)
  for (i in 1:10) {
    for (j in 1:10) {
      ind.0[i,j]=ifelse(new.datamatrix[i,1]<new.datamatrix[j,2],1,0)
    }
  }
  u.0 = sum(ind.0)
  u.1 = 100 - u.0
  sd.u=sqrt(10*10*(10+10+1)/12)
  z.statis=(u.0-50)/sd.u
  
  w.statis = wilcox.test(new.datamatrix[,1], new.datamatrix[,2], exact=F, correct=F)
  wil.u.0 = w.statis$statistic
  wil.u.1 = 105 - w.statis$statistic
  
  pmat[n,1] = mean.di
  pmat[n,2] = t.statis
  pmat[n,3] = u.0
  pmat[n,4] = u.1
  pmat[n,5] = z.statis
  pmat[n,6] = wil.u.0
  pmat[n,7] = wil.u.1
  
}
par(mfrow=c(3,3))
hist(pmat[,1], main="Mean Difference")
hist(pmat[,2], main="t Statistic")
hist(pmat[,3], main="U Hare")
hist(pmat[,4], main="U Tortoise")
hist(pmat[,5], main="Z statistic")
hist(pmat[,6], main="Wilcox´s rank sum Tortoise")
hist(pmat[,7], main="Wilcox´s rank sum Hare")

true.value=c(-5.045642, -0.5550947, 81, 19, 2.34338, 19, 81)

par(mfrow=c(3,3))
hist(pmat[,1], main="Mean Difference")
abline(v=true.value[1], lty=2, col="red")
hist(pmat[,2], main="t Statistic")
abline(v=true.value[2], lty=2, col="red")
hist(pmat[,3], main="U Hare")
abline(v=true.value[3], lty=2, col="red")
hist(pmat[,4], main="U Tortoise")
abline(v=true.value[4], lty=2, col="red")
hist(pmat[,5], main="Z statistic")
abline(v=true.value[5], lty=2, col="red")
hist(pmat[,6], main="W Tortoise")
abline(v=true.value[6], lty=2, col="red")
hist(pmat[,7], main="W Hare")
abline(v=true.value[7], lty=2, col="red")


p.val1=sum(pmat[,1]>=true.value[1])/3000
p.val2=sum(pmat[,2]>=true.value[2])/3000
p.val3=sum(pmat[,2]>=true.value[3])/3000
p.val4=sum(pmat[,4]>=true.value[4])/3000
p.val5=sum(pmat[,5]>=true.value[5])/3000
p.val6=sum(pmat[,6]>=true.value[6])/3000
p.val7=sum(pmat[,7]>=true.value[7])/3000


reject <- function(p.v) {
  ifelse(p.v>=.975 | p.v<=.025, "Reject the null", "Fail to reject")
}

# Mean difference
c(p.val1, reject(p.val1))
# t statistic
c(p.val2, reject(p.val2))
# U statistic (tortoise)
c(p.val4, reject(p.val4))
# Z statistic
c(p.val5, reject(p.val5))
# Wilcox´s rank sum (tortoise)
c(p.val6, reject(p.val6))


## 3c) Plots Of The Above Distributions
histo.mean <- hist(perm.statistics$smeandiff, col='lightblue', xlab = "Difference In Sample Means", main = "Difference In Sample Means Distribution")
##abline(v = statistic.SMeanDiff, col='red',lwd=2)
##abline(v = -statistic.SMeanDiff, col='red',lwd=2)
histo.t <- hist(perm.statistics$t, col='lightgreen', xlab = "t", main = "t Distribution")
histo.U_Hare <- hist(perm.statistics$U_Hare, col='red', xlab = "U_Hare", main = "U_Hare Distribution")
histo.U_Tort <- hist(perm.statistics$U_Tort, col='gray', xlab = "U_Tort", main = "U_Tort Distribution")
histo.z <- hist(perm.statistics$z, col='lightpink', xlab = "z", main = "z Distribution")
##abline(v = statisticz.Zdis, col='red',lwd =2)
histo.W_Hare <- hist(perm.statistics$W_Hare, col='lavender', xlab = "W_Hare", main = "W_Hare Distribution")
histo.W_Tort <- hist(perm.statistics$W_Tort, col='bisque2', xlab = "W_Tort", main = "W_Tort Distribution")


## 3c) (v) QQ Plot For t Staistic
qqnorm(perm.statistics$t, xlab ="Theoretical vs Permutation Based", main="t Statistic");qqline(perm.statistics$t, col = 2)

## 3c) (v) QQ Plot For z Staistic
qqnorm(perm.statistics$z, xlab ="Theoretical vs Permutation Based", main="z Statistic");qqline(perm.statistics$z, col = 2)
##setwd("C:/Ramya Dhatri/Statistical Inference/Projects/Project 2")
berkeley_data <- read.csv("BGS.csv")
n <- nrow(berkeley_data)
berkeley_data$Color <- as.factor(berkeley_data$Sex)

############################ Descriptive Statistics ############################################
boys_data <- subset(berkeley_data, berkeley_data$Sex=="0")
girls_data <- subset(berkeley_data, berkeley_data$Sex=="1")

##### Tables - Continuous Variables ######
cont_data <- berkeley_data
cont_data$X <- NULL
cont_data$Sex <- NULL
library(stargazer)
stargazer(cont_data, type="text", median=TRUE, digits=2, 
          title="Descriptive Statistics Of The Total Dataset", 
          summary.stat = c("Mean", "sd", "Median", "Min", "Max"))

##### Tables - Categorical Variables ######


##### Plots #######

library(ggplot2)
library("cowplot")

## Histogram - Somatotype
s1.plot <- ggplot(data=berkeley_data) + 
  geom_histogram( aes(Soma), alpha = 0.3) +
  labs(title="Histogram for Somatotype") +
  labs(x="Somatotype")

s2.plot <- ggplot() +
  geom_histogram(data = boys_data, aes(Soma), alpha = 0.3, fill = "red", color = "red") + 
  labs(title="Histogram for Somatotype (Boys)") +
  labs(x="Somatotype")

s3.plot <- ggplot() + 
  geom_histogram(data=girls_data, aes(Soma), alpha = 0.3, fill = "blue", color = "blue") +
  labs(title="Histogram for Somatotype (Girls)") +
  labs(x="Somatotype")

plot_grid(s1.plot, align = "v", nrow = 2, ncol=2)
plot_grid(s2.plot, s3.plot, align = "v", nrow = 2, ncol=2)

## Scatterplot HT vs WT
hw2.plot <- ggplot(berkeley_data,aes(x=WT2, y=HT2))+
  geom_point(aes(colour=Color))+
  ggtitle("Heights At Age 2 vs Weights At Age 2")

hw9.plot <- ggplot(berkeley_data,aes(x=WT9, y=HT9))+
  geom_point(aes(colour=Color))+
  ggtitle("Heights At Age 9 vs Weights At Age 9")

hw18.plot <- ggplot(berkeley_data,aes(x=WT18, y=HT18))+
  geom_point(aes(colour=Color))+
  ggtitle("Heights At Age 18 vs Weights At Age 18")

plot_grid(hw2.plot, hw9.plot, hw18.plot, align = "v", nrow = 2, ncol=2)

## Boxplot Height
ggplot(berkeley_data[berkeley_data$Sex=="0" | berkeley_data$Sex=="1",], aes(x=cut_interval(x=WT2, length = 200), y=HT2, fill=Color)) + geom_boxplot()

h2.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=HT2, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Heights At Age 2")

h9.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=HT9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Heights At Age 9")

h18.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=HT18, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Heights At Age 18")

plot_grid(h2.plot, h9.plot, h18.plot, align = "v", nrow = 2, ncol=2)


## Boxplot Weight
w2.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=WT2, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Weights At Age 2")

w9.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=WT9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Weights At Age 9")

w18.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=WT18, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Weights At Age 18")

plot_grid(w2.plot, w9.plot, w18.plot, align = "v", nrow = 2, ncol=2)

## Boxplot Leg Circumfrence
l9.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=LG9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Leg Circumfrence At Age 9")

l18.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=LG18, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Leg Circumfrence At Age 18")

plot_grid(l9.plot, l18.plot, align = "v", nrow = 1, ncol=2)


## Boxplot Strength
s9.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=ST9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Strength At Age 9")

s18.plot <- ggplot(berkeley_data, aes(as.factor(Sex), y=ST18, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("Strength At Age 18")

plot_grid(s9.plot, s18.plot, align = "v", nrow = 1, ncol=2)

bgs <- berkeley_data
bgs$DW9 <- (bgs$WT9 - bgs$WT2)
bgs$DW18 <- (bgs$WT18 - bgs$WT9)
bgs$AVE <- (bgs$WT2 + bgs$WT9 + bgs$WT18)/3
bgs$LIN <- (bgs$WT18 - bgs$WT2)
bgs$QUAD <- (bgs$WT2 - 2*bgs$WT9 + bgs$WT18)

## Boxplot DW9, SW18 and LIN
wc1.plot <- ggplot(bgs, aes(as.factor(Sex), y=DW9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("WT9 - WT2")

wc2.plot <- ggplot(bgs, aes(as.factor(Sex), y=DW18, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("WT18 - WT9")

wc3.plot <- ggplot(bgs, aes(as.factor(Sex), y=LIN, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("WT18 - WT2")

plot_grid(wc1.plot, wc2.plot, wc3.plot, align = "v", nrow = 2, ncol=2)

## Boxplot Change In Height
hc1.plot <- ggplot(bgs, aes(as.factor(Sex), y=HT9-HT2, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("HT9 - HT2")

hc2.plot <- ggplot(bgs, aes(as.factor(Sex), y=HT18-HT9, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("HT18 - HT9")

hc3.plot <- ggplot(bgs, aes(as.factor(Sex), y=HT18-HT2, fill=Color)) + 
  geom_boxplot() +
  labs(x="Gender") +
  ggtitle("HT18 - HT2")

plot_grid(hc1.plot, hc2.plot, hc3.plot, align = "v", nrow = 2, ncol=2)

## Regression of Soma Vs HT
somah.plot1 <- ggplot(berkeley_data,aes(x=HT2, y=Soma))+
  geom_point(aes(colour=Color))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Soma vs HT2")

somah.plot2 <- ggplot(berkeley_data,aes(x=HT9, y=Soma))+
  geom_point(aes(colour=Color))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Soma vs HT9")

somah.plot3 <- ggplot(berkeley_data,aes(x=HT18, y=Soma))+
  geom_point(aes(colour=Color))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Soma vs HT18")

##install.packages("cowplot")
library("cowplot")
plot_grid(somah.plot1, somah.plot2, somah.plot3, align = "v", nrow = 2, ncol=2)


############################## Scatter Plot Matrix #####################################
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  #p <- cor.test(x, y)$p.value
  #txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  #txt2 <- paste("p= ", txt2, sep = "")
  #if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  #text(0.5, 0.4, txt2)
}
temp <- bgs
temp$Sex <- NULL
temp$DW9 <- NULL
temp$DW18 <- NULL
temp$AVE <- NULL
temp$LIN <- NULL
temp$QUAD <- NULL
pairs(temp, upper.panel = panel.cor)

##########################################################################################
bgs$X <- NULL
bgs$Color <- NULL

null_lm = lm(Soma~1, data=bgs)
full_lm = lm(Soma ~ as.factor(Sex) + WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + WT18
             + HT18 + LG18 + ST18 + DW9 + DW18 + AVE + LIN + QUAD +
               WT2:Sex + HT2:Sex + WT9:Sex + 
               HT9:Sex +LG9:Sex + ST9:Sex +
               WT18:Sex + HT18:Sex + LG18:Sex +
               ST18:Sex + DW9:Sex + DW18:Sex +
               AVE:Sex + LIN:Sex + QUAD:Sex , data=bgs)

full_lm_with_1 = lm(Soma ~ 1 + as.factor(Sex) + WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + WT18
                    + HT18 + LG18 + ST18 + DW9 + DW18 + AVE + LIN + QUAD +
                      WT2:Sex + HT2:Sex + WT9:Sex + 
                      HT9:Sex +LG9:Sex + ST9:Sex +
                      WT18:Sex + HT18:Sex + LG18:Sex +
                      ST18:Sex + DW9:Sex + DW18:Sex +
                      AVE:Sex + LIN:Sex + QUAD:Sex, data=bgs)

################################# Forward Elimination ######################################
# Using the R function "step" to do forward elimination:

###### Using the AIC: #########
forward.aic <- step(null_lm, scope=list(lower=null_lm,upper=full_lm_with_1),
                    direction="forward", data=bgs, k=2)
## forward.aic: AIC=-87.36
## lm(formula = Soma ~ as.factor(Sex) + LIN + HT18 + ST18 + DW9 + HT2 + ST18:Sex + 
##    LIN:Sex + DW9:Sex + HT2:Sex, data = bgs)


###### Using the BIC: #########
forward.bic <- step(null_lm, scope=list(lower=null_lm, upper=full_lm_with_1),
                    direction="forward", data=bgs, k=log(length(null_lm$residuals)))
## forward.bic: BIC=-57.7
## lm(formula = Soma ~ as.factor(Sex) + LIN + HT18 + ST18 + DW9 + ST18:Sex + 
##    LIN:Sex, data = bgs) ##


###### Using the C_p criterion: ########
library("arm")
forward.Cp <- step( null_lm, scope=list(lower=null_lm,upper=full_lm_with_1),
                    direction="forward", data=bgs, scale=sigma.hat(null_lm)^2 )
## forward.Cp: Cp=-88.61
## lm(formula = Soma ~ as.factor(Sex) + LIN + HT18 + ST18 + ST18:Sex, data = bgs)

##cor(as.matrix(bgs)[,c(1,16,9,11,13,3)])


################################# Backward Elimination #####################################
# Using the R function "step" to do backard elimination:

###### Using the AIC: #########
backward.aic <- step(full_lm, scope=list(lower=null_lm, upper=full_lm_with_1),
                     direction="backward",data=bgs, k=2 )
## backward.aic: AIC=-88.69
## lm(formula = Soma ~ WT2 + WT9 + HT9 + ST9 + WT18 + HT18 + LG18 + ST18 + WT2:Sex + 
##    HT9:Sex + ST9:Sex + HT18:Sex + LG18:Sex, data = bgs) ##


# Using the BIC:
backward.bic <- step(full_lm, scope=list(lower=null_lm, upper=full_lm_with_1),
                     direction="backward", data=bgs, k=log(length(full_lm$residuals)) )
## backward.bic: BIC=-58.15
## lm(formula = Soma ~ WT2 + WT9 + HT9 + ST9 + WT18 + LG18 + ST18 + WT2:Sex +  
##    ST9:Sex + LG18:Sex, data = bgs)

##diff ht18, ht9:sex, ht18:sex

# Using the C_p criterion:
backward.Cp <- step(full_lm, scope=list(lower=null_lm, upper=full_lm_with_1),
                    direction="backward", data=bgs, scale=sigma.hat(full_lm)^2 )
## backward.Cp: Cp=9.83
## lm(formula = Soma ~ WT2 + WT9 + HT9 + ST9 + WT18 + HT18 + LG18 + 
##    ST18 + WT2:Sex + HT9:Sex + ST9:Sex + HT18:Sex + LG18:Sex, data = bgs)

par(mfrow=c(2,2))
qqnorm(residuals(backward.bic), main = "Residuals - M5 (BIC)")
qqline(residuals(backward.bic))
qqnorm(residuals(backward.aic), main = "Residuals - M4 (AIC)")
qqline(residuals(backward.aic))
hist(residuals(backward.bic), main = "Residuals - M5 (BIC)")
hist(residuals(backward.aic), main = "Residuals - M4 (AIC)")

par(mfrow=c(2,2))
plot(bgs$Soma, residuals(backward.bic))
abline(0,0)
plot(bgs$Soma, residuals(backward.aic))
abline(0,0)

par(mfrow=c(1,1))
plot(bgs$Soma, residuals(backward.bic), xlab = "Predicted Values")
abline(0,0)
qplot(y = backward.bic$residuals, x = backward.bic$fitted.values,
      ylab = "Residuals", xlab = "Fitted values", 
      main = "Residuals vs. Fitted Plot (M5)") +
  stat_smooth(method = "loess", span = 0.1, colour = I("red"), se = FALSE)

library(xtable)
print(xtable(backward.bic, caption = "Statistic of Model M5 (BIC)"), type = "latex")
print(xtable(backward.aic, caption = "Statistic of Model M4 (AIC)"), type = "latex")

pairs(temp, upper.panel = panel.cor)
# set.seed(18)
# n1 = 40
# n2 = 20
# age1 = runif(n1, 6, 12)
# age2 = runif(n2, 6, 9)
# beta0 = -2
# beta1 = 6
# beta2 = -22
# beta12 = 4
# sigma = 4
# mu1 = beta0 + beta1*age1 + rnorm(n1, 0, sigma)
# mu2 = beta0 + beta2 + (beta1 + beta12)*age2 + rnorm(n2, 0, sigma)
#
# dat = data.frame(score = c(mu1, mu2),
#                  age = c(age1, age2),
#                  group = c(rep("Control", n1),
#                            rep("Treatment", n2)))
#
# write.csv(dat, file = "reading.csv")

dat = read.csv("data/reading.csv")
# Graph 1
# jitter for boxplot
set.seed(1)
noise = runif(n1 + n2,-.075,.075)
t_col = function(col, percent){
  rgb.val <- col2rgb(col)
  rgb(rgb.val[1], rgb.val[2], rgb.val[3],
      max = 255,
      alpha = (100 - percent) * 255 / 100)
}
# colours
mypurple = rgb(106, 90, 205,maxColorValue=255)
cruk_blue = rgb(55,56,149,maxColorValue=255)
cruk_lightblue = rgb(6, 188, 241,maxColorValue=255)
cruk_pink = rgb(235,7,142,maxColorValue=255)
cruk_yellow = rgb(240,178,0,maxColorValue=255)
cruk_yellow = rgb(240,178,0,maxColorValue=255)
beamer_gray = gray(.35)

library(tikzDevice)
tikz("tikz/tex/boxplot_RAE.tex",width=4,height=3,standAlone = TRUE)
par(mar=c(4,0,2,6))
plot(1,1,pch="",axes=FALSE,ylim=c(.5,2.6),xlim=range(dat$score),
     main="Reading ability assessment",xlab="",ylab="",col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
# Control
posw = dat$group=="Control"
boxplot(dat$score[posw],at=2,add=TRUE,outline=FALSE,
        col="light gray",axes=FALSE,horizontal=TRUE)
points(dat$score[posw],noise[posw]+2, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(dat$score[posw],noise[posw]+2, pch = 1, col = cruk_pink, cex = 1.25)

#.p(cruk_pink,50))
# Treatement
posw = dat$group=="Treatment"
boxplot(dat$score[posw],at=1,add=TRUE,outline=FALSE,
        col="light gray",axes=FALSE,horizontal=TRUE)
points(dat$score[posw],noise[posw]+1, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(dat$score[posw],noise[posw]+1, pch = 0, col = mypurple, cex = 1.25)

#.p(cruk_pink,50))

#abline(v=0,col=cruk_lightblue)

axis(1,las=1,col=beamer_gray,col.axis=beamer_gray)
axis(4,at=2,"Control",col=beamer_gray,col.axis=beamer_gray,tick=FALSE,las=2)
axis(4,at=1,"Treatement",col=beamer_gray,col.axis=beamer_gray,tick=FALSE,las=2)
dev.off()



library(tikzDevice)
tikz("tikz/tex/points.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()
points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

text(x = 10.55, y = 35, pos = 4, "Control", cex = 1.25, col = beamer_gray)
text(x = 10.55, y = 32, pos = 4, "Treatement", cex = 1.25, col = beamer_gray)

points(10.5, 35.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
points(10.5, 35.3, pch = 1, col = cruk_pink, cex = 1.5)
points(10.5, 32.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
points(10.5, 32.3, pch = 0, col = mypurple, cex = 1.5)
dev.off()




t.test(mu1, mu2, alternative = "less")

mod1 = lm(score ~ age + group, data = dat)


library(tikzDevice)
tikz("tikz/tex/points_with_mod1.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()
points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

mu1_pred = mod1$coefficients[1] + mod1$coefficients[2]*c(6,12)
mu2_pred = mod1$coefficients[1] +  mod1$coefficients[3] + mod1$coefficients[2]*c(6,12)

lines(c(6,12), mu1_pred, col = cruk_pink, lwd = 2.5)
lines(c(6,12), mu2_pred, col = mypurple, lwd = 2.5)

text(x = 10.55, y = 35, pos = 4, "Control", cex = 1.25, col = beamer_gray)
text(x = 10.55, y = 32, pos = 4, "Treatement", cex = 1.25, col = beamer_gray)

points(10.5, 35.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
points(10.5, 35.3, pch = 1, col = cruk_pink, cex = 1.5)
points(10.5, 32.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
points(10.5, 32.3, pch = 0, col = mypurple, cex = 1.5)

text(9.25, 45, pos = 4, "Score$_i$ = -7.86 + 6.65 Age$_i$", cex = 1, col = cruk_pink)

text(6, 70, pos = 4, "Score$_i$ = -1.49 + 6.65 Age$_i$", cex = 1, col = mypurple)

dev.off()


library(tikzDevice)
tikz("tikz/tex/qqplot_mod1.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
my_qq = qqnorm(mod1$residuals, plot.it = FALSE)

plot(NA, main="Normal Q-Q Plot",xlab=" ",ylab=" ",
     xlim = range(my_qq$x), ylim = range(my_qq$y),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Theoretical Quantiles", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Sample Quantiles", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)
grid()
qqline(mod1$residuals, lwd = 2)

posw = dat$group=="Control"
points(my_qq$x[posw], my_qq$y[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 1, col = cruk_pink, cex = 1)

posw = dat$group=="Treatement"
points(my_qq$x[posw], my_qq$y[posw], pch = 15, col = paste0(mypurple, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 0, col = mypurple, cex = 1)
dev.off()


# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag_mod1_v2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(mod1$fitted.values), ylim = range(mod1$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Fitted value", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(mod1$fitted.values[posw], mod1$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(mod1$fitted.values[posw], mod1$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod1$residuals[posw] ~ mod1$fitted.values[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(mod1$fitted.values)
lines(range(mod1$fitted.values), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatement"
points(mod1$fitted.values[posw], mod1$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(mod1$fitted.values[posw], mod1$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod1$residuals[posw] ~ mod1$fitted.values[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(mod1$fitted.values)
lines(range(mod1$fitted.values), pred2, col = mypurple, lwd = 2.5)
dev.off()



# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag2_mod1.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(dat$age),
     ylim = range(mod1$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(dat$age[posw], mod1$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(dat$age[posw], mod1$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod1$residuals[posw] ~ dat$age[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(dat$age)
lines(range(dat$age), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatement"
points(dat$age[posw], mod1$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(dat$age[posw], mod1$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod1$residuals[posw] ~ dat$age[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(dat$age)
lines(range(dat$age), pred2, col = mypurple, lwd = 2.5)
dev.off()



mod2 = lm(score ~ age*group, data = dat)

library(tikzDevice)
tikz("tikz/tex/points_with_mod2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()
points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

mu1_pred = mod2$coefficients[1] + mod2$coefficients[2]*c(6,12)
mu2_pred = mod2$coefficients[1] + mod2$coefficients[3] + (mod2$coefficients[2] + mod2$coefficients[4])*c(6,12)

lines(c(6,12), mu1_pred, col = cruk_pink, lwd = 2.5)
lines(c(6,12), mu2_pred, col = mypurple, lwd = 2.5)

text(x = 10.55, y = 35, pos = 4, "Control", cex = 1.25, col = beamer_gray)
text(x = 10.55, y = 32, pos = 4, "Treatement", cex = 1.25, col = beamer_gray)

points(10.5, 35.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
points(10.5, 35.3, pch = 1, col = cruk_pink, cex = 1.5)
points(10.5, 32.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
points(10.5, 32.3, pch = 0, col = mypurple, cex = 1.5)

text(9.25, 45, pos = 4, "Score$_i$ = -3.249 + 6.12 Age$_i$", cex = 1, col = cruk_pink)

text(6, 70, pos = 4, "Score$_i$ = -39.28 + 12.07 Age$_i$", cex = 1, col = mypurple)

dev.off()


library(tikzDevice)
tikz("tikz/tex/qqplot_mod2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
my_qq = qqnorm(mod2$residuals, plot.it = FALSE)

plot(NA, main="Normal Q-Q Plot",xlab=" ",ylab=" ",
     xlim = range(my_qq$x), ylim = range(my_qq$y),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Theoretical Quantiles", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Sample Quantiles", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)
grid()
qqline(mod2$residuals, lwd = 2)

posw = dat$group=="Control"
points(my_qq$x[posw], my_qq$y[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 1, col = cruk_pink, cex = 1)

posw = dat$group=="Treatement"
points(my_qq$x[posw], my_qq$y[posw], pch = 15, col = paste0(mypurple, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 0, col = mypurple, cex = 1)
dev.off()


# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag_mod2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(mod2$fitted.values), ylim = range(mod2$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Fitted value", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

posw = dat$group=="Treatement"
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 0, col = mypurple, cex = 1.25)
dev.off()


# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag_mod2_v2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(mod2$fitted.values), ylim = range(mod2$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Fitted value", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod2$residuals[posw] ~ mod2$fitted.values[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(mod2$fitted.values)
lines(range(mod2$fitted.values), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatement"
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(mod2$fitted.values[posw], mod2$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod2$residuals[posw] ~ mod2$fitted.values[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(mod2$fitted.values)
lines(range(mod2$fitted.values), pred2, col = mypurple, lwd = 2.5)
dev.off()



# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag2_mod2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(dat$age),
     ylim = range(mod2$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(dat$age[posw], mod2$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(dat$age[posw], mod2$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod2$residuals[posw] ~ dat$age[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(dat$age)
lines(range(dat$age), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatement"
points(dat$age[posw], mod2$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(dat$age[posw], mod2$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod2$residuals[posw] ~ dat$age[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(dat$age)
lines(range(dat$age), pred2, col = mypurple, lwd = 2.5)
dev.off()


# Import data (if you haven't already)
dat = read.csv("data/reading.csv")
dat$age_minus_6 = dat$age - 6

# Fit linear regression model
mod3 = lm(score ~ age_minus_6 + group:age_minus_6, data = dat)


library(tikzDevice)
tikz("tikz/tex/points_with_mod3.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()
points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

mu1_pred = mod3$coefficients[1] + (mod3$coefficients[2])*(c(6,12)-6)
mu2_pred = mod3$coefficients[1]+ (mod3$coefficients[2] + mod3$coefficients[3])*(c(6,12)-6)

lines(c(6,12), mu1_pred, col = cruk_pink, lwd = 2.5)
lines(c(6,12), mu2_pred, col = mypurple, lwd = 2.5)

text(x = 10.55, y = 35, pos = 4, "Control", cex = 1.25, col = beamer_gray)
text(x = 10.55, y = 32, pos = 4, "Treatement", cex = 1.25, col = beamer_gray)

points(10.5, 35.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
points(10.5, 35.3, pch = 1, col = cruk_pink, cex = 1.5)
points(10.5, 32.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
points(10.5, 32.3, pch = 0, col = mypurple, cex = 1.5)

text(9.1, 45, pos = 4, "Score$_i$ = 33.35 + 6.15 (Age$_i$ - 6)", cex = 0.96, col = cruk_pink)

text(5.6, 70, pos = 4, "Score$_i$ = 33.35 + 11.96 (Age$_i-6)$", cex = 0.96, col = mypurple)

dev.off()


library(tikzDevice)
tikz("tikz/tex/qqplot_mod3.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))
my_qq = qqnorm(mod3$residuals, plot.it = FALSE)

plot(NA, main="Normal Q-Q Plot",xlab=" ",ylab=" ",
     xlim = range(my_qq$x), ylim = range(my_qq$y),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Theoretical Quantiles", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Sample Quantiles", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)
grid()
qqline(mod3$residuals, lwd = 2)

posw = dat$group=="Control"
points(my_qq$x[posw], my_qq$y[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 1, col = cruk_pink, cex = 1)

posw = dat$group=="Treatment"
points(my_qq$x[posw], my_qq$y[posw], pch = 15, col = paste0(mypurple, 50), cex = 1)
points(my_qq$x[posw], my_qq$y[posw], pch = 0, col = mypurple, cex = 1)
dev.off()


# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag_mod3.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(mod3$fitted.values), ylim = range(mod3$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Fitted value", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

posw = dat$group=="Treatment"
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 0, col = mypurple, cex = 1.25)
dev.off()


# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag_mod3_v2.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(mod3$fitted.values), ylim = range(mod3$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Fitted value", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod3$residuals[posw] ~ mod3$fitted.values[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(mod3$fitted.values)
lines(range(mod3$fitted.values), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatment"
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(mod3$fitted.values[posw], mod3$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod3$residuals[posw] ~ mod3$fitted.values[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(mod3$fitted.values)
lines(range(mod3$fitted.values), pred2, col = mypurple, lwd = 2.5)
dev.off()




# Graph 2
library(tikzDevice)
tikz("tikz/tex/diag2_mod3.tex",width=5.25,height=3.9375,standAlone = TRUE)
par(mar=c(4,4,2,2))

plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = range(dat$age),
     ylim = range(mod3$residuals),
     col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
mtext("Residual", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

grid()

posw = dat$group=="Control"
points(dat$age[posw], mod3$residuals[posw], pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
points(dat$age[posw], mod3$residuals[posw], pch = 1, col = cruk_pink, cex = 1.25)

fit1 = lm(mod3$residuals[posw] ~ dat$age[posw])
pred1 = fit1$coefficients[1] + fit1$coefficients[2]*range(dat$age)
lines(range(dat$age), pred1, col = cruk_pink, lwd = 2.5)

posw = dat$group=="Treatment"
points(dat$age[posw], mod3$residuals[posw], pch = 15, col = paste0(mypurple, 50), cex = 1.25)
points(dat$age[posw], mod3$residuals[posw], pch = 0, col = mypurple, cex = 1.25)

fit2 = lm(mod3$residuals[posw] ~ dat$age[posw])
pred2 = fit2$coefficients[1] + fit2$coefficients[2]*range(dat$age)
lines(range(dat$age), pred2, col = mypurple, lwd = 2.5)
dev.off()




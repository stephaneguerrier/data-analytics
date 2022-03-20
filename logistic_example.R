library(mlbench)
data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)

fit.glm = glm(diabetes ~ ., data = PimaIndiansDiabetes, family = binomial(link="logit"))
summary(fit.glm)

fit.glm.aic = step(fit.glm, trace = TRUE)
summary(fit.glm.aic)

# Initial model
fit.glm.inital = glm(diabetes ~ 1, data = PimaIndiansDiabetes,
                     family = binomial(link="logit"))

# Find a model with a forward approach using the AIC
fit.glm.aic.forward = step(fit.glm.inital,
                           scope = list(lower = formula(fit.glm.inital),
                                        upper = formula(fit.glm)),
                           direction = "forward", trace = FALSE)
summary(fit.glm.aic.forward)


class_predict = fit.glm.aic$fitted.values > 0.5
in_accuracy = mean((PimaIndiansDiabetes$diabetes == "pos") == class_predict)
in_accuracy

table(PimaIndiansDiabetes$diabetes)/dim(PimaIndiansDiabetes)[1]

library(boot)
cost = function(resp, pred){
  mean(resp == (pred > 0.5))
}
out_accuracy = cv.glm(PimaIndiansDiabetes, fit.glm.aic, cost, K = 10)$delta[2]
out_accuracy

fit.gamlss = gamlss(formula(fit.glm.aic), data=PimaIndiansDiabetes, family=BI)
plot(fit.gamlss)






data(BreastCancer)
dim(BreastCancer)
head(BreastCancer)
table(BreastCancer$Class)

BreastCancer = na.omit(BreastCancer)
dim(BreastCancer)

fit.glm = glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data = BreastCancer, family=binomial)
summary(fit.glm)
length(fit.glm$coefficients)
BreastCancer$Cl.thickness

min(table(BreastCancer$Class))/20


# Initial model with forward approach using AIC
fit.glm.inital = glm(Class ~ 1, data = BreastCancer,family=binomial)
forward.fit = step(fit.glm.inital,
                   scope=list(lower = formula(fit.glm.inital),
                              upper=formula(fit.glm)),
                   direction="forward", trace = FALSE)

summary(forward.fit)
length(forward.fit$coefficients)


forward.fit.BIC = step(fit.glm.inital,
                       scope=list(lower = formula(fit.glm.inital),
                                  upper=formula(fit.glm)),
                       direction="forward",
                       k = log(length(BreastCancer$Id)), # for BIC
                       trace = FALSE)

summary(forward.fit.BIC)
length(forward.fit.BIC$coefficients) # Better!


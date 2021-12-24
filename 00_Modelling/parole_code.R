library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)

parole <- read.csv('parole.csv', 
                   colClasses = c('factor', 'factor','numeric', 'factor', 'numeric',
                            'numeric', 'factor', 'factor', 'numeric') )
parole$viol_fac <- factor(parole$violator,levels=c(0,1),labels=c("No","Yes"))
parole$crime_fac <- factor(parole$crime,levels=c(1,2,3,4),labels=c("Other","Larceny", "Drug", "Driving"))



## Centering continuous predictors
parole$ageC <- parole$age - mean(parole$age)
parole$time.servedC <- parole$time.served - mean(parole$time.served)
parole$max.sentenceC <- parole$max.sentence - mean(parole$max.sentence) 
# summary(parole)
# head(parole)
# dim(parole)

## EDA

## Age vs violation
## almost no change in trends. I would assume the younger people are less 
##likely to violate
chisq.test(parole$viol_fac, parole$age)
ggplot(parole,aes(x=violator, y=age, fill=violator)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs violator",
       x="violator",y="age") + 
  theme_classic() + theme(legend.position="none") +
scale_x_discrete(labels=c("0" = "No","1" = "Yes"))

## time served in months vs violators
## kinda seems like more time served correlated with unlikely to 
chisq.test(parole$viol_fac, parole$time.served)
ggplot(parole,aes(x=violator, y=time.served, fill=violator)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs violator",
       x="violator",y="time served") + 
  theme_classic() + theme(legend.position="none") +
  scale_x_discrete(labels=c("0" = "No","1" = "Yes"))

## max sentence in months vs violators
## kinda seems like longer sentence correlated with unlikely to violate
chisq.test(parole$viol_fac, parole$max.sentence)
ggplot(parole,aes(x=violator, y=max.sentence, fill=violator)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Purples") +
  labs(title="Age vs violator",
       x="violator",y="max sentence") + 
  theme_classic() + theme(legend.position="none") +
  scale_x_discrete(labels=c("0" = "No","1" = "Yes"))


##Chi-squared tests for categorical predictors
#not significant
chisq.test(parole$viol_fac, parole$male)
chisq.test(parole$viol_fac, parole$race)
chisq.test(parole$viol_fac, parole$crime)

# significant, mult offenses kinda sig
chisq.test(parole$viol_fac, parole$state)
chisq.test(parole$viol_fac, parole$multiple.offenses)

## Categorical Variables
tapply(parole$violator, parole$male, function(x) table(x)/sum(table(x)))
#gender
apply(table(parole[,c("violator","male")])/sum(table(parole[,c("violator","male")])),
      2,function(x) x/sum(x)) 
#race
apply(table(parole[,c("violator","race")])/sum(table(parole[,c("violator","race")])),
      2,function(x) x/sum(x)) 

#state
apply(table(parole[,c("violator","state")])/sum(table(parole[,c("violator","state")])),
      2,function(x) x/sum(x)) 

#multiple.offenses
apply(table(parole[,c("violator","multiple.offenses")])/sum(table(parole[,c("violator","multiple.offenses")])),
      2,function(x) x/sum(x)) 

#crime 
apply(table(parole[,c("violator","crime")])/sum(table(parole[,c("violator","crime")])),
      2,function(x) x/sum(x)) 


## Interaction between continuous and categorical predictors
# age and gender
# seems like median age for women who violated and didnt were different
# whereas for men they were mostly the same 
ggplot(data=parole, aes(x=age, y=viol_fac)) + geom_boxplot() +
  facet_wrap(~male)

# age and race
# nothing super interesting but slight difference in trends
ggplot(data=parole, aes(x=age, y=viol_fac)) + geom_boxplot() +
  facet_wrap(~race)

#age and crime
# interesting plot here but perhaps just because of 
# data points being limited
ggplot(data=parole, aes(x=age, y=viol_fac,  fill = viol_fac)) + 
  geom_boxplot() + scale_fill_manual(values = c('plum4', 'salmon2')) +
  facet_wrap(~crime_fac)  

## Interactions between categorical variables
# race and gender, not interesting
men = parole[which(parole$male == 1),]
women = parole[which(parole$male == 0),]
round(apply(table(men[,c('viol_fac','race')])/sum(table(men[,c('viol_fac','race')])),
            2,function(x) x/sum(x)),2)
round(apply(table(women[,c('viol_fac','race')])/sum(table(women[,c('viol_fac','race')])),
            2,function(x) x/sum(x)),2)

## Binned Plots of continuous predictors vs violators

# time served
binnedplot(y=parole$violator,parole$time.served,xlab="Time Served",ylim=c(0,1),col.pts="navy",
           ylab ="Violator?",main="Binned time served and violator cases",
           col.int="white")

# age
binnedplot(y=parole$violator,parole$age,xlab="Age",ylim=c(0,1),col.pts="navy",
           ylab ="Violator?",main="Binned Age and violator cases",
           col.int="white")

# max.sentence
binnedplot(y=parole$violator,parole$max.sentence,xlab="max.sentence",ylim=c(0,1),col.pts="navy",
           ylab ="Violator?",main="Binned max.sentence and violator cases",
           col.int="white")

## Fitting a simple initial model
parole$crime <- relevel(parole$crime, 4)
parole$state <- relevel(parole$state, 1)
init_model <- glm(violator ~ male + race + ageC + state + crime + 
          time.servedC + max.sentenceC, data = parole, family = 'binomial')

summary(init_model)




## Specificity and Sensitivity
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(init_model) >= 0.5, "1","0")),
                            as.factor(parole$violator),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] 



## Model Selection

n <- nrow(parole)

## Null Model
null_model <- glm(violator ~ ageC + crime + time.servedC , data = parole, 
                  family = 'binomial')

## Full Model
full_model <- init_model <- glm(violator ~ male * race * ageC * state * crime + 
                                  time.servedC * max.sentenceC, 
                                data = parole, family = 'binomial')

## Selection Methods
aic_step <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "both", trace = 0)
aic_for <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "forward", trace = 0)
aic_back <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "back", trace = 0)
bic_step <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "both", trace = 0, k = log(n))
bic_for <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "forward", trace = 0, k = log(n))
bic_back <- step(null_model, scope = list(upper = full_model, lower = null_model), direction = "back", trace = 0, k = log(n))

## Anova test between AIC step and BIC step
anova(aic_step, bic_step, test = 'Chisq')
## test was significant at the 95% level so we decided to go with aic_step, 
##to have a more meaningful model
 final_model <- aic_step

## Model Assessment
#binned residual plots
rawresfinal <- residuals(final_model, "resp")
binnedplot(x=fitted(final_model),y=rawresfinal, xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",
           col.pts="navy")

## Raw residuals with Age Centered
binnedplot(x=parole$ageC, y=rawresfinal, xlab="Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


## Raw residuals with Time served Centered
binnedplot(x=parole$time.servedC, y=rawresfinal, xlab="Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


## Raw residuals with max sentence Centered
binnedplot(x=parole$max.sentenceC, y=rawresfinal, xlab="Arsenic centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


mean(parole$violator)

Conf_mat2 <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= mean(parole$violator), "1","0")),
                             as.factor(parole$violator),positive = "1")
Conf_mat2$table
Conf_mat2$overall["Accuracy"];
Conf_mat2$byClass[c("Sensitivity","Specificity")]

## ROC Curve
roc(parole$violator,fitted(final_model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")


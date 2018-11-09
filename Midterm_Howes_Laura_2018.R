
#' Midterm 2018
#' 
#' @Date 2018-11-09
#' 
#' @author Laura Howes
#' -----------------------------------------------------------------------

####You will be graded on
####1. Correct answers
####2. Showing how you arrived at that answer
####3. Well formatted and documented code
####4. Thoughtful answers

library(readr)
library(profileModel)
library(ggplot2)
library(dplyr)
library(MASS)
library(tidyr)
library(brms)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(purrr)

setwd("C:/Users/Laura/Dropbox/Grad school/BIOL 607 Biostats/Midterm/data")
getwd()

#1) Sampling your system (10 points)
####Each of you has a study system your work in and a question of interest. Give an example of one variable that you would sample in order to get a sense of its variation in nature. Describe, in detail, how you would sample for the population of that variable in order to understand its distribution. Questions to consider include, but are not limited to: Just what is your sample versus your population? What would your sampling design be? Why would you design it that particular way? What are potential confounding influences of both sampling technique and sample design that you need to be careful to avoid? What statistical distribution might the variable take, and why?




  
#2) Let's get philosophical. (10 points)
####Are you a frequentist, likelihoodist, or Bayesian? Why? Include in your answer why you prefer the inferential tools (e.g. confidence intervals, test statistics, posterior probabilities, etc.) of your chosen worldview and why you do not like the ones of the other one. This includes defining just what those different tools mean! extra credit for citing and discussing outside sources - one point per source/point





#3) Power (20 points)
####We have a lot of aspects of the sample of data that we collect which can alter the power of our linear regressions.

####Slope
####Intercept
####Residual variance
####Sample Size
####Range of X values

####Choose three of the above properties and demonstrate how they alter power of an F-test from a linear regression using at least three different alpha levels (more if you want!) As a baseline of the parameters, let's use the information from the seal data:
  
####slope = 0.00237, intercept=115.767, sigma = 5.6805, range of seal ages = 958 to 8353, or, if you prefer, seal ages ~ N(3730.246, 1293.485). Your call what distribution to use for seal age simulation.


#simulate the data - need a data frame

#sample size,slope,intercept is what I chose

seal_data_sim <- data.frame(slope = seq(0, 0.00237, 0.001),
                      sigma = 5.6805) %>%
  crossing(n=seq(5, 65, 10))%>%
  crossing(intercept = seq(95, 115.767, 10)) %>%
  crossing(alpha = c(0.001, 0.05, 0.1)) #3 alphas #get a p-value

#expand to have a certain number of simulations for each sample size
seal_data_sim <- seal_data_sim %>%
  group_by(slope, intercept, sigma, n, alpha) %>%
  expand(reps = 1:n) %>%
  ungroup()

#simulate each # of times
seal_data_sim <- seal_data_sim %>%
  crossing(sim = 1:500)  #500 times #repeat many number of times

#add in fitted values, simulate random draws of ages)
seal_data_sim <- seal_data_sim %>%
  mutate(age.days = runif(n(), 958, 8353)) %>% #range of seal ages
  mutate(length.cm = rnorm(n(), intercept + slope*age.days, sigma)) #y = mx+ b

seal_data_sim

#fit a model
seal_fit <- seal_data_sim %>%
  group_by(slope, intercept, sigma, n, sim, alpha) %>%
  nest() %>%
  mutate(mod = purrr:::map(data, ~lm(length.cm ~ age.days, data = seal_data_sim))) #adding linear regression

seal_fit

#calculate powers to compare sample size, slope, intercept:
seal_power_slope <- seal_fit  %>%
  group_by(slope, alpha) %>%
  summarise(power = 1-sum(0.05 >= alpha)/n()) %>%
  ungroup()

seal_power_intercept <- seal_fit  %>%
  group_by(intercept, alpha) %>%
  summarise(power = 1-sum(0.05 >= alpha)/n()) %>%
  ungroup()

seal_power_sample_size <- seal_fit  %>%
  group_by(n, alpha) %>%
  summarise(power = 1-sum(0.05 >= alpha)/n()) %>%
  ungroup()


#plot to show how they alter power of an F-test from a linear regression

seal_slope_plot <- ggplot(data = seal_power_slope, 
                     mapping = aes(x = slope, y = power, color = factor(alpha))) +
  geom_line()  +
  theme_bw()

seal_intercept_plot<- ggplot(data = seal_power_intercept, 
                    mapping = aes(x = intercept, y = power, color = factor(alpha))) +
  geom_line()  +
  theme_bw()

seal_sample_size_plot <- ggplot(data = seal_power_sample_size, 
                        mapping = aes(x = n, y = power, color = factor(alpha))) +
  geom_line()  +
  theme_bw()


seal_slope_plot + seal_intercept_plot + seal_sample_size_plot

#####So far none of them have an effect on power? #and where is .001 alpha?


####Extra credit 1 - test whether the distribution of ages alters power: 3 points

####Extra Credit 2 Choose just one of the above elements to vary. Using likelihood to fit models, repeat your power analysis for a chi-square likelihood ratio test. You can use glm(), bbmle or some other means of fitting and obtaining a LRT at your discretion. 5 points.



#4) Bayes Theorem
####I've referenced the following figure a few times. I'd like you to demonstrate your understanding of Bayes Theorem by hand showing what the probability of the sun exploding is given the data. Assume that your prior probability that the sun explodes is p(Sun Explodes) = 0.0001. The rest of the information you need is in the cartoon!

####Bayes theorum:

#### p(H|D) = [p(D|H)*p(H)] / p(D)
##### p(D|H) = likelihood
##### p(H) = prior
##### p(D) = marginal probability p(D|H2)*p(H1) + p(D|H2)*p(H2)

####p(SunExplodes|Yes) = [p(Yes|SunExplodes)*p(SunExplodes)] / p(Yes)
  
prob_yes_sun_exp <- 0.5833 #35/36 #can assume from cartoon
prob_sun_explodes <- 0.0001  #prior given to us

#need to calculate marginal p(Yes): p(D|H2)*p(H1) + p(D|H2)*p(H2)

prob_dice_lied_that_sun_exploaded <- 0.027 #1/36
prob_no_sun_exploaded <- 1 - prob_sun_explodes #0.9999

p_yes <- prob_yes_sun_exp*prob_sun_explodes + prob_dice_lied_that_sun_exploaded*prob_no_sun_exploaded

p_yes <- (0.5833*0.0001) + (0.027*0.9999) #0.00005833 + 0.0269973

p_yes <- 0.02705563

####p(SunExplodes|Yes) = [prob_yes_sun_exp*prob_sun_explodes] / p_yes

p_SunExplodes_Yes <- (0.5833 * 0.0001)/0.02705563

#The probability the sun explodes given the data is 0.00215593


  
#5) Quailing at the Prospect of Linear Models
####I'd like us to walk through the three different 'engines' that we have learned about to fit linear models. To motivate this, we'll look at Burness et al.'s 2012 study "Post-hatch heat warms adult beaks: irreversible physiological plasticity in Japanese quail http://rspb.royalsocietypublishing.org/content/280/1767/20131436.short the data for which they have made available at Data Dryad at http://datadryad.org/resource/doi:10.5061/dryad.gs661. We'll be looking at the morphology data.

morph_data <- read_csv("./Morphology data.csv")

head(morph_data)

#initial visualization
plot <- ggplot(data = morph_data, mapping = aes(x = `Culmen (mm)`, y = `Tarsus (mm)`)) +
  geom_point()

plot #at first glance it appears there is a positive linear trend of how leg length predicts beak length, but it also starts to curve a bit in the upper region.


##5.1 Three fits (10 points)
####To begin with, I'd like you to fit the relationship that describes how Tarsus (leg) length predicts upper beak (Culmen) length. Fit this relationship using least squares, likelihood, and Bayesian techniques. For each fit, demonstrate that the necessary assumptions have been met. Note, functions used to fit with likelihood and Bayes may or may not behave well when fed NAs. So look out for those errors.

####Least squares

#least squares model

morph_lsqs <- lm(morph_data$`Culmen (mm)`~ morph_data$`Tarsus (mm)`) #when I take out morph_data$, Error in eval(predvars, data, env) : object 'Culmen (mm)' not found. It only works with morph_data$

#checking assumptions
plot(morph_lsqs, which = 1) #residuals scattered a bit
plot(morph_lsqs, which = 2) #tails don't fit on qqplot line

#testing assumptions
summary(morph_lsqs) #F value is high, and P value is very small, so can reject the null

#to address qqplot line weirdness, do a log transformation
morph_lsqs_log <- lm(log(morph_data$`Culmen (mm)`) ~ log(morph_data$`Tarsus (mm)`))

#checking assumption of log transformation
plot(morph_lsqs_log, which = 1) #fts better than first residual plot
plot(morph_lsqs_log, which = 2) #better fit for qq line, but one tail doesn't fit

#testing assumptions
summary(morph_lsqs_log) #same F value and P value

####Likelihood

#likelihood model 
morph_glm <- glm((morph_data$`Culmen (mm)`~ morph_data$`Tarsus (mm)`), 
                 data = morph_data,
                 family = gaussian(link = "identity"))


#checking assumptions
morph_fit <- predict(morph_glm)
morph_res<- residuals(morph_glm)
qplot(morph_fit, morph_res) #no real pattern in fitted vs residuals, so good with model


qqnorm(morph_res)
qqline(morph_res) #tails are not a great fit again, log would be better fit

plot(profile(morph_glm, objective = "ordinaryDeviance")) #lines are straight

#testing assumptions
#f-test
morph_glm_null <- glm(morph_data$`Culmen (mm)` ~ 1, 
                     family = gaussian(link = "identity"),
                     data = morph_data)

anova(morph_glm_null, morph_glm , test = "LRT") # f-value high, and p-value is small, so can reject the null

#t-tests of parameters
summary(morph_glm)


####Bayesian

#Bayesian model

morph_data_csv <- read.csv("./Morphology data.csv") #for some reason I can only get Bayes to work with read.csv, but not read_csv, hence new data frame.
head(morph_data_csv)

morph_bayes <- brm(Culmen..mm.~ Tarsus..mm., #Culmen is response variable
                   family = gaussian(link = "identity"),
                   data = morph_data_csv ,
                   file = "./morph_bayes.rds")  #NAs were excluded from model

#checking assumptions:

#look at chains:
plot(morph_bayes) #looks normal
#Inspect rhat
mcmc_rhat(rhat(morph_bayes)) #close to 1, which is good
#Inspect Autocorrelation
mcmc_acf(as.data.frame(morph_bayes)) #autocorrelation drops off really quickly, so looks good


#testing assumptions:

#model assumptions
morph_bayes_fit <- predict(morph_bayes) %>% as_tibble
morph_bayes_res <- residuals(morph_bayes) %>% as_tibble

qplot(morph_bayes_res$Estimate, morph_bayes_fit$Estimate) #no reat pattern in fitted vs residuals, so good about gaussian model

#fit
pp_check(morph_bayes, type="scatter") #observed and fitted match up, so pretty good predictions

#normality
qqnorm(morph_bayes_res$Estimate)
qqline(morph_bayes_res$Estimate) #pretty good fit, tails could be better
pp_check(morph_bayes, type="error_hist", bins = 8) #each histogram looks normal

##match to posterior
pp_check(morph_bayes, type="stat_2d", test=c("mean", "sd")) #pretty good point cloud, well-centered around point.
pp_check(morph_bayes) #pretty nice distribution fit


##5.2 Three interpretations (10 points)
####OK, now that we have fits, take a look! Do the coefficients and their associated measures of error in their estimation match? How would we interpret the results from these different analyses differently? Or would we? Note, confint works on lm objects as well.

####least squares model
summary(morph_lsqs) #this shows we can reject the null with the low P-value.
confint(morph_lsqs)

####likelihood model
morph_glm
confint(morph_glm) #this mathes the least sqauares confidence intervals

####Bayesian model
plot(morph_bayes) #looks normal
summary(morph_bayes, digits = 5) #Good effect of sample size, good R hat, CIs didn't include zero, so means they didn't overlap. 
posterior_interval(morph_bayes) #confidence intervals - nice posteriors

#####When performing each model, all of them seem to show some sort of relationship of culmen length being affected by tarsus length. The least squares model is tested with an f-test and produces a p-value, which can be rejected or accepted based on an alpha of 0.05. P-value was small, which means the null hypothesis of no affect could be rejected. 

#####The likelihood model looks at how well the data supports the given hypothesis, and each parameter choice is a hypothesis. It evaluates the weight of evidence for each hypothesis. It's similar to the frequentist method of least squares, looking at the probability for the data given the hypothesis. It has the same confidence intervals at the least squares model, and also works with p-values (which this test also produced a p-value to reject the null).

#####The Bayesian model looks at the probability of the hypothesis given the data. It also includes prior hypotheses into the model, making it a better predictor. One reason it is a better predict is because you can evaluate the degree of belief.   


##5.3 Everyday I'm Profilin' (10 points)  
####For your likelihood fit, are your profiles well behaved? For just the slope, use grid sampling to create a profile. You'll need to write functions for this, and use the results from your glm() fit to provide the reasonable bounds of what you should be profiling over (3SE should do). Is it well behaved? Plot the profile and give the 80% and 95% CI. Verify your results with profileModel.


#data generating proccess, need to make a function

lik_func <- function(slope, intercept, resid_sd){
    
    morph_fit <- intercept + slope * morph_data$`Tarsus (mm)`

    #likelihood
    sum(dnorm(morph_data$`Culmen (mm)`, morph_fit, resid_sd, log = TRUE))
}
    
    
#grid sample

summary(morph_glm) #to get values for grid. Intercept = -0.09871 
profile(morph_glm, objective = "ordinaryDeviance")
#range of slope values (3SE)

morph_grid <- crossing(slope = seq(0.25, 0.45, 0.005),
                        intercept = seq(-.75, -0.09871, .75),
                        resid_sd = .5, 1.5, .05) %>% 
  rowwise() %>% 
  mutate(logl = lik_func(slope, intercept, resid_sd)) %>%
  ungroup()


#get our profiles so we can get the CI, looking via slope
slope_prof <- morph_grid %>%
  group_by(slope) %>%
  filter(logl == max(logl)) %>% #filtering for max likelihood
  ungroup()

#plot profile
slope_prof_plot <- ggplot(slope_prof,
       mapping = aes(x = slope, y = logl)) + 
  geom_line() +  xlim(0.35,0.39)+ ylim(-1260,-1248) #zoom in

slope_prof_plot  #nothing plots? #is it well behaved?



#give the 95% CI.
#profile
slope_prof %>%
  filter(logl >= max(logl) - 1.96) %>% #1.96SE = CI
  arrange(slope) %>%
  filter(row_number()==1 | row_number()== n()) %>%
  as.data.frame()


#give the 80% CI.
#profile
slope_prof %>%
  filter(logl >= max(logl) - 1.28) %>% #z value 1.28 = 80%CI
  arrange(slope) %>%
  filter(row_number()==1 | row_number()== n()) %>%
  as.data.frame()


#Verify your results with profileModel

prof_mod <- profileModel(morph_glm,
                              objective = "ordinaryDeviance")

plot(prof_mod)

##5.4 The Power of the Prior (10 points)  
####This data set is pretty big. After excluding NAs in the variables we're interested in, it's over 766 lines of data! Now, a lot of data can overhwelm a strong prior. But only to a point. Show first that there is enough data here that a prior for the slope with an estimate of 0.4 and a sd of 0.01 is overwhelmed by the data by demonstrating that it produces similar results to our already fit flat prior. Second, see if a very small sample size (n = 10) would at least include 0.4 in it's 95% Credible Interval. Last, demonstrate at what sample size that 95% CL first begins to include 0.4 when we have a strong prior. How much data do we really need to overcome our prior belief? Note, it takes a long time to fit these models, so, try a strategy of spacing out the 3-4 sample sizes, and then zoom in on an interesting region.

prior_summary(morph_bayes)

morph_bayes_prior <- brm(Culmen..mm.~ Tarsus..mm., 
                         family = gaussian(link = "identity"),
                         data = morph_data_csv,
                         prior = c(
                           prior(normal(0.4,0.01))),
                         file = "./morph_bayes_prior.rds")  #NAs were excluded from model

#checking assumptions: 

#look at chains:
plot(morph_bayes_prior)  #looks normal
#Inspect rhat
mcmc_rhat(rhat(morph_bayes_prior))  #almost all close to 1, so good
#Inspect Autocorrelation
mcmc_acf(as.data.frame(morph_bayes_prior))  #autocorrelation drops off really quickly, so looks good


#testing assumptions:

#model assumptions
morph_bayes_prior_fit <- predict(morph_bayes_prior) %>% as_tibble
morph_bayes_prior_res <- residuals(morph_bayes_prior) %>% as_tibble

qplot(morph_bayes_prior_res$Estimate, morph_bayes_prior_fit$Estimate)  #no patterns in fitted vs residuals, so good about gaussian model

#fit
pp_check(morph_bayes_prior, type ="scatter") #do observed and fitted match up? pretty good predictions

#normality
qqnorm(morph_bayes_prior_res$Estimate)
qqline(morph_bayes_prior_res$Estimate) #fits pretty ok, tails slightly off
pp_check(morph_bayes_prior, type="error_hist", bins = 8)  #each histogram looks pretty normal

##match to posterior
pp_check(morph_bayes_prior, type="stat_2d", test=c("mean", "sd")) #Stat 2d pretty good point cloud
pp_check(morph_bayes_prior) #pretty nice of distribution

posterior_interval(morph_bayes_prior) 

summary(morph_bayes_prior, digits = 5)  #0.4 is not in the CI



####See if a very small sample size (n = 10) would at least include 0.4 in it's 95% Credible Interval

#sample size of n = 10 data creation
sample_10_size <- data.frame(Tarsus10 = morph_data_csv$Tarsus..mm.[1:10],
                             Culmen10 = morph_data_csv$Culmen..mm. [1:10])

sample_10_size

morph_bayes_prior_n10 <- brm(Culmen10 ~ Tarsus10, 
                         family = gaussian(link = "identity"),
                         data = sample_10_size,
                         prior = c(
                           prior(normal(0.4,0.01))),
                         file = "./morph_bayes_prior_n10.rds") 

posterior_interval(morph_bayes_prior_n10) 

summary(morph_bayes_prior_n10, digits = 5) 

#####yes, it does include 0.4 in it's 95% Credible Interval.


####Last, demonstrate at what sample size that 95% CL first begins to include 0.4 when we have a strong prior. How much data do we really need to overcome our prior belief?

#need to make different sample size data to test:

sample_100_size <- data.frame(Tarsus100 = morph_data_csv$Tarsus..mm.[1:100],
                             Culmen100 = morph_data_csv$Culmen..mm. [1:100])

sample_100_size

morph_bayes_prior_n100 <- brm(Culmen100 ~ Tarsus100, 
                             family = gaussian(link = "identity"),
                             data = sample_100_size,
                             prior = c(
                               prior(normal(0.4,0.01))),
                             file = "./morph_bayes_prior_n100.rds") 


posterior_interval(morph_bayes_prior_n100) 

summary(morph_bayes_prior_n100, digits = 5) 

#####yes, it does include 0.4 in it's 95% Credible Interval.

sample_200_size <- data.frame(Tarsus200 = morph_data_csv$Tarsus..mm.[1:200],
                              Culmen200 = morph_data_csv$Culmen..mm. [1:200])

sample_200_size

morph_bayes_prior_n200 <- brm(Culmen200 ~ Tarsus200, 
                              family = gaussian(link = "identity"),
                              data = sample_200_size,
                              prior = c(
                                prior(normal(0.4,0.01))),
                              file = "./morph_bayes_prior_n200.rds") 


posterior_interval(morph_bayes_prior_n200) 

summary(morph_bayes_prior_n200, digits = 5) 

#####no, it doesn't include 0.4 in it's 95% Credible Interval.

sample_150_size <- data.frame(Tarsus150 = morph_data_csv$Tarsus..mm.[1:150],
                              Culmen150 = morph_data_csv$Culmen..mm. [1:150])

sample_150_size

morph_bayes_prior_n150 <- brm(Culmen150 ~ Tarsus150, 
                              family = gaussian(link = "identity"),
                              data = sample_150_size,
                              prior = c(
                                prior(normal(0.4,0.01))),
                              file = "./morph_bayes_prior_n150.rds") 


posterior_interval(morph_bayes_prior_n150) 

summary(morph_bayes_prior_n150, digits = 5) 

#####no, it doesn't include 0.4 in it's 95% Credible Interval.

######somewhere between 100-150 is how much data do we really need to overcome our prior belief.





     
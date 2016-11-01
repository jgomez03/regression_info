library(tidyverse)
population_data <- read_csv ("population_data.csv")

glimpse(population_data)

#get sample data
set.seed(1)#ensuring we same as david, saying start here in the number table
sample1_data <- sample_n(population_data, size=200)
glimpse(sample1_data)

#conduct SAMPLE regression
sample1_lm_results <- lm(performance ~ IQ + 1, data=sample1_data) #performance is criterion, IQ is the predictor, + 1 means gives us an intercept
#for this, we'll leave intercept OFF
sample1_lm_results <- lm(performance ~ IQ, data=sample1_data)
summary(sample1_lm_results)
#(Intercept) 50.59668 = elevation of line   
# (slope) 0.24233= angle of line

#two type of weights we use in regression - b weights/bvalue/unstandarzied regression coeffiecient/unstandardized slope/regression coefficient (raw data, data itself)
#second type: beta values/weights = same analysis/same math - when you see a beta weight it means they took performance column and transform it (run through Z formula, mean = -, SD = 1)
                                                            # do the same for IQ - means they were STANDARDIZED
#each column was standardized before running the analysis 

#get more spss like output with apa tables
library(apaTables)
apa.reg.table(sample1_lm_results)
#fit is saying, what is overall r squared? (can see correlation as fit index for the slope - how closely do points cluster around line)
##Results: Regression results using performance as the criterion
#Predictor       b       b_95%_CI beta  beta_95%_CI sr2 sr2_95%_CI     r  Fit
#(Intercept) 50.60** [45.40, 55.79]                                                       
#IQ  0.24**   [0.19, 0.29] 0.55 [0.43, 0.67] .30 [.20, .40] .55**                
#R2 = .304**
# 95% CI[.20,.40]

###now we can do with POPULATION, see how sample results differ from population results
##POPULATION regression
population_lm_results <- lm(performance ~ IQ, data=sample1_data)
summary(population_lm_results)
#population data results:
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 50.59668    2.63419  19.208   <2e-16 ***
#IQ           0.24233    0.02605   9.304   <2e-16 ***

#when there's only 1 predictor, the beta weight and the regular correlation are the same value (if there's more than one, that's not true)


###Finding score for single person - predicted value for a single person
x_axis_range <- data.frame(IQ=c(120))

CI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval="confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)

#CI_data <- sample1_lm_results - put in regression line, i.e., sample 1 results
#newdata=x_axis_range (this is the value of 120), 
#interval="confidence", level=.95) - with a CI of 95%
#dont need to use bind if you remember that you set it as 120
#figured out CI for that one person

###Predicted value for entire x axis range
min_predictor <- min(sample1_data$IQ)
max_predictor <- max(sample1_data$IQ) # $ means we are CHOOSING the IQ column 
# if we were to not put min/max we'd get ALL values in the IQ column 

#seq in R - seq(11,21,by=2) - 11, 13, 15 .... etc

x_axis_range <- data.frame( IQ=seq(min_predictor, max_predictor, by=.5) )
CI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval="confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))

PI_data <- predict(sample1_lm_results, newdata=x_axis_range, interval="prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
head(CI_data)
head(PI_data)

reg_plot <- ggplot(sample1_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ,y=fit, ymin=lwr, ymax=upr),stat="identity")
print(reg_plot)

#now with CI data
reg_plot <- ggplot(sample1_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ,y=fit, ymin=lwr, ymax=upr),stat="identity")
print(reg_plot)

#don't need to go through all this work if you JUST want to the CONFIDENCE INTERVAL - doesnt have a shortcut for PI intervals 
### dont understand this .... need to review the shortcutand what to change in code 
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE)
# does the same as reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ,y=fit, ymin=lwr, ymax=upr),stat="identity")
##see below
reg_plot <- ggplot(sample1_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE)
print(reg_plot)

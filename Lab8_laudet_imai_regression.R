# Laudet & Imai Chapter 4: Regression Analysis

# GDP = gross demestic product (monetary # of goods produced and services provided during X time)

# Hypothesis-- nighttime light emissions increase: GDP increase, too, for a specific country?
# GDP growth = outome variable, change in nighttime light emissons = predictor variable

co <- read.csv(file = "https://raw.githubusercontent.com/ellaudet/DSS/master/countries.csv")
head(co)
dim(co) # total number of observations --> 170 countries
# country = character variable
# gdp & prior_gdp = country's GDP at X point in time (in trillions of local currency)
# light and prior_light are average nighttime emissions at X point in time (scale 0 - 63)
# Ie. USA. 2005-2006: GDP = $11 trillion, light = 4.2. 1992-1993: GDP = $7 trillion, light = 4.5


# how much does GDP change?
plot(x = co$prior_gdp, y=co$gdp) # scatter plot of all gdp associations btw the two variables
# positive association, appears to be very linear
cor(co$gdp, co$prior_gdp) # computes correlation
    # [1] 0.9903451
fit <- lm(co$gdp~co$prior_gdp) # fits linear model
    # (Intercept)  co$prior_gdp  
    #   0.7161        1.6131 
# which tells us...:
# estimated intercept coefficient indicates that we may predict that 
    # average GDP is 0.72 trillion currency units on average
# and the estimated slope coefficient indicates that an increase in prior GDP of
    # 1 trillion local currency units is assc. with a predicted GDP increase of
    # 1.61 trillion local currency units, on average
# now, add the fitted line to the scatter plot
abline(fit) # add fitten linear model line to scatter plot

# two types of predictions we might want to make
# 1. predict average value of outcome variable given a value of the predictor
# 2. predict the average change in outcome variable 
    # associated with a change in the value of the predictor


# what about dealing with skew?
    # good idea to transform the variable by taking its natural log
# create log-transformed GDP variables
co$log_gdp <- log(co$gdp) # transform gdp and save it in the same df (co)
co$log_prior_gdp <- log(co$prior_gdp) # transform prior_gdp, save in co
# visualize transformations using histograms
hist(co$gdp)
hist(co$log_gdp)
hist(co$prior_gdp)
hist(co$log_prior_gdp)
# and create new scatterplots to compare
plot(x = co$prior_gdp, y = co$gdp)
plot(x = co$log_prior_gdp, y = co$log_gdp)
# new correlation coefficient?
cor(co$log_gdp, co$log_prior_gdp)
    # [1] 0.9982696 <- helped!
#and new linear model
lm(co$gdp~co$prior_gdp)
    # (Intercept)  co$prior_gdp  
    #   0.7161        1.6131 


# predicting GDP using light emmissions
# create GDP percentage change variable
co$gdp_change <- ((co$gdp - co$prior_gdp)/co$prior_gdp) * 100
# create light percentage change variable
co$light_change <- ((co$light - co$prior_light)/co$prior_light) * 100
head(co) # check!
# and create histograms of the light and GDP change percentages
hist(co$gdp_change)
hist(co$light_change)
# create scatterplot comparing how the two change variables relate
plot(x = co$light_change, y = co$gdp_change)
# and a correlation coefficient...
cor(co$gdp_change, co$light_change)
    # [1] 0.4577672
# and now for the linear model, for estimations of gdp based on light emissions
lm(gdp_change~light_change, data = co) # just a different format
    #(Intercept)  light_change  
    # 49.8202        0.2546  


# how well is the model fit? 
    # using R2, coefficient of determination (range 0-1)
# for each predictor model:
cor(co$gdp, co$prior_gdp)^2 # model 1
    # [1] 0.9807834
cor(co$log_gdp, co$log_prior_gdp)^2 # model 2
    # [1] 0.9965422
cor(co$gdp_change, co$light_change)^2 # model 3
    # [1] 0.2095508
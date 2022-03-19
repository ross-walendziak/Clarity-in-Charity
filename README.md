# Clarity-in-Charity
Using big data econometrics to optimize a direct marketing advertising campaign

# Donation Estimator | Project Overview
* Created various classification and regression models for a theoretical charity to optimize profits from a direct marketing campaign.
* Each individual mailing to a potential donor costs the charity $2.
* On average, without application of any predicitive models, the response rate for the mailing campaign is 10% and each mailing sent to a potential donor is expected     to cost the charity $0.55. Thus, sending mailings out to donors using a simple random sample approach is a negative net present value proposition for the charity.
* Our best classification model is the boost model and the best regression model is the random forest model.  
  * Expected profit outcomes change from -$0.55 per mailing without use of the models to +$0.09 per mailing with use of the models. The charity can become self-           sustaining.

# Code and Resources Used:
* R Version: 4.1.2
* Packages: tidyr, ggplot2, psych, caret, hmisc, xlsx, writexl, car, mlogit, MASS, pscl, AER, boot, purrr, pastecs, gam, e1071, tree, rpart, randomForest, gbm, leaps, glmnet, pls
* Text: [An Introduction to Statistical Learning](https://static1.squarespace.com/static/5ff2adbe3fe4fe33db902812/t/6062a083acbfe82c7195b27d/1617076404560/ISLR%2BSeventh%2BPrinting.pdf)

# Data Source:
https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/charity.csv

# Data Description:

* Total Obs (N): 8,009

* Dependent Variables:
  * DONR: Classification Response Variable (1 = Donor, 0 = Non-donor)
  * DAMT: Prediction (Regression) Response Variable (Donation Amount in $)
* Independent Variables:
  * REG1 - REG4: Region (A “1” indicates the potential donor belongs to this region.)
  * HOME: (1 = homeowner, 0 = not a homeowner)
  * CHLD: Number of children
  * HINC: Household income (7 categories)
  * GENF: Gender (0 = Male, 1 = Female)
  * WRAT: Wealth Rating (The segments are denoted 0-9, with 9 being the highest wealth group and 0 being the lowest.)
  * AVHV: Average Home Value in potential donor's neighborhood in $ thousands
  * INCM: Median Family Income in potential donor's neighborhood in $ thousands
  * INCA: Average Family Income in potential donor's neighborhood in $ thousands
  * PLOW: Percent categorized as “low income” in potential donor's neighborhood
  * NPRO: Lifetime number of promotions received to date
  * TGIF: Dollar amount of lifetime gifts to date
  * LGIF: Dollar amount of largest gift to date
  * RGIF: Dollar amount of most recent gift
  * TDON: Number of months since last donation
  * TLAG: Number of months between first and second gift
  * AGIF: Average dollar amount of gifts to date

# Exploratory Data Analysis:

* The donor predictor variables for children (CHLD) and wealth rating (WRAT) have promising correlations with the donor classification response (DONR) and the donation   amount (DAMT) respont variables.
![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Corr%20-%20Donor%20Predictors.png)

* The negative correlation between DAMT and CHILD as well as the positive correlations between household income (HINC), wealth rating (WRAT) and DAMT are shown           visually below.

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Damt%20vs%20Chld%20hinc%20and%20wrat.png)

# Data Transformation:

* Subsetting the data:
  * Training Set: 3,984 Obs
  * Validation Set: 2,018 Obs
  * Test Set: 2,007 Obs

* Some explanatory variables exhibited a positive right skew.  The log of these variables was taken to approximate a more normal distribution.
  * avhv, incm, inca, tgif, and agif

* A new continuous variable for financial wellness is created (finWellness), calculated as the most recent gift amount (rgif) divided by the largest gift amount (lgif)   if lgif is not zero, zero otherwise.  Larger ratios should indicate recent upward financial mobility and increased ability to donate whereas lower ratios should       indicate recent financial hardship and less ability to donate.
* A new dummy variable (chldLess1) that returns true if the number of childeren in a household is one or less, zero otherwise.
* A new dummy variable (wealthGr6) that returns true if the wealth rating (WRAT) is six or above and returns false for WRAT categories less than six.

# Classification Model Results:

* The Boosting model produced the highest accuracy ((TP+TN)/N) in the validation set of all classification models considered.  It also has the largest expected profit. 

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Classification%20Model%20Results.png)

* Confusion Matrix for the Boost classificaton model on the validation set:

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Classification%20Confusion%20Matrix.png)

* Profit is maximized at aproximatly $12,000 by sending 1,218 donation requests to prospective donors:

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Boosting%20Profit.png)

# Regression Model Results:

* The Random Forest model produced the lowest mean squared error (MSE) in the validation set of all regression models considered.  The Lasso model was also a strong candidate for best fit, producing a slightly larger MSE but with a significantly lower standared error.

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Regression%20Model%20Results.png)

* Both the Random Forest and Lasso models attributed the greatest greatest explanatory power to the variables reg4, rgif, lgif, chld, and agifLog - as evident in the graphics below. 

* %IncMSE shows the percentage increase in MSE if that variable is removed from the Random Forest model.
* IncNodePurity shows the total decrease in node purity from that variable, averaged over all trees created in the random forest model.

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Random%20Forest%20Variable%20Importance.png)

* Coefficent estimates from the Lasso model are shown below, ranked in decending absoulte value order.

![](https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/Graphics/Lasso%20Regression%20Coefficient%20Estimates.png)

# Economic Implications:

* Using profit maximization as our model selection criteria, best results are achieved using the boosting model for classification and the random forest model for regression.
* The classification model predicts a response rate of 14.45%; 44.5% higher than the expected response rate before the model was implemented (10%).
* The regression model predicts an average donation of $14.22 across the test data set for those selected for mailing.
* If we restrict the mailing to only classification outcomes of expected donors, the average donation increases to $14.45. 
 * This yields an expected profit of $14.45*0.1445 - 2 = $0.09 per mailing !

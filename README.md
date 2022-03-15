# Clarity-in-Charity
Using big data econometrics to optimize a direct marketing add campaign

# Donation Estimator | Project Overview
* Created various classification and regression models for a theoretical charity to optimize profits from a direct marketing campaign.
* Each individual mailing costs the charity $2.
* On average, Without application of any predicitive models, the response rate for the mailing campaign is 10% and each mailing sent to a potential donor is expected to cost the charity $0.55. Thus, sending mailings out to donors using a simple random technique is a negative net present value proposition for the charity.
* Our best classification model is the boost model and the best regression model is the random forest model.  Expected profit outcomes change from -$0.55 per mailing without use of the models to +$0.09 per mailing with use of the models. The charity can become self-sustaining.

# Code and Resources Used:
* R Version: 4.1.2
* Packages: tidyr, ggplot2, psych, caret, Hmisc, xlsx, writexl, car, mlogit, MASS, pscl, AER, boot, purrr, pastecs, gam, e1071, tree, rpart, randomForest, gbm, leaps, glmnet, pls
* Text: [An Introduction to Statistical Learning](https://static1.squarespace.com/static/5ff2adbe3fe4fe33db902812/t/6062a083acbfe82c7195b27d/1617076404560/ISLR%2BSeventh%2BPrinting.pdf)

# Data Source:
https://github.com/ross-walendziak/Clarity-in-Charity/blob/main/charity.csv

# Data Description:
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

# Data Transformation:

# Classification Models:

# Regression Models:

# Economic Implications:

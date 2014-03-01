# http://www.kaggle.com/c/benchmark-bond-trade-price-challenge/data
setwd('~/Dropbox/STA135/Project')
load('train.rda')
library(MASS)

# basic information
nrow(data)
length(unique(data$bond_id))

# trade price versus curve based price
curve_diff = with(data, curve_based_price - trade_price)
curve_diff_below = data[curve_diff < 0]
curve_diff_above = data[curve_diff > 0]

# yield
current_yield = with(data, current_coupon / trade_price)
past_yield = vector(10)
for (i in 1:10) {
  temp = paste0('trade_price_last', i)
  past_yield[i] = current_coupon / temp
}

## CALL RISK
price.call = data$trade_price[data$is_callable == 1]
price.nocall = data$trade_price[data$is_callable == 0]
# see if call and nocall seem to be correlated
matrix.call = cbind(price.call, price.nocall)
cov(matrix.call)

# 2 sample confidence interval for difference in mean
# different sample sizes
xbar.call = mean(price.call)
xbar.nocall = mean(price.nocall)
n.call = length(price.call)
n.nocall = length(price.nocall)

norm.call = (price.call - ave.price.call) / sd.price.call
norm.nocall = (price.nocall - ave.price.nocall) / sd.price.nocall

# CIs for call risk mean
call.upper = ave.price.call + 1.96 * sd.price.call
call.lower = ave.price.call - 1.96 * sd.price.call
nocall.upper = ave.price.nocall + 1.96 * sd.price.nocall
nocall.lower = ave.price.nocall - 1.96 * sd.price.nocall

## Time 
# 1. Look at time_to_maturity
# 2. look at number of times an individual bond has been traded
#     if multiple times then better because longer length and more time points
split()
sapply(foo, nrow)
unique()
# 3. 


## Classification Ch. 11 or 12
# need to be able to split data into 2 or 3 categories
# need training and test data
# finding new categories for variables
  # use classification to break data into groups that you dont already
  # know should be groups (ie not a variable you already have)
# Possible Categories:
  # will the bond go up in price?
  # will the bond turn over quickly?
# from: https://www.stat.berkeley.edu/classes/s133/Class2a.html
data1 = data
rose = data$trade_price > data$trade_price_last1
rose10 = data$trade_price > data$trade_price_last10
data1 = cbind(data1, rose)
data1[is.na(data1)] = 0

foo = lda(rose ~ ., data = data1)
bar = predict(foo)
table(data1$is_callable, bar$class)

vlda = function(v, formula, data, cl) {
   require(MASS)
   grps = cut(1:nrow(data1), v, labels = FALSE)[sample(1:nrow(data1))]
   pred = lapply(1:v, function(i, formula, data1) {
	    omit = which(grps == i)
	    z = lda(formula, data = data[-omit, ])
            predict(z, data[omit, ])
	    }, formula, data)

   wh = unlist(lapply(pred,function(pp)pp$class))
   table(wh, cl[order(grps)])
}

tt = vlda(5, rose ~ ., data1, data1$rose)
# Error Rate of LDA
# tt = matrix(c(442570, 137614, 6580, 175914), nrow = 2, byrow = TRUE)
error.rate = sum(tt[row(tt) != col(tt)]) / sum(tt)

# Linear combos of original variables used for LDA
foo$scaling

# could use PCA and then classification
  # compare classifcation with and without PCA and see if there is a
  # difference
  # can do with covariates and std covariates and compare


## Clustering





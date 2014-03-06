setwd('~/Dropbox/STA135/Project')
load('train.rda')
load('test.rda')
library(MASS)
library(lattice)

## IDEAS FOR PROJECT
# classification trees if response is categorical
# regression trees if response is numerical

## Using trade price and/or curve based price to classify increase in
## trade price and/or curve based price

############
## FUNCTIONS
############

outlier.finder = function(index, data) {
  box = boxplot(data[[index]])
  outliers = box$out
  outlier0 = data[[index]] %in% outliers
  index0 = index.all[outlier0]
  return(index0)
}


##########################
#### DISCRIMINATE ANALYSIS
##########################

trade.price = train[grep('trade_price_*', names(train))]
based.price = train[grep('*based_price*', names(train))]

# STEP 0: create train and test data
 
# set seed and find indices for first have and second half of data
set.seed(1234)
n = nrow(train)
n.train = floor(n / 2)
n.test = n - n.train

# randomize which indices are used
train.index = sample(x = n, size = n.train)

# split data into train and test data
data.train = trade.price[train.index, ]
data.test = trade.price[-train.index, ]
# how to treat previous trade prices which have value NA?
# for this I removed any observations where >= 1 values were NA

trade.price.complete = data.train[complete.cases(data.train), ]

# STEP 1: Is the Data Normal?
# Answer: No, data has many extreme high and low values
# could break up into quantiles

## outliers

# FIRST TRY: use data as is (removing NA values)
x11()
qqnorm(trade.price[[1]])
qqline(trade.price[[1]])

# all look bad. also, all look very similar
# takes awhile to run, may want to avoid
x11()
par(mfrow = c(3, 4))
sapply(names(trade.price.complete), function(x) { 
       qqnorm(trade.price.complete[[x]], main = x); 
       qqline(trade.price.complete[[x]])})

mah.trade = mahalanobis(trade.price.complete, colMeans(trade.price.complete), 
                  cov(trade.price.complete))

# very bad.
x11()
qqplot(qchisq(seq(0, 1, length = nrow(trade.price)), ncol(trade.price)), mah.trade,
  main = 'Theoretical Chi-Sq Quantiles vs. Sample Quantiles for 1', 
  ylab = 'Mahalanobis Distance', xlab = 'Theoretical Chi-Sq') 


# SECOND TRY: remove outliers from previous step
# so. many. outliers.
index.all = 1:nrow(trade.price.complete)
row.numbers = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

outlier.list = sapply(row.numbers, outlier.finder, trade.price.complete)

outlier.index = index.all %in% unique(unlist(outlier.list))
no.outliers = trade.price.complete[!outlier.index, ]

# still not great, but much better!
qqnorm(no.outliers[[9]])
qqline(no.outliers[[9]])

mah.no.outliers = mahalanobis(no.outliers, colMeans(no.outliers), 
                  cov(no.outliers))

# still seems to be non-MVN
qqplot(qchisq(seq(0, 1, length = nrow(no.outliers)), ncol(no.outliers)), 
       mah.no.outliers)

# THIRD TRY: normalize columns data from previous step
# same result as without normalizing
no.out.norm = apply(no.outliers, 2, function(x) (x - mean(x)) / sd(x))

# reasonable
qqnorm(no.out.norm[[1]])
qqline(no.out.norm[[1]])

mah.no.out.norm = mahalanobis(no.out.norm, colMeans(no.out.norm), 
                  cov(no.out.norm))

# unreasonable, but much better
qqplot(qchisq(seq(0, 1, length = nrow(no.out.norm)), ncol(no.out.norm)), 
       mah.no.out.norm)

# conclusion: use non-normalized data but exclude outliers
#             not completely unreasonable because bond trade may want to
#             avoid buying/selling bonds with very strange values

## STEP 2: Prepare training data

# remove obs with NA values
trade.price.test = data.test[complete.cases(data.test), ]

# remove outliers
index.all.test = 1:nrow(trade.price.test)
row.numbers.test = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

outlier.list.test = sapply(row.numbers.test, outlier.finder, trade.price.test)

outlier.index.test = index.all.test %in% unique(unlist(outlier.list.test))
no.outliers.test = trade.price.test[!outlier.index.test, ]


## STEP 3: Try QDA anyway

# can't use with our test data because we don't have the current price,
# meaning we can't check the accuracy of our analysis
# remove current price from train since it is not in test

# create groupings for train and test data
rose.train = with(no.outliers, ifelse(trade_price < trade_price_last1, 0, 1))
rose.test = with(no.outliers.test, ifelse(trade_price < trade_price_last1, 0, 1))


# remove current price from training and test data
no.outliers.train = no.outliers[-1]
no.outliers.test = no.outliers.test[-1]

qda = qda(x = no.outliers.train, grouping = rose.train)

pred = predict(qda, newdata = no.outliers.test)
# results are dissapointing
confusion.table = table(pred$class, rose.test)
# error rate of 0.33 percent. could be worse?
error.rate = (confusion.table[[2]] + confusion.table[[3]] ) / sum(confusion.table)


#################
## OTHER ANALYSIS
#################

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



setwd('~/Dropbox/STA135/Project')
load('train.rda')
load('test.rda')
load('uniqueRows.rda')
load('priceTimeDiff.rda')
library(MASS)
library(lattice)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(doSNOW)
library(foreach)
library(randomForest)

ibrary(plyr)

## IDEAS FOR PROJECT
# classification trees if response is categorical
# regression trees if response is numerical
# LaTeX or Lyx for making things look pretty

# difference between trade times

# maybe analyze type 4 seperately than type 2 and 3

## Using trade price and/or curve based price to classify increase in
## trade price and/or curve based price

# for test data, use 10-fold cv

##############
## EXPLORATION
##############

trade.price = train[grep('trade_price_*', names(train))]
based.price = train[grep('*based_price*', names(train))]

# STEP 0: create train and test data
 
# set seed and find indices for first section and second section of data
set.seed(2718)
n = nrow(uniqueRows)
n.test = floor(n / 10)
n.train = n - n.test

# randomize which indices are used
train.index = sample(x = n, size = n.train)

# split data into train and test data
data.train = uniqueRows[train.index, ]
data.test = uniqueRows[-train.index, ]


# how to treat previous trade prices which have value NA?
# for this I removed any observations where >= 1 values were NA

trade.price.complete = data.train[complete.cases(data.train), ]


trade.price.id1 = trade.price[1:27,]
trade.price.id1 = trade.price[1:28,]
table(trade.price.id1[27,] %in% trade.price.id1[28,])
table(trade.price.id1[18,] %in% trade.price.id1[28,])
table(trade.price.id1[17,] %in% trade.price.id1[28,])
train$time_to_maturity[1]
train$time_to_maturity[27]

rawr = split(train, train$bond_id)
data.unique.rows = lapply(rawr, function(x) x[rev(seq(nrow(x), 1, -11)),])
uniqueRows = rbind.fill(data.unique.rows)
save(uniqueRows, file = 'uniqueRows.rda')

price.diff = uniqueRows[, grep('^trade_price', names(uniqueRows))]
price.diff = as.numeric(diff(t(price.diff)))

time.diff0 = uniqueRows[, grep('^received_time', names(uniqueRows))]
time.diff1 = diff(t(time.diff0))
time.diff2 = rbind(t(time.diff0)[1,], time.diff1)
time.diff3 = as.numeric(time.diff2)

priceTimeDiff = cbind(price.diff, time.diff3)
save(priceTimeDiff, file = 'priceTimeDiff.rda')

fairDiff = with(uniqueRows, trade_price - curve_based_price)



# subset.bestset = 

foo = sapply()

# seq(nrow, 1, -11)

# write.table(trade.price, "tradePrice.txt", sep = " ", col.names = FALSE, row.names = FALSE)
# 22994 rows contain overlap out of 762678
# done the other way, 758943 out of 762678
# nrow(train) - length(unique(train$bond)) = 758942


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

##################
## REGRESSION TREE 
##################

data.time = train[grep('received_time*', names(train))]
time_avg = rowMeans(train[grep('received_time*', names(train))])

train.time = cbind(train, time_avg)




fit.tree = tree(trade_price ~ ., data = data.train)
fit.pred = predict(fit.tree, data.test)

# correct on average, but lots of variance
plot(fit.pred, data.test$trade_price)
abline(0, 1)
sqrt(mean((fit.pred - data.test$trade_price) ^ 2))

# make variable names a-z and have key mapping temp variables to actual vars
summary(fit.tree)
plot(fit.tree)
text(fit.tree, pretty = 0)


##########
## BAGGING
##########

#http://www.milbo.org/rpart-plot/prp.pdf 

# don't include current variables (e.g. current size, current delay)
train.complete = data.train[complete.cases(data.train), ]
bad.indices = names(train.complete) %in% c('curve_based_price', 
                                           'received_time_diff_last1', 'trade_size',
                                           'trade_type')
train.complete = train.complete[ , !bad.indices]
test.complete = data.test[complete.cases(data.test), ]
test.complete = test.complete[ , !bad.indices]

registerDoSNOW(makeCluster(4, type = 'SOCK'))

rf = foreach(ntree = rep(7, 4), .combine = combine, 
             .packages = 'randomForest') %dopar%
             randomForest(train.complete[, -3], train.complete[[3]], 
                          ntree = ntree)

fairDiff.complete = with(train.complete, trade_price - curve_based_price) 

rf.fair = foreach(ntree = rep(7, 4), .combine = combine, 
             .packages = 'randomForest') %dopar%
             randomForest(train.complete[, -3], fairDiff.complete, 
                          ntree = ntree)

yhat.fair = predict(rf.fair, newdata = test.complete)
plot(yhat.fair, with(test.complete, trade_price - curve_based_price) )
abline(0, 1)

x11()
yhat.bag = predict(rf, newdata = test.complete)
plot(yhat.bag, test.complete[[3]])
abline(0, 1)

importance(rf.fair)
varImpPlot(rf.fair)
# m = p, aka Bagging
## USES WRONG DATA SET
bag.price = randomForest(trade_price ~ ., data = train.complete, 
                          mtry = ncol(train.complete) - 1, importance = TRUE)


time.data = train[ grep('(received_time*)|(^trade_price$)', names(train))]

time.stand = apply(time.data, 1, function(x) (x - mean(x)) / sd(x))


## CHECK PREDCITION USING REG TREE VS RF
# it is in fact the case that reg tree only predictions discrete categories
# whereas rf appears to predict individually
set.seed(1)
train = sample(1: nrow ( Boston ) , nrow ( Boston ) /2)
tree.boston = tree(medv ~ . , Boston , subset = train )

summary ( tree.boston )

yhat = predict ( tree.boston , newdata = Boston [ -train ,])
boston.test = Boston [ -train ,"medv"]
plot ( yhat , boston.test )
abline (0 ,1)
mean (( yhat - boston.test ) ^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
plot(yhat.rf, boston.test)
abline(0,1)
mean((yhat.rf-boston.test)^2)





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



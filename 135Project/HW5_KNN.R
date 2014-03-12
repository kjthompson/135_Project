load('digitsTrain.rda')
load('digitsTest.rda')
library(multicore)
library(class)

## QUESTION 1
data18 = sampleTrain[sampleTrain$label %in% c(1, 8), ]
data18.test = newTest[newTest$label %in% c(1, 8), ]

## QUESTION 2

pca.maker = function(dataSet, moreInfo = FALSE) {
  S = cov(dataSet)
  eig = eigen(S)

  pca = t(eig$vectors) %*% t(dataSet)
  pca = t(pca)
  colnames(pca) = paste0("PC", 1:ncol(dataSet))
      
  # How much does each component matter?
  var_explained = cumsum(eig$values) / sum(eig$values)
  eig.keep = var_explained < 0.95

  # keep PCs with cumsum less then 95, plus 1 to capture crossing point
  pca.keep = pca[ , 1:(table(eig.keep)[[2]] + 1)]

  if (moreInfo)
    return(list(pca.keep, var_explained, eig))
  else
    return(pca.keep)
}

pca = pca.maker(data18[-1])

## QUESTION 3

# distance between all observations of data set (here named pca)
distance = dist(pca,  upper = TRUE)
# convert distance object to matrix
dist = as.matrix(distance)
# dist includes observations with itself, so make those Inf
# meaning they will never be counted as a nearest neighbor
dist[dist == 0] = Inf

## pre: takes value of k, data = data set, groups = grouping for the data set
##      and type = type of distance to be calculated
## post: returns binary vector of whether each query point was correctly
##       classified
point.dist = function(k, data, groups, type) {
  n = nrow(data)
  # initialize vector
  error.vec = numeric(length = n)
  
  # LOOCV
  for (i in seq_len(n)) {
    dataTrain = data[-i, ]
    groups.train = groups[-i]
    queryPoint = data[i, ]
    groups.test = groups[i]
    
    dist.i = as.numeric(dist[, i])
    # remove infinite values (distance from itself to itself)
    dist.i = dist.i[is.finite(dist.i)]

    data.dist = as.data.frame(cbind(groups.train, dist.i))
    sort.dist = data.dist[order(data.dist[[2]]), ]
    
    sort.keep = sort.dist[1:k, 1]
    table = sort(table(sort.keep), decreasing = TRUE)

    if (length(table) > 1 && table[[1]] == table[[2]])
      class = (sample(as.numeric(c(names(table)[[1]], names(table[[2]]), 1))))
    else
      class = (as.numeric(c(names(table)[[1]])))
    
    error.vec[i] = (class == groups.test)
  }

  # filled binary vector of length n, 1 = correct, 0 = incorrect
  return(error.vec)
}

# Bob! You should use lapply rather than mclapply, you can use lapply,
# but it will not save you any time :(
k = mclapply(1:5, point.dist, pca, data18[[1]], 'euclid')
k.mat = matrix(unlist(k), ncol = 5)
k.accuracy = colMeans(k.mat)



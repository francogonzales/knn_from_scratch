# STA 141A
# Project 6
# Franco Gonzales
# 914953764
# 12/12/2018

setwd("/Users/franco/Desktop/R Files/141A/Project 6")

#Q1: Write a function read_digits()that reads a digits file into R. Your function must allow 
# users to specify the path to the file (training or test) that they want to read.


read_digits = function(path_file) { # Allow users to specify the path to the file
  file = read.table(path_file)
  names = names(file)
  names = tolower(names)
  names(file) = names
  colnames(file)[1] = "class"
  file
}


# -------------------------------------------------------------------------------------

#Q2: Explore the digits data:
#   * Display graphically what each digit (0 through 9) looks like on average.

train = read_digits("digits/train.txt")
test = read_digits("digits/test.txt")

# Let's see how the images look in general
train_matrix = as.matrix(test)
show_image = matrix(test2[1, 2:ncol(train)], 16, 16, byrow = T)
image(t(apply(show_image, 2, rev)), col=grey(seq(0,1,length=256)))

amount = table(train$class)
amount
# So the data has a ton of 0s and 1s with relatively equal amounts of everything else.

library(ggplot2)
train_plot = ggplot() + geom_density(aes(class), color = "red", data = train) +
  geom_density(aes(class), color = "blue", data = test) +
  ylim(0, .20) +
  scale_x_continuous(labels = as.character(0:9), breaks = (0:9))  +
  labs(x = "Class", title = "Distribution of Classes for Training Set")

train_separate = split(train, train$class)


means = lapply(train_separate, colMeans)
means = do.call(rbind, means)
means = as.matrix(means)


images = lapply(split(means, row(means)), function(x) t(apply(matrix(x[-1], 16, 16, byrow=T), 2, rev)))
img = do.call(rbind, lapply(split(images[1:10], 1:10), function(x) do.call(cbind, x)))
image(img, col=grey(seq(0,1,length=100)))

library(matrixStats)

matrix_trainsep = lapply(train_separate, as.matrix)
vars = as.data.frame(t(as.data.frame(lapply(matrix_trainsep, colVars))))
names = tolower(names(vars))
names(vars) = vars

colnames(vars)[1] = "class"
vars$class = c(0:9)

vars_matrix = as.matrix(vars)
images2 = lapply(split(vars_matrix, row(vars_matrix)), function(x) t(apply(matrix(x[-1], 16, 16, byrow=T), 2, rev)))
img2 = do.call(rbind, lapply(split(images2[1:10], 1:10), function(x) do.call(cbind, x)))
image(img2, col=grey(seq(0,1,length=100)))

# -------------------------------------------------------------------------------------

# Q3: Write a function predict_knn() that uses k-nearest neighbors to predict the label for 
# a point or collection of points. At a minimum, your function must have parameters for the
# prediction point(s), the training points, a distance metric, and k. 

train = read_digits("digits/train.txt")
test = read_digits("digits/test.txt")

distance = function(x, type) {   # Let's make this the distance metric that we put into
                                 # our KNN model. Let type = euclidean or mankowski or etc.
  measure = dist(x, method = type, upper = TRUE, diag = TRUE) 
  measure_mat = as.matrix(measure)
  return(measure_mat)
  
}

y = distance(rbind(test, train), "euclidean")
g = rownames(test)
distances = y[rownames(test), !colnames(y) %in% rownames(test)]

predict_knn = function(k, train, test, metric) { 
  # Where metric is the type of distance
  # that we want to use.
  # colmatrix is there for the columns that we want in the matrix for our function to predict.
  # Using cross-validation and comparing two different sets would have different matrices.
  # It helps to choose the specific columns that we want.
  digits = train[ ,1]


  # Returns us a matrix with 2007 rows with columns that
  # correspond with the distances (total of 7291)
  
  min_distances2 = apply(metric, 1, function(x){ # Now find the K-nearest neighbor
    smallest = order(x)[1:k]                        # for each row in our test set.
    label = digits[smallest]
    votes = table(label)
    
    winner = which.max(votes)
    names = as.numeric(names(winner))
    digs =  return(names)
    digs
  })
  return(min_distances2) # Gives us the digits after calculating the k-nearest neighbor.
  
}




# -------------------------------------------------------------------------------------

# Q4: Write a function cv_error_knn() that uses 10-fold cross-validation to estimate the 
# error rate for k-nearest neighbors. Briefly discuss the strategies you used to make your 
# function run efficiently.

distance_train = distance(train, "euclidian")


cv_error_knn = function(b, k, train_set, metric) {
  set = train_set[sample(nrow(train_set)), ]
  folds = cut(seq(1,nrow(set)),breaks=b,labels=FALSE)
  
  error_estimates = 0
  
  for(i in 1:b){
    
    indexes = which(folds == i, arr.ind = TRUE)
    testset = set[indexes,]
    trainset = set[-indexes,]
    m = metric[rownames(testset), rownames(trainset)]
    
    labs = predict_knn(k, trainset, testset, m)
    error_estimates[i] = 1 -  (sum(labs == testset[, 1])/nrow(testset))
  }

  return(mean(error_estimates))
}



# ---------------------------------------------------------------------------------------

# Q5: In one plot, display 10-fold CV error rates for all combinations of k= 1,...,15 and 
# two different distance metrics. Which combination of k and distance metric works best for
# this data set? Would it be useful to consider additional values of k?

m1 = distance_train
m2 = distance(train, "manhattan")
m3 = as.matrix(dist(train, "minkowski", p = 3))
k = 15

euc_error = function(b, k, data, metric1, metric2, metric3){
  means_euc = 0  
  for(i in 1:k){
    
    means_euc[i] = cv_error_knn(b, i, data, metric1)
  }
 
  df1  = data.frame(Errors = means_euc, k = 1:15, method = "Euclidean")  
  
  means_man = 0
  for(i in 1:k) {
    
    means_man[i] = cv_error_knn(b, i, data, metric2)
  }
  
  df2 = data.frame(Errors = means_man, k = 1:15, method = "Manhattan")
  
  means_min = 0
  for(i in 1:k) {
    means_min[i] = cv_error_knn(b, i, data, metric3)
  }
  df3 = data.frame(Errors = means_min, k = 1:15, method = "Minkowski")
  
  final = rbind(df1,df2,df3)
  return(final)
}

# Writing this is actually 100x more work than making separate variables and then binding
# but oh well haha.
library(ggplot2)

errors = euc_error(10, 15, train, m1, m2, m3)

ggplot(errors, aes(k, Errors, fill = method)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs( x = "Kth-Nearest Neighbors", y = "Error Rate", title = "Error Rates for Different k values and Distances", fill = "Distance Metric")

ggsave("error_train.jpeg", width = 8, height = 5)

# Find the best k by finding the k with the lowest error rate. Minkowski is the best metric.

best_k = subset(errors, metric == "Minkowski")

# Best k value is 1.

# -------------------------------------------------------------------------------------

# Q6: In one plot, display the test set error rates for all combinations of k= 1,...,15 
# and two different distance metrics. How does the result compare to the 10-fold CV error
# rates? What kinds of digits does the “best” model tend to get wrong?

dist_traintest_euc = distance(rbind(test,train), "euclidean")
metric_euc = dist_traintest[rownames(test), !colnames(dist_traintest) %in% rownames(test)]

dist_traintest_man = distance(rbind(test,train), "manhattan")
metric_man = dist_traintest_man[rownames(test), !colnames(dist_traintest_man) %in% rownames(test)]

dist_traintest_min = dist(rbind(test,train), "minkowski", p = 3)
metric_min = dist_traintest_min[rownames(test), !colnames(dist_traintest_min) %in% rownames(test)]
  
loop_test = function(k , train, test , metric, type) {
  error = 0
  for(i in 1:k) {
    digits = predict_knn(i, train, test, metric)
    error[i] =  1 - (sum(digits == test[,1])/nrow(test))
    
    
  }
  results = data.frame(Errors = error, k = 1:15, method = type)
  return(results)

}

errors_euc = loop_test(15, train, test, metric_euc, "Euclidean")
errors_man = loop_test(15, train, test, metric_man, "Manhattan")
errors_min = loop_test(15, train, test, metric_min, "Minkowski")
errors_all = rbind(errors_euc, errors_man, errors_min)

# Plot the data together to see which distance metric best suits our needs.

ggplot(errors_all, aes(k, Errors, fill = method)) + geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Values for k", y = "Error Rate", title = "Error Rate for Test Data", fill = "Distance Metric")

ggsave("testerrors.jpeg", width = 8, height = 5)


cvv_results =  subset(errors, method == "Minkowski")
cvv_results$`Type` =  "Cross Validation"

test_results = subset(errors_all, method == "Minkowski")
test_results$`Type` = "Actual"

compare = rbind(test_results, cvv_results)

ggplot(compare, aes(k, Errors, fill = Type)) + geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Values for k", y = "Error Rate", title = "Error Rate CV vs Error Rate Test Set")

# k = 3 had the lowest error rate.
test_digits = as.data.frame(table(test[,1]))
colnames(test_digits) = c("Class", "Count")
test_digits$`Source` = "Actual"

digits = predict_knn(3, train, test, metric_min)
predict_digits = as.data.frame(table(digits))
colnames(predict_digits) = c("Class", "Count" )
predict_digits$`Source` = "Predict"

compare2 = rbind(predict_digits, test_digits)


ggplot(compare2, aes(Class, Count, fill = Source)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Class", y = "Count", title = "Predicted Data vs Actual Data")

ggsave("predictvactual.jpeg", width = 8 , height = 5)

rowMeans(vars) # To see if lowest variances leads to accurate predictions for digits.




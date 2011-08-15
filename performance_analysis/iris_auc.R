library(e1071)
library(ROCR)

cat("iris dataset AUC value for one class (setosa)")

data(iris)

# set random ordering
rng = sample(150, 150)
train_vals = rng[1:15]
test_vals = rng[16:150]

gen_metrics <- function(class_name) {
    x <- subset(iris, select = -Species)
    y <- 1 * (subset(iris, select = Species) == class_name)

    train_x <- x[train_vals,]
    train_y <- y[train_vals,]

    test_x <- x[test_vals,]
    test_y <- y[test_vals,]

    svm_model <- svm(Species ~ ., data = data.frame(x,Species=y))
    pred <- predict(svm_model, test_x)
    pp <- prediction(pred, test_y)
    prf <- performance(pp, "auc")
    print(prf)

    plot(performance(pp, "tpr", "fpr"))
    prf@y.values
}

# iris data
gen_metrics('setosa')
X11()
gen_metrics('versicolor')
X11()
gen_metrics('virginica')

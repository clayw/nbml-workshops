library(e1071)
library(MASS)
library(ROCR)

cat("cats dataset prediction of sex")

data(cats)

# set random ordering
rng = sample(144, 144)
train_vals = rng[1:20]
test_vals = rng[21:144]

gen_metrics <- function(class_name) {
    x <- subset(cats, select = -Sex)
    y <- 1 * (subset(cats, select = Sex) == class_name)

    train_x <- x[train_vals,]
    train_y <- y[train_vals,]

    test_x <- x[test_vals,]
    test_y <- y[test_vals,]

    svm_model <- svm(Sex ~ ., data = data.frame(x,Sex=y))
    pred <- predict(svm_model, test_x)
    pp <- prediction(pred, test_y)
    prf <- performance(pp, "auc")
    print(prf)

    plot(performance(pp, "tpr", "fpr"))
    prf@y.values
}

# make female positive examples
gen_metrics('F')


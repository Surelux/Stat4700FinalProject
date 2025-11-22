#1
#a
# Read txt file and convert to data frame
BodyDF=data.frame(read.table("C:/Users/slipg/OneDrive/Desktop/StatFinal/body.dat.txt",header=F))
colnames(BodyDF) = c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                     "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                     "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth",
                     "Age", "Weight", "Height", "Gender")

Y=cbind(BodyDF$Age, BodyDF$Weight, BodyDF$Height, BodyDF$Gender)


X=cbind(BodyDF$BiacromialDiam,BodyDF$BiiliacDiam,BodyDF$BitrochantericDiam,BodyDF$ChestDepth,BodyDF$ChestDiam,BodyDF$ElbowDiam,BodyDF$WristDiam,BodyDF$KneeDiam,BodyDF$AnkleDiam,BodyDF$ShoulderGirth,BodyDF$ChestGirth,BodyDF$WaistGirth,BodyDF$NavelGirth,BodyDF$HipGirth,BodyDF$ThighGirth,BodyDF$BicepGirth,BodyDF$ForearmGirth,BodyDF$KneeGirth,BodyDF$CalfMaxGirth,BodyDF$AnkleMinGirth,BodyDF$WristMinGirth)
Y <- as.data.frame(Y)
X <- as.data.frame(X)
names(Y) <- c("Age", "Weight", "Height", "Gender")
names(Y)
names(X) <- c("BiacromialDiam","BiiliacDiam","BitrochantericDiam","ChestDepth","ChestDiam","ElbowDiam","WristDiam","KneeDiam","AnkleDiam","ShoulderGirth","ChestGirth","WaistGirth","NavelGirth","HipGirth","ThighGirth","BicepGirth","ForearmGirth","KneeGirth","CalfMaxGirth","AnkleMinGirth","WristMinGirth")
names(X)

boxplot(Height ~ Gender, 
        data = Y,
        xlab = "Gender", 
        ylab = "Height (cm)", 
        main = "Height by Gender")
# As we can see here the Value 1 has a higher average height and due to knowing
# that men have a higher avreg height compared to woman we know that 1 is the 
# numeric representation for the male gender and 0 for the femal gender.

#b
library(pls)

set.seed(123)
n <- nrow(X)
test_indices <- sample(1:n, 200)
train_indices <- setdiff(1:n, test_indices)

X_train <- X[train_indices, ]
X_test <- X[test_indices, ]
Y_train <- Y[train_indices, ]
Y_test <- Y[test_indices, ]

# Fit PCR model
pcr_model <- pcr(Weight ~ ., data = data.frame(Weight = Y_train$Weight, X_train),
                 scale = TRUE, validation = "CV")

# Fit PLS model
plsr_model <- plsr(Weight ~ ., data = data.frame(Weight = Y_train$Weight, X_train),
                   scale = TRUE, validation = "CV")
# It makes sense because things like hight with a large scale might otherwise 
# dominate the PCA/PLS components.

#c
summary(pcr_model)
summary(plsr_model)
# Both modles use the same amount of componants but the PLS modle has slightly 
# lower variance across the componants compared to the PCR modle

#d
validationplot(pcr_model, val.type = "MSEP")
validationplot(plsr_model, val.type = "MSEP")
# From what it appears is that 21 componants generats the lowest MSEP in the two
# modles 

#e
# From the above analysis I determin that those two modles will not allow that 
# therefore I will use laso which which zeros out certain factors
library(glmnet)

X_mat <- as.matrix(X_train)
Y_vec <- Y_train$Weight

cv_lasso <- cv.glmnet(X_mat, Y_vec, alpha = 1)
plot(cv_lasso)

best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(X_mat, Y_vec, alpha = 1, lambda = best_lambda)

#f
pcr_pred <- predict(pcr_model, newdata = X_test, ncomp = 5)
plsr_pred <- predict(plsr_model, newdata = X_test, ncomp = 4)
X_test_mat <- as.matrix(X_test)
lasso_pred <- predict(lasso_model, s = best_lambda, newx = X_test_mat)

actual <- Y_test$Weight

pcr_mse <- mean((pcr_pred - actual)^2)
plsr_mse <- mean((plsr_pred - actual)^2)
lasso_mse <- mean((lasso_pred - actual)^2)

c(PCR = pcr_mse, PLS = plsr_mse, Lasso = lasso_mse)
# PLS is our best modle to use due to its low MSE


#2
#a
library(randomForest)
set.seed(123)
n <- nrow(X)
test_indices2 <- sample(1:n, 200)
train_indices2 <- setdiff(1:n, test_indices2)

X_train2 <- X[train_indices2, ]
X_test2  <- X[test_indices2, ]
Y_train2 <- Y[train_indices2, ]
Y_test2  <- Y[test_indices2, ]

train_data <- data.frame(Weight = Y_train2$Weight, X_train2)
test_data  <- data.frame(Weight = Y_test2$Weight, X_test2)

# Bagging
bag_model <- randomForest(Weight ~ ., data = train_data, 
                          mtry = ncol(X_train), ntree = 500, 
                          importance = TRUE)

# Random Forest
rf_model <- randomForest(Weight ~ ., data = train_data, 
                         mtry = floor(sqrt(ncol(X_train))), 
                         ntree = 500, importance = TRUE)

bag_pred_matrix <- predict(bag_model, newdata = test_data, predict.all = TRUE)$individual
rf_pred_matrix  <- predict(rf_model, newdata = test_data, predict.all = TRUE)$individual

mse_vec <- function(pred_matrix, actual) {
  apply(pred_matrix, 2, function(preds) mean((preds - actual)^2))
}

bag_mse <- mse_vec(bag_pred_matrix, Y_test2$Weight)
rf_mse  <- mse_vec(rf_pred_matrix,  Y_test2$Weight)

plot(1:500, bag_mse, type = "l", col = "blue", ylim = range(c(bag_mse, rf_mse)),
     xlab = "Number of Trees", ylab = "Test MSE", main = "Test MSE vs. Number of Trees")
lines(1:500, rf_mse, col = "red")
legend("topright", legend = c("Bagging", "Random Forest"), col = c("blue", "red"), lty = 1)


#b
importance(bag_model)
importance(rf_model)

varImpPlot(bag_model, main = "Bagging Variable Importance")
varImpPlot(rf_model, main = "Random Forest Variable Importance")
# The top four variables are the same in both modles although they hold diffrent
# importance and are in a diffrent order

#c
rf_pred  <- predict(rf_model, newdata = test_data)
rf_mse_500  <- mean((rf_pred - Y_test$Weight)^2)
c(PCR = pcr_mse, PLS = plsr_mse, Lasso = lasso_mse, RandomForest = rf_mse_500)
# The PLS modle still has the lowest MSE therefor it is the best option.

#d
plot(rf_model, main = "OOB Error vs. Number of Trees (Random Forest)")
# No more would not be better but a couple less apper to be better


#3
library(ISLR2)
library(e1071)
set.seed(123)

data(OJ)

#a
train_indices <- sample(1:nrow(OJ), 800)
train_data2 <- OJ[train_indices, ]
test_data2  <- OJ[-train_indices, ]

#b
svm_linear <- svm(Purchase ~ ., data = train_data2, kernel = "linear", cost = 0.01, scale = TRUE)
summary(svm_linear)
# This tells us we have 422 support vectores out of 800
# and that we have 2 classes.

#c
train_pred_linear <- predict(svm_linear, train_data2)
test_pred_linear  <- predict(svm_linear, test_data2)

train_table <- table(Predicted = train_pred_linear, Actual = train_data2$Purchase)
test_table  <- table(Predicted = test_pred_linear,  Actual = test_data2$Purchase)

train_error_linear <- 1 - sum(diag(train_table)) / sum(train_table)
test_error_linear  <- 1 - sum(diag(test_table))  / sum(test_table)

c(TrainError = train_error_linear, TestError = test_error_linear)
# We have slightly less error when predicting using the train data set

#d
tune_out_linear <- tune(svm, Purchase ~ ., data = train_data2, kernel = "linear",
                        ranges = list(cost = c(0.01, 0.1, 1, 10)))

summary(tune_out_linear)
best_linear_model <- tune_out_linear$best.model

#e
train_pred_best <- predict(best_linear_model, train_data2)
test_pred_best  <- predict(best_linear_model, test_data2)

train_err_best <- mean(train_pred_best != train_data2$Purchase)
test_err_best  <- mean(test_pred_best != test_data2$Purchase)

c(TrainError = train_err_best, TestError = test_err_best)
# We have slightly less error when predicting using the test data set

#f
svm_radial <- svm(Purchase ~ ., data = train_data2, kernel = "radial", cost = 0.01)
summary(svm_radial)
#2 classes and 629 support vectores

train_pred_radial <- predict(svm_radial, train_data2)
test_pred_radial  <- predict(svm_radial, test_data2)

c(Train = mean(train_pred_radial != train_data2$Purchase),
  Test = mean(test_pred_radial  != test_data2$Purchase))

tune_out_radial <- tune(svm, Purchase ~ ., data = train_data2, kernel = "radial",
                        ranges = list(cost = c(0.01, 0.1, 1, 10)))
best_radial <- tune_out_radial$best.model

train_best_radial <- predict(best_radial, train_data2)
test_best_radial  <- predict(best_radial, test_data2)

c(TrainBest = mean(train_best_radial != train_data2$Purchase),
  TestBest  = mean(test_best_radial  != test_data2$Purchase))
# We have slightly less error when predicting using the train data set

#g
svm_poly <- svm(Purchase ~ ., data = train_data2, kernel = "polynomial", degree = 2, cost = 0.01)
summary(svm_poly)
#2 classes and 631 support vectores
train_pred_poly <- predict(svm_poly, train_data2)
test_pred_poly  <- predict(svm_poly, test_data2)

c(Train = mean(train_pred_poly != train_data2$Purchase),
  Test = mean(test_pred_poly  != test_data2$Purchase))

tune_out_poly <- tune(svm, Purchase ~ ., data = train_data2, kernel = "polynomial",
                      degree = 2, ranges = list(cost = c(0.01, 0.1, 1, 10)))
best_poly <- tune_out_poly$best.model

train_best_poly <- predict(best_poly, train_data2)
test_best_poly  <- predict(best_poly, test_data2)

c(TrainBest = mean(train_best_poly != train_data2$Purchase),
  TestBest  = mean(test_best_poly  != test_data2$Purchase))
# We have slightly less error when predicting using the train data set

#h
Test_radial = mean(test_pred_radial  != test_data2$Purchase)
TestBest_radial  = mean(test_best_radial  != test_data2$Purchase)
Test_poly = mean(test_pred_poly  != test_data2$Purchase)
TestBest_poly  = mean(test_best_poly  != test_data2$Purchase)
svm_results <- data.frame(
  Model = c("Linear (0.01)", "Linear (Tuned)",
            "Radial (0.01)", "Radial (Tuned)",
            "Poly (0.01)", "Poly (Tuned)"),
  Test_Error = c(test_error_linear, test_err_best,
                 Test_radial, TestBest_radial,
                 Test_poly, TestBest_poly)
)
print(svm_results)
# The best Results come from the Tuned Linear modle due tio having the lowest
# Test Error.


#4
library(ISLR2)
library(keras)

xdata <- data.matrix(NYSE[, c("DJ_return", "log_volume", "log_volatility")])
istrain <- NYSE[, "train"]
xdata <- scale(xdata)

lagm <- function(x, k = 1) {
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

arframe <- data.frame(
  log_volume = xdata[, "log_volume"],
  L1 = lagm(xdata, 1), L2 = lagm(xdata, 2),
  L3 = lagm(xdata, 3), L4 = lagm(xdata, 4),
  L5 = lagm(xdata, 5)
)

arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]
dow <- NYSE[-(1:5), "day_of_week"]

dow_oh <- model.matrix(~ factor(dow) - 1)

n <- nrow(arframe)
n_dow <- ncol(dow_oh)
dow_array <- array(NA, dim = c(n, 5, n_dow))
for (i in 1:5) {
  dow_array[, i, ] <- dow_oh
}

xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn, c(n, 3, 5))
xrnn <- xrnn[, , 5:1]
xrnn <- aperm(xrnn, c(1, 3, 2))

xall <- array(NA, dim = c(n, 5, 3 + n_dow))
xall[, , 1:3] <- xrnn
xall[, , 4:(3 + n_dow)] <- dow_array

model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 12, input_shape = list(5, dim(xall)[3]),
                   dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse"
)

history <- model %>% fit(
  xall[istrain,, ], arframe[istrain, "log_volume"],
  batch_size = 64, epochs = 75,
  validation_data = list(xall[!istrain,, ], arframe[!istrain, "log_volume"])
)

# Predict and compute test set R²
kpred <- predict(model, xall[!istrain,, ])
V0 <- var(arframe[!istrain, "log_volume"])
rsq <- 1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0
rsq
# A R^2 value of 0.4429193


#5
img_dir <- "C:/Users/slipg/OneDrive/Desktop/book_images"
image_names <- list.files(img_dir)
num_images <-length(image_names)
x <-array(dim = c(num_images, 224, 224, 3))
for (i in 1:num_images) {
    img_path <- paste(img_dir, image_names[i], sep = "/")
     img <-image_load(img_path, target_size = c(224, 224))
     x[i,,, ] <-image_to_array(img)
     }
 x <-imagenet_preprocess_input(x)
model <- application_resnet50(weights = "imagenet")
summary(model)
pred10 <-model %>% predict(x) %>%
imagenet_decode_predictions(top = 5)
names(pred10) <-image_names
print(pred10)
#Scores
# $Cat1.jpg
# class_name class_description      score
# 1  n03404251          fur_coat 0.19295615
# 2  n02096177             cairn 0.13855880
# 3  n02112350          keeshond 0.11867543
# 4  n02445715             skunk 0.09808522
# 5  n02123394       Persian_cat 0.06048730
# 
# $Cat2.jpg
# class_name class_description      score
# 1  n02123045             tabby 0.28807220
# 2  n02127052              lynx 0.09692022
# 3  n04239074      sliding_door 0.09176992
# 4  n02123394       Persian_cat 0.07446904
# 5  n04589890     window_screen 0.06706259
# 
# $Cat3.jpg
# class_name class_description      score
# 1  n02123394       Persian_cat 0.34656721
# 2  n02112350          keeshond 0.21892142
# 3  n04589890     window_screen 0.09564587
# 4  n04590129      window_shade 0.03720363
# 5  n01882714             koala 0.03061247
# 
# $Dog.jpg
# class_name class_description      score
# 1  n02091134           whippet 0.56450498
# 2  n02091032 Italian_greyhound 0.21097673
# 3  n02091244      Ibizan_hound 0.12872258
# 4  n02091831            Saluki 0.02390808
# 5  n02110806           basenji 0.02198472
# 
# $dog2.jpg
# class_name              class_description      score
# 1  n02110806                        basenji 0.30218062
# 2  n02093256      Staffordshire_bullterrier 0.29307294
# 3  n02093428 American_Staffordshire_terrier 0.09952494
# 4  n02088364                         beagle 0.06500665
# 5  n02085620                      Chihuahua 0.06312467
# 
# $dog3.jpg
# class_name         class_description      score
# 1  n02085620                 Chihuahua 0.42213145
# 2  n02087046               toy_terrier 0.31679508
# 3  n02093256 Staffordshire_bullterrier 0.06516744
# 4  n02088364                    beagle 0.05902775
# 5  n04409515               tennis_ball 0.04894623
# 
# $duck1.jpg
# class_name  class_description      score
# 1  n01833805        hummingbird 0.75930417
# 2  n02536864               coho 0.07676112
# 3  n02168699 long-horned_beetle 0.03924390
# 4  n04067472               reel 0.03692535
# 5  n02169497        leaf_beetle 0.01505145
# 
# $horse.jpg
# class_name   class_description        score
# 1  n02389026              sorrel 9.995128e-01
# 2  n02087394 Rhodesian_ridgeback 1.670573e-04
# 3  n02422106          hartebeest 8.708149e-05
# 4  n02100583              vizsla 5.933364e-05
# 5  n04604644          worm_fence 5.769904e-05
# 
# $horse2.jpg
# class_name class_description       score
# 1  n02389026            sorrel 0.987130940
# 2  n02437616             llama 0.003740421
# 3  n02091032 Italian_greyhound 0.001690893
# 4  n02091244      Ibizan_hound 0.001525011
# 5  n02395406               hog 0.001321647
# 
# $Rand_Cat.jpg
# class_name class_description        score
# 1  n02445715             skunk 9.990155e-01
# 2  n01582220            magpie 1.737182e-04
# 3  n02443114           polecat 9.623604e-05
# 4  n02124075      Egyptian_cat 9.057549e-05
# 5  n02441942            weasel 8.466261e-05

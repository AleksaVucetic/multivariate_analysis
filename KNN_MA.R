# data load;
fund_indicators <- read.csv("C:\\Users\\Aleksa\\Downloads\\Fund_Indicators.csv")
fund_indicators$RETURN_OF_CAPITAL[fund_indicators$RETURN_OF_CAPITAL == 0] <- NA

str(fund_indicators)
summary(fund_indicators)

# since the IQID column is a system variable of the given software, it is not
# important to us for this task, so it is removed;
fund_indicators$IQID <- NULL

# as we saw that certain variables that should represent numeric values were
# represented as char values, they had to be converted to num of factor values for
# further manipulation;
fund_indicators$COMMITMENT <- as.numeric(fund_indicators$COMMITMENT)
fund_indicators$REMAINING_COMMITMENT <- as.numeric(fund_indicators$REMAINING_COMMITMENT)
fund_indicators$CASH_CALLED <- as.numeric(fund_indicators$CASH_CALLED)
str(fund_indicators)
summary(fund_indicators)

fund_indicators$FUND_STATUS <- as.factor(fund_indicators$FUND_STATUS)
fund_indicators$CURRENCY <- as.factor(fund_indicators$CURRENCY)
str(fund_indicators)
summary(fund_indicators)

# summarization of variables; here we see how many missing values each
# variable has;
summary(fund_indicators)

install.packages('Amelia')
library(Amelia)

# visualization of missing values;
par(mfrow = c(1,2))
missmap(obj = fund_indicators, main = "Funds", legend = FALSE)

install.packages("corrplot")

fund_indicators <- fund_indicators[!is.na(fund_indicators$CASH_DISTRIBUTED),]
summary(fund_indicators)

corr.data <- fund_indicators[2:10]
corr.data$CURRENCY <- NULL
corr.data$FUND_STATUS <- NULL

# visualization and creation of a correlation matrix;
library("corrplot")
kor.matrica <- cor(corr.data, use = "complete.obs")
kor.matrica
corrplot(kor.matrica, method = "number", type = "upper", diag = FALSE, number.cex=0.75, tl.cex = 0.01)
# after looking at the correlation matrix, it was decided to exclude the
# REMAINING_COMMITMENT and CASH_CALLED variables from further observation,
# since they are directly dependent on the COMMITMENT variable
# (this makes sense in the real world as well);
fund_indicators$REMAINING_COMMITMENT <- NULL
fund_indicators$CASH_CALLED <- NULL

summary(fund_indicators)

# testing the normality of variables (num type) to determine whether to
# replace missing values with mean or median (when it does not have a normal
# distribution, we use the median);
shapiro.test(fund_indicators$COMMITMENT)
shapiro.test(fund_indicators$ADJUSTED_NAV)
shapiro.test(fund_indicators$RETURN_OF_CAPITAL)
shapiro.test(fund_indicators$NET_CASH)
shapiro.test(fund_indicators$CASH_DISTRIBUTED)

median.commitment <- median(x = fund_indicators$COMMITMENT, na.rm = TRUE)
median.adjusted_nav <- median(x = fund_indicators$ADJUSTED_NAV, na.rm = TRUE)
median.return_of_capital <- median(x = fund_indicators$RETURN_OF_CAPITAL, na.rm = TRUE)
median.net_cash <- median(x = fund_indicators$NET_CASH, na.rm = TRUE)
median.cash_distributed <- median(x = fund_indicators$CASH_DISTRIBUTED, na.rm = TRUE)

fund_indicators$COMMITMENT[is.na(fund_indicators$COMMITMENT)] <- median.commitment
sum(is.na(fund_indicators$COMMITMENT))
fund_indicators$ADJUSTED_NAV[is.na(fund_indicators$ADJUSTED_NAV)] <- median.adjusted_nav
sum(is.na(fund_indicators$ADJUSTED_NAV))
fund_indicators$RETURN_OF_CAPITAL[is.na(fund_indicators$RETURN_OF_CAPITAL)] <- median.return_of_capital
sum(is.na(fund_indicators$RETURN_OF_CAPITAL))
fund_indicators$NET_CASH[is.na(fund_indicators$NET_CASH)] <- median.net_cash
sum(is.na(fund_indicators$NET_CASH))
fund_indicators$CASH_DISTRIBUTED[is.na(fund_indicators$CASH_DISTRIBUTED)] <- median.cash_distributed
sum(is.na(fund_indicators$CASH_DISTRIBUTED))

summary(fund_indicators)

# after this step we see that none of the variables have missing values and
# that this dataset is ready for manipulation;
# also, we need to set the target variable;
commitment.3Q <- quantile(fund_indicators$COMMITMENT, 0.75)
fund_indicators$HIGH_COMMITMENT <- ifelse(test = fund_indicators$COMMITMENT > commitment.3Q,
                                          yes = 'Yes',
                                          no = 'No')
fund_indicators$HIGH_COMMITMENT <- as.factor(fund_indicators$HIGH_COMMITMENT)
str(fund_indicators)

fund_indicators <- fund_indicators[,-2]








# -------------------- KNN CLUSTERIZATION -------------------- #
# we check whether numerical variables have outliers;
boxplot(fund_indicators$ADJUSTED_NAV)
boxplot(fund_indicators$RETURN_OF_CAPITAL)
boxplot(fund_indicators$NET_CASH)
boxplot(fund_indicators$CASH_DISTRIBUTED)

str(fund_indicators)
numeric.vars <- c(2:4,7)

apply(X = fund_indicators[,numeric.vars],
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))
# we see that each numerical variable has about 20% of outliers, except for
# the CASH_DISTRIBUTED variable, which has more; therefore, instead of
# normalization, we will use standardization for data scaling;

apply(X = fund_indicators[,numeric.vars],
      MARGIN = 2,
      FUN = shapiro.test)
# the null hypothesis of this test is that the sample is from a normal
# distribution; if p > 0.05, we assume that the null hypothesis is valid and
# that the sample is from a normal distribution; otherwise, the sample does
# not have a normal distribution, which is the case with all five numerical
# variables in our example; that's why we will standardize all 5 numerical
# variables with median and IQR;

fund_indicators.standardized <- apply(X = fund_indicators[,numeric.vars],
                                      MARGIN = 2,
                                      FUN = function(x) scale(x, center = median(x), scale = IQR(x)))
fund_indicators.standardized <- as.data.frame(fund_indicators.standardized)
summary(fund_indicators.standardized)

levels(fund_indicators$FUND_STATUS)
levels(fund_indicators$CURRENCY)

fund_indicators.standardized$FUND_STATUS <- as.integer(fund_indicators$FUND_STATUS)
fund_indicators.standardized$CURRENCY <- as.integer(fund_indicators$CURRENCY)

fund_indicators.standardized$HIGH_COMMITMENT <- fund_indicators$HIGH_COMMITMENT
str(fund_indicators.standardized)
summary(fund_indicators.standardized)



# creation of training and test dataset;
install.packages("caret")
library(caret)

set.seed(222)
train.indices <- createDataPartition(fund_indicators.standardized$HIGH_COMMITMENT, p = 0.8, list = FALSE)
train.podaci <- fund_indicators.standardized[train.indices,]
test.podaci <- fund_indicators.standardized[-train.indices,]









# ---------------- Model Creation --------- #

library(class)

evaluacione.metrike <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

library(e1071)

# cross-validation parameter definition;
numFolds = trainControl(method = "cv", number = 10)
numFolds
cpGrid = expand.grid(.k = seq(from = 3, to = 25, by = 2))
cpGrid

set.seed(222)

# cross-validation;
knn.cv <- train(x = train.podaci[,-7],
                y = train.podaci$HIGH_COMMITMENT,
                method = "knn",
                trControl = numFolds,
                tuneGrid = cpGrid)
knn.cv

# visualization of cross-validation-a;
plot(knn.cv)

best_k <- knn.cv$bestTune$k

knn.pred <- knn(train = train.podaci[,-7],
                  test = test.podaci[,-7],
                  cl = train.podaci$HIGH_COMMITMENT,
                  k = best_k)
knn.pred

# confusion matrix creation;
knn.cm <- table(true = test.podaci$HIGH_COMMITMENT, predicted = knn.pred)
knn.cm

# evaluation metrics;
knn.evaluacije <- evaluacione.metrike(knn.cm)
knn.evaluacije

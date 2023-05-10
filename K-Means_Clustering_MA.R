# data load;
fund_indicators <- read.csv("C:\\Users\\Aleksa\\Downloads\\Fund_Indicators.csv")

fund_indicators$RETURN_OF_CAPITAL[fund_indicators$RETURN_OF_CAPITAL == 0] <- NA
# removing the system variable;
fund_indicators$IQID <- NULL

summary(fund_indicators)
str(fund_indicators)

# as we saw that certain variables that should represent numeric values were
# represented as char values, they had to be converted to num of factor values for
# further manipulation;
fund_indicators$COMMITMENT <- as.numeric(fund_indicators$COMMITMENT)
fund_indicators$REMAINING_COMMITMENT <- as.numeric(fund_indicators$REMAINING_COMMITMENT)
fund_indicators$CASH_CALLED <- as.numeric(fund_indicators$CASH_CALLED)
fund_indicators$FUND_STATUS <- as.factor(fund_indicators$FUND_STATUS)
fund_indicators$CURRENCY <- as.factor(fund_indicators$CURRENCY)

str(fund_indicators)
summary(fund_indicators)

library(Amelia)

# visualization of missing values;
par(mfrow = c(1,2))
missmap(obj = fund_indicators, main = "Funds", legend = FALSE)

# instances with NA values being deleted;
fund_indicators <- fund_indicators[!is.na(fund_indicators$CASH_DISTRIBUTED),]
summary(fund_indicators)

par(mfrow = c(1,2))
missmap(obj = fund_indicators, main = "Funds", legend = FALSE)

corr.data <- fund_indicators[2:10]
corr.data$CURRENCY <- NULL
corr.data$FUND_STATUS <- NULL

# visualization and creation of corelation matrix;
library("corrplot")
kor.matrica <- cor(corr.data, use = "complete.obs")
kor.matrica
corrplot(kor.matrica, method = "number", type = "upper", diag = FALSE, number.cex=0.75, tl.cex = 0.01)

# after looking at the correlation matrix, it was decided to exclude the
# REMAINING_COMMITMENT and CASH_CALLED variables from further observation,
# since they are directly dependent on the COMMITMENT variable (this makes
#sense in the real world as well);
fund_indicators$REMAINING_COMMITMENT <- NULL
fund_indicators$CASH_CALLED <- NULL
summary(fund_indicators)

str(fund_indicators)
numeric.vars <- c(2:5,8)

# checking the normality of numerical variables;
apply(X = fund_indicators[,numeric.vars],
      MARGIN = 2,
      FUN = shapiro.test)
# none of the numerical variables has a normal distribution, therefore we
# change their NA values by the median;

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

non.numeric.vars <- c(1,6,7)

# since outliers negatively affect the entire method, it is necessary to
# identify them;
apply(X = fund_indicators[,-non.numeric.vars],
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))

library(DescTools)

# visualization of outliers for the Winsorizing technique;
boxplot(fund_indicators$COMMITMENT, xlab='COMMITMENT')
boxplot(fund_indicators$ADJUSTED_NAV, xlab='ADJUSTED_NAV')
boxplot(fund_indicators$RETURN_OF_CAPITAL, xlab='RETURN_OF_CAPITAL')
boxplot(fund_indicators$NET_CASH, xlab='NET_CASH')
boxplot(fund_indicators$CASH_DISTRIBUTED, xlab='CASH_DISTRIBUTED')

# using the Winsorizing technique to remove outliers;
fund_indicators$COMMITMENT <- Winsorize(fund_indicators$COMMITMENT,
                       probs = c(0,0.85))
fund_indicators$ADJUSTED_NAV <- Winsorize(fund_indicators$ADJUSTED_NAV,
                                        probs = c(0.05,0.86))
fund_indicators$RETURN_OF_CAPITAL <- Winsorize(fund_indicators$RETURN_OF_CAPITAL,
                                        probs = c(0,0.94))
fund_indicators$NET_CASH <- Winsorize(fund_indicators$NET_CASH,
                                          probs = c(0.17,0.87))
fund_indicators$CASH_DISTRIBUTED <- Winsorize(fund_indicators$CASH_DISTRIBUTED,
                                        probs = c(0,0.86))

apply(X = fund_indicators[,-non.numeric.vars],
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))

summary(fund_indicators)

# when we sorted out all the outliers, it is necessary to normalize, i.e.
# reduce all values of each variable to the interval from 0 to 1;

normalizacija <- function( feature ) {
   if ( sum(feature, na.rm = T) == 0 ) feature
   else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

fund_indicators.norm <- as.data.frame(apply(fund_indicators[,-non.numeric.vars],
                                            2,
                                            normalizacija))
# checking;
summary(fund_indicators.norm)
evaluacione.metrike <- data.frame()

# Elbow method;
for (k in 2:8) {
   set.seed(111)
   km.res <- kmeans(x = fund_indicators.norm,
                    centers = k,
                    iter.max = 20,
                    nstart = 1000)
   evaluacione.metrike <- rbind(evaluacione.metrike,
                              c(k, km.res$tot.withinss, km.res$betweenss/km.res$totss))
}

names(evaluacione.metrike) <- c("k", "tot.within.ss", "ratio")
evaluacione.metrike

# visualization of Elbow method;
install.packages("ggplot2")
library("ggplot2")
ggplot(data = evaluacione.metrike, aes(x = k, y = tot.within.ss)) +
   geom_line() + labs(x = "\nK (cluster number)",
     y = "Total Within Cluster Sum of Squares\n",
     title = "Reduction in error for different values of K\n") +
   theme_bw() +
   scale_x_continuous(breaks = seq(from=0, to=8, by=1))

izracunaj.razliku <- function(values) {
   dif <- list()
   dif[[1]] <- NA
   for(i in 1:(length(values)-1)) {
      dif[[i+1]] <- abs(values[i+1] - values[i])
   }
   unlist(dif)
}

# calculating the difference between every two instances from the
# evaluacione.metrike data frame to determine the optimal number of clusters;
data.frame(k = c(2:8),
           tot.within.ss.delta = izracunaj.razliku(evaluacione.metrike$tot.within.ss),
           ration.delta = izracunaj.razliku(evaluacione.metrike$ratio))

set.seed(111)

# 3-cluster clusterization;
fund_ind.clusters <- kmeans(x = fund_indicators.norm,
                            centers = 3,
                            iter.max = 20,
                            nstart = 1000)

# predefined (outsourced) function;
summary.stats <- function(feature.set, clusters, cl.num) {
   sum.stats <- aggregate(x = feature.set,
                          by = list(clusters),
                          FUN = function(x) {
                             m <- mean(x, na.rm = T)
                             sd <- sqrt(var(x, na.rm = T))
                             paste(round(m, digits = 2), " (",
                                   round(sd, digits = 2), ")", sep = "")
                          })
   sum.stat.df <- data.frame(cluster = sum.stats[,1],
                             freq = as.vector(table(clusters)),
                             sum.stats[,-1])
   sum.stats.transpose <- t( as.matrix(sum.stat.df) )
   sum.stats.transpose <- as.data.frame(sum.stats.transpose)
   attributes <- rownames(sum.stats.transpose)
   sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
   colnames(sum.stats.transpose) <- c( "attributes", rep("Mean (SD)", cl.num) )
   rownames(sum.stats.transpose) <- NULL
   sum.stats.transpose
}

# cluster center evaluation;
sum.stats <- summary.stats(feature.set = fund_indicators.norm,
                           clusters = fund_ind.clusters$cluster,
                           cl.num = 3)
sum.stats

# cluster visualization;
install.packages("ggpubr")

# predefined (outsourced) function;
create_attr_boxplot <- function(df, attribute, clust_var) {
   ggplot(data = df,
          mapping = aes(x=.data[[clust_var]],
                        y=.data[[attribute]],
                        fill=.data[[clust_var]])) +
      geom_boxplot() +
      labs(y = attribute, x = "") +
      theme_classic()
}

# predefined (outsourced) function;
create_comparison_plots <- function(df, clust) {
   require(dplyr)
   require(ggpubr)
   df_clust <- df
   df_clust[['cluster']] <- clust
   boxplots <- lapply(colnames(df),
                      function(x) create_attr_boxplot(df_clust, x, 'cluster'))
   ggarrange(plotlist = boxplots,
             ncol = 3, nrow = 2,
             common.legend = TRUE, legend = "bottom",
             vjust = 1, hjust = -1, font.label = list(size=12))
}

# cluster visualization;
create_comparison_plots(df = fund_indicators.norm,
                        clust = as.factor(fund_ind.clusters$cluster))

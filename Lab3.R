#Activity 1
df <- read.csv("C:/Users/Chiao Ee/Downloads/fifa-21.csv")

df <- data.frame(Product = c('A','B','C','D','E'),Price = c(612,447,NA,374,831))
print(df)

df$Price[is.na(df$Price)] <- 0


titanic_train$Age

library(ggplot2)
library(dplyr)
library(cowplot)

ggplot(titanic_train, aes(Age))+
  geom_histogram(color = "#000000", fill = "#0099F8")+
  ggtitle("Variable distribution")+
  theme_classic()+
  theme(plot.title = element_text(size = 18))

value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age,
is.na(titanic_train$Age),0),
   imputed_mean = replace(titanic_train$Age, 
is.na(titanic_train$Age), mean(titanic_train$Age,na.rm = TRUE)),
   imputed_median = replace(titanic_train$Age,
is.na(titanic_train$Age), median(titanic_train$Age, na.rm = TRUE))
)

value_imputed

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 2
library(mice)
titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

mice_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
  imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
  imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age)

mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 3
library(missForest)

missForest_imputed <- data.frame(
  original = titanic_numeric$Age,
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)

missForest_imputed

h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()


plot_grid(h1, h2, nrow = 2, ncol = 2)

#Activity 4
library(titanic)
log_scale = log(as.data.frame(titanic_train$Fare))

library(caret)
process <- preProcess(as.data.frame(titanic_train$Fare),method=c("range"))

norm_scale <- predict(process, as.data.frame(titanic_train$Fare))

scale_data <- as.data.frame(scale(titanic_train$Fare))

#Activity 5
gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)

new_dat = data.frame(titanic_train$Fare, titanic_train$Sex, titanic_train$Embarked)
summary(new_dat)

library(caret)

dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))

glimpse(dat_transformed)

summary(new_dat$titanic_train.Fare)

bins <- c(-Inf, 7.91, 31.00, Inf)
bin_names <- c("Low", "Mid50", "High")

new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks = bins, labels = bin_names)

summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)
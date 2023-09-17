
#Loading relevant libraries for current session

library(readxl) # Read Excel
raw_data=read_excel("/Users/YUSUF/Desktop/PGPBABI/FRA 2/Individual Assignment/raw-data.xlsx",sheet = "raw data",na = "")
coydefault = raw_data

# 1. Exploratory Data Analysis

summary(coydefault)
str(coydefault)
dim(coydefault)
colnames(coydefault)
library(visdat)

vis_miss(coydefault)
vis_dat(coydefault)

library(skimr)
skim(coydefault)

library(DataExplorer)
DataExplorer::create_report(coydefault)


# Convert character var to numeric

coydefault_numtrans <- type.convert(coydefault,as.is = T) #converts all to numeric
str(coydefault_numtrans)




# distribution - plot
# Remove - Num and Deposits (accepted by commercial banks)   Borrowings       

# Lets drop num and networth
coydefault_var_remv_1_22 <- coydefault_numtrans[,-c(1,22)] 
library(ggplot2)
library(explore)# visualize - bivariate analysis
library(dplyr)
library(tidyverse)

# to view data distribution
p.data1 = coydefault_var_remv_1_22[,c(1:12)]
p.data1 %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

p.data2 = coydefault_var_remv_1_22[,c(13:24)]
p.data2 %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

p.data3 = coydefault_var_remv_1_22[,c(25:36)]
p.data3 %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

p.data4 = coydefault_var_remv_1_22[,c(37:48)]
p.data4 %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

p.data5 = coydefault_var_remv_1_22[,c(49:52)]
p.data5 %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap( ~ metric, scales = "free")

# check NA's and missing values
dim(coydefault_var_remv_1_22)
glimpse(coydefault_var_remv_1_22)
library(skimr)
skim(coydefault_var_remv_1_22)

library(ggplot2)
library(devtools)
library(visdat)

vis_miss(coydefault_var_remv_1_22)
vis_dat(coydefault_var_remv_1_22)

library(DataExplorer)
DataExplorer::create_report(coydefault_var_remv_1_22)

library(corrplot)
library(mlr)
library(explore)

#summary(coydefault$`Networth Next Year`)

rm(default) # to cross check whether var name default is already available
summary(coydefault_var_remv_1_22)
coydefault_numtrans$default = 
  as.factor(ifelse(coydefault_numtrans$`Networth Next Year`>0,0,1))
p.data_scaling = data.frame(scale(coydefault_var_remv_1_22))


# Outlier visualization

boxplot(p.data_scaling,col= "green", border = "darkblue", horizontal = FALSE)

p.data_trim = p.data_scaling

# capping and flooring

for (i in colnames(p.data_trim))
{
  x = p.data_trim[,i]
  qntl = quantile(x,probs = c(0.25,0.75), na.rm = T)
  cap = quantile(x, probs = c(0.05,0.95), na.rm = T)
  H = 1.5 * IQR(x, na.rm = T)
                x[x < (qntl[1] - H)] = cap[1]
                x[x > (qntl[2] + H)] = cap[2]
  p.data_trim[,i] = x
}

boxplot(p.data_trim,col= "green", border = "darkblue", horizontal = FALSE)

# Outlier treatment with MICE

library(mice)
p.data_mice = 
  mice(p.data_trim,
       method = 'rf',
       maxit = 0, printFlag = FALSE)

p.data_mice_imp = data.frame(complete(p.data_mice))
p.data_mice_imp$defualt = coydefault_numtrans$default
boxplot(p.data_mice_imp,col= "lightgreen", border = "darkblue", horizontal = FALSE)

# view after imputation
library(UpSetR) # for missing value treatments
library(tidyverse) # for data manipulation and viz
library(naniar)# for missing values
sum(is.na(p.data_mice_imp)) 
vis_miss(p.data_mice_imp)
unique (unlist (lapply (p.data_mice_imp, function (x) which (is.na (x)))))
gg_miss_upset(p.data_mice_imp) # comparative view of missing values in each variable
n_var_miss(p.data_mice_imp)

DataExplorer::create_report(p.data_mice_imp)


# New variable creation (Profitability Ratio, leverage Ratio, Liquidity Ratio & Company Size)

p.data.final <- data.frame(p.data_mice_imp)
p.data.final$NWCbyCL <-
  p.data.final$Net.working.capital / p.data.final$Current.liabilities...provisions
p.data.final$Borrowings_by_Total_asset <-
  p.data.final$Borrowings / p.data.final$Total.assets
p.data.final$profit_by_income <-
  p.data.final$Profit.after.tax / p.data.final$Total.income
p.data.final$equity_by_asset <-
  (p.data.final$Total.assets - p.data.final$Total.liabilities) / p.data.final$Total.assets

# Multicollinearity Check

str(p.data.final)
p.data.final_rem_1 = p.data.final[,-c(1)]

# model 1

model_1 = glm(defualt ~ .,data = p.data.final_rem_1,family = "binomial")
summary(model_1)

# linear model - general logistic regression

model_2 = glm(defualt ~ PBT +
              PAT.as...of.net.worth + Borrowings +
               Cumulative.retained.profits + Capital.employed +
                TOL.TNW + Current.ratio..times. +
                Debt.to.equity.ratio..times. +
                Cash.to.current.liabilities..times. + 
                Raw.material.turnover +
                profit_by_income, 
              data = p.data.final_rem_1,family = "binomial")
summary(model_2)


model_3 = glm(defualt ~ PBT + TOL.TNW + Current.ratio..times. 
              + Debt.to.equity.ratio..times.
              + Cash.to.current.liabilities..times.
              + Raw.material.turnover, data = p.data.final_rem_1,family = "binomial")
summary(model_3)


# #######

library(caret)
p.data.final_rem_1$prob <- model_3$fitted.values
p.data.final_rem_1$pred <- as.factor(ifelse(model_3$fitted.values < 0.53, 0, 1))
confusionMatrix(p.data.final_rem_1$defualt,p.data.final_rem_1$pred)



# Preparing the test data for processing
coydef_data_test <- read.csv("/Users/YUSUF/Desktop/PGPBABI/FRA 2/Individual Assignment/validation_data.csv")
coydef_data_test <- coydef_data_test[, -c(1, 22)]
coydef_data_test$default <- as.factor(coydef_data_test$Default...1)
coydef_data_test <- coydef_data_test[,-c(1)]

# Creating the new variables
colnames(coydef_data_test)
coydef_data_test$NWor_capital_by_Cur_liability <-
  coydef_data_test$Net.working.capital / coydef_data_test$Current.liabilities...provisions
coydef_data_test$Borrowings_by_Total_asset <-
  coydef_data_test$Borrowings / coydef_data_test$Total.assets
coydef_data_test$profit_by_income <-
  coydef_data_test$Profit.after.tax / coydef_data_test$Total.income
coydef_data_test$equity_by_asset <-
  (coydef_data_test$Total.assets - coydef_data_test$Total.liabilities) / coydef_data_test$Total.assets
coydef_data_test$pred <-
  as.factor(ifelse(predict(model_3, coydef_data_test, type = 'response') < 0.53,
                   0,
                   1))
confusionMatrix(coydef_data_test$default, coydef_data_test$pred)

coydef_data_test$prob <-
  predict(model_3, coydef_data_test, type = 'response')

#

library(dplyr)
rdf_order <- p.data.final_rem_1[order(p.data.final_rem_1$prob),]
coydef_data_test_order <- coydef_data_test[order(coydef_data_test$prob),]
rdf_order$decile <- ntile(rdf_order$prob, 10)
coydef_data_test_order$decile <- ntile(coydef_data_test_order$prob, 10)
decile_df <- data.frame()
for (i in 1:10) {
  tmp <- rdf_order[rdf_order$decile == i, ]
  cn <- confusionMatrix(tmp$defualt, tmp$pred)
  var <- data.frame(cn$overall['Accuracy'])
  names(var) <- c("col1")
  decile_df <- rbind(decile_df, c(var$col1, i))
  
}
names(decile_df) <- c("accuracy", "decile")
plot(
  decile_df$decile,
  decile_df$accuracy,
  type = "l",
  col = "blue",
  xlab = "Decile",
  ylab = "Accuracy",
  main = "Accuracy of Train Dataset in Deciles"
)
write.csv(decile_df, file = "/Users/YUSUF/Desktop/PGPBABI/FRA 2/Individual Assignment/Train_Data_set_Accuracy_in_deciles.csv", row.names = FALSE)
#
decile_df <- data.frame()
for (i in 1:10) {
  tmp <- coydef_data_test_order[coydef_data_test_order$decile == i, ]
  cn <- confusionMatrix(tmp$default, tmp$pred)
  var <- data.frame(cn$overall['Accuracy'])
  names(var) <- c("col1")
  decile_df <- rbind(decile_df, c(var$col1, i))
  
}
names(decile_df) <- c("accuracy", "decile")
plot(
  decile_df$decile,
  decile_df$accuracy,
  type = "l",
  col = "orange",
  xlab = "Decile",
  ylab = "Accuracy",
  main = "Accuracy of Test Dataset in Deciles"
)
write.csv(decile_df, file = "/Users/YUSUF/Desktop/PGPBABI/FRA 2/Individual Assignment/Test_Data_set_Accuracy_in_deciles.csv", row.names = FALSE)







# last_mile_2021_6_27.R based on diary171819_codebook_210206.R 

library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(mice)# imputing missing values
setwd("~/Papers/last_mile/last_mile_coding")
dir()
# diary171819.df = readRDS("diary171819_210206.rds")
# dim(diary171819.df)# number of payments
# length(unique(diary171819.df$id))# num resp
# 
# # subset to the min needed variables
# last_mile_1.df = subset(diary171819.df, select = c(id, weight_171819_1, hh_income, gender, education, age, chk_acnt_adopt, sav_acnt_adopt))
# dim(last_mile_1.df)# num trans
# names(last_mile_1.df)
# #
# # reduce transaction data to indiv data
# last_mile_2.df = last_mile_1.df[!duplicated(last_mile_1.df$id), ]
# dim(last_mile_2.df)
# # save
# saveRDS(last_mile_2.df, "last_mile.rds")

# read RDS
last_3.df = readRDS("last_mile.rds")
names(last_3.df)
str(last_3.df)

# rescale weights to = num resp
last_4.df = last_3.df
sum(last_4.df$weight_171819_1)
nrow(last_4.df)
last_4.df$weight = nrow(last_4.df)*last_4.df$weight_171819_1/sum(last_4.df$weight_171819_1)
head(last_4.df)
sum(last_4.df$weight)
last_5.df = subset(last_4.df, select = -c(id, weight_171819_1))# delete original weights and id
str(last_5.df)

## find missing values 
last_6.df = last_5.df
md.pattern(last_6.df, plot=F)
# only income has NAs => impute
100*sum(is.na(last_6.df$hh_income))/nrow(last_6.df)# percentage of missing incomes
#
# impute 
last_6_temp.df = mice(last_6.df)
summary(last_6_temp.df)
#
last_7.df = complete(last_6_temp.df)
names(last_7.df)
anyNA(last_7.df)
dim(last_7.df)
head(last_7.df)

## start plot unbanked versus income
last_8.df = last_7.df
# can someone have a saving accounts but no checking?
nrow(subset(last_8.df, sav_acnt_adopt==1 & chk_acnt_adopt==0))

# add a new column "banked" if either checking or banking
last_9.df = last_8.df %>% mutate(banked = ifelse(chk_acnt_adopt==1 | sav_acnt_adopt==1, 1, 0) ) 
# perc of banked
100*nrow(subset(last_9.df, banked==1))/nrow(last_9.df)
# perc of checking
100*nrow(subset(last_9.df, chk_acnt_adopt==1))/nrow(last_9.df)
# perc of savings
100*nrow(subset(last_9.df, sav_acnt_adopt==1))/nrow(last_9.df)

# add a new column unbanked
last_10.df = last_9.df %>% mutate(unbanked = ifelse(banked==0, 1, 0))
# perc of unbanked
100*nrow(subset(last_10.df, unbanked==1))/nrow(last_10.df)

# perc with income > 120k
100*nrow(subset(last_10.df, hh_income > 120000))/nrow(last_10.df)
summary(last_10.df$hh_income)

# restrict income to not exceed 120k
last_11.df = subset(last_10.df, hh_income <= 120000)
summary(last_11.df$hh_income)
# need to rescale weights
nrow(last_11.df)
sum(last_11.df$weight)
#
last_12.df = last_11.df
last_12.df$weight = nrow(last_11.df)*last_11.df$weight/sum(last_11.df$weight)
sum(last_12.df$weight)

## plot per unbanked versus income
#dev.off()# otherwise, I got an error for some unknown
ggplot(last_12.df, mapping=aes(x=hh_income, y=unbanked)) +  stat_summary_bin(fun = "mean", geom="bar", color="black", fill="lightblue", bins=12) +
  ylab("mean") +
  scale_x_continuous(breaks = seq(0,120000,10000), labels=scales::dollar_format())+  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16), axis.text.y = element_text(size = 16),
text = element_text(size = 18)) +  scale_y_continuous(breaks = seq(0, 0.25, 0.05), labels = scales::percent_format(accuracy = 1)) +  ylab("Percentage unbanked") + xlab("Annual U.S. household income") + geom_smooth(aes(x=hh_income,y=unbanked), color="red")


# weighted plot below
#ggplot(last_12.df, mapping=aes(x=hh_income, y=unbanked*weight/nrow(last_11.df))) +  stat_summary_bin(fun = "mean", geom="bar", bins=12) +  ylab("mean")


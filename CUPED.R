library(data.table)
library(magrittr)
library(ggplot2)
library(boot)
library(agridat)

set.seed(1)
girls <- rnorm(n = 50,mean = 160,sd = 5) %>% data.table %>% 
  setnames(old = '.', new = 'height')
boys <- rnorm(n= 50, mean = 175, sd =5) %>% data.table %>%
  setnames(old = '.', new = 'height')

# Have to combine girls and boys into a single dataframe, assign a
# group to the dataframe.
girls[,group := 1]
boys[,group := 2]

pooled <- rbind(girls,boys)

ggplot(data=pooled, aes(x=height,color=factor(group))) + geom_density()

# Random sample variances from pooled
boot.mean <- function(df,indices){
  df <- df[indices]
  mean(df)
}
boot_means <- boot(data = pooled$height,statistic = boot.mean,R = 100)

boot_means_stratified <- boot(data=pooled$height,statistic = boot.mean, R = 100, strata = pooled$group)

min_x <- min(boot_means$t, boot_means_stratified$t)
max_x <- max(boot_means$t, boot_means_stratified$t)
seq(min_x,max_x,length.out=5)

ggplot() + geom_density(aes(x=boot_means$t, color='regular')) + 
  geom_density(aes(x=boot_means_stratified$t, color='stratified')) +
  scale_color_manual(name='sampling', values = c(regular ='red',
                                                 stratified ='blue')) +
  scale_x_continuous(breaks = seq(min_x,max_x,length.out=5)) +
  xlab('Means') + 
  geom_vline(xintercept=mean(boot_means$t),
             color='red',alpha=.5,
             linetype='dashed') +
  geom_vline(xintercept=mean(boot_means_stratified$t),
             color='blue',alpha=.5,
             linetype='dashed')

# Calculate the variance of Y_bar and Y_strat.
var(boot_means$t) # 0.932
var(boot_means_stratified$t) #0.1667
# var(Y_strat) < var(Y_bar), variance reduction from stratified sampling.

##############################################
df <- byers.apple %>% data.table
small_df <- df[(time %in% c(3,4)),]

# Consider the case of every apple (every customer, every user)
dice_roll <- sample(x = unique(small_df$appleid),
                    size = length(unique(small_df$appleid)),
                    replace = FALSE)

test_group <- dice_roll[1:(length(dice_roll)/2)]
control_group <- dice_roll[!dice_roll %in% test_group]

small_df[time %in% c(1,2,3), group := 'pre-experiment']
small_df[(time %in% c(4,5,6)) & 
           (appleid %in% test_group), group := 'test']
small_df[is.na(group), group := 'control']

small_df[,'random_noise' := rnorm(dim(small_df)[1],mean=0.002,sd=0.0009314159)]

small_df[group =='test', new_diameter := diameter*(1+random_noise)]
# small_df[group =='control', new_diameter := diameter*(1+(1/10)*random_noise)]
small_df[is.na(new_diameter), new_diameter := diameter]

# Before any elixir - is there already a difference in the means?
small_df[appleid %in% test_group,mean(new_diameter,na.rm=TRUE)] 
small_df[appleid %in% control_group,mean(new_diameter,na.rm=TRUE)] 

# Vanilla
y_test_vanilla <- small_df[(time == 4) & (appleid %in% test_group),mean(new_diameter,na.rm = TRUE)]
y_control_vanilla <- small_df[(time == 4) & (appleid %in% control_group),mean(new_diameter,na.rm=TRUE)]
var_y_test_vanilla <- small_df[(time == 4) & (appleid %in% test_group),var(new_diameter,na.rm = TRUE)]/length(test_group)
var_y_control_vanilla <- small_df[(time == 4) & (appleid %in% control_group),var(new_diameter,na.rm=TRUE)]/length(control_group)
test_stat <- (y_test_vanilla - y_control_vanilla)/sqrt(var_y_test_vanilla + var_y_control_vanilla)

# CUPED
pop_mean <- small_df[(time == 3),mean(new_diameter,na.rm=TRUE)]

# theta_test <- cov(small_df[(time == 3) & (appleid %in% test_group), new_diameter],
#                   small_df[(time == 4) & (appleid %in% test_group), new_diameter],
#                   use = 'pairwise.complete.obs') /
#   var(small_df[(time == 3) & (appleid %in% test_group), new_diameter],na.rm=TRUE)

sample_mean_test <- small_df[group=='test',mean(new_diameter,na.rm=TRUE)]
sample_mean_test_pre <- small_df[(group=='pre-experiment') &
                                   (appleid %in% test_group),mean(new_diameter,na.rm=TRUE)]

sample_mean_control <- small_df[group=='control', mean(new_diameter,na.rm=TRUE)]
sample_mean_control_pre <- small_df[(group=='pre-experiment') &
                                    (appleid %in% control_group),mean(new_diameter,na.rm=TRUE)]

theta <-cov(x = small_df[group=='pre-experiment',new_diameter],
            y = small_df[group!='pre-experiment',new_diameter],
            use = 'pairwise.complete.obs') / 
  var(small_df[group=='pre-experiment',new_diameter],na.rm=TRUE)

y_hat_cv_test <- sample_mean_test - theta*(sample_mean_test_pre - pop_mean)
y_hat_cv_control <- sample_mean_control -theta*(sample_mean_control_pre - pop_mean)

correl <- cor(x = small_df[group=='pre-experiment',new_diameter],
              y = small_df[group!='pre-experiment',new_diameter],
              use = 'pairwise.complete.obs')
delta_hat <- y_hat_cv_test - y_hat_cv_control
var_delta_hat <- (var_y_control_vanilla + var_y_test_vanilla) * (1-correl)
test_stat_cv <- delta_hat/sqrt(var_delta_hat)

writeLines(c(paste('Before experiment, mean of apples in test group is',small_df[(appleid %in% test_group) &
                                                                                   (time==3),round(mean(new_diameter,na.rm=TRUE),3)]),
             paste('Before experiment, mean of apples in control group is',small_df[(appleid %in% control_group) & 
                                                                                      (time==3),round(mean(new_diameter,na.rm=TRUE),3)]),
             paste('After experiment, mean of apples in test group is',small_df[(group=='test'),round(mean(new_diameter,na.rm=TRUE),3)]),
             paste('After experiment, mean of apples in control group is',small_df[(group=='control'),round(mean(new_diameter,na.rm = TRUE),3)]),
             paste('The vanilla test statistic is',round(test_stat,3)),
             paste('The CUPED test statistic is',round(test_stat_cv,3))
             ))

require(ggplot2)
require(dplyr)
require(ggpubr)

time_1 = c(0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72)
exp1 = c(1.01,0.38,0.83,1.18,1.90,0.66,0.63,0.75,0.85,1.00,0.87,0.29,0.52,0.55,0.76,0.83,0.81,0.56,0.51)
exp2 = c(1.00,0.35,0.99,1.03,0.68,0.41,0.46,0.68,1.15,1.17,0.59,0.43,0.56,0.87,1.07,0.99,0.91,0.75,0.57)

std_1 = c(0.13830844,0.07782919957,0.162277357,0.2917749578,0.2366300921,0.2128557509,0.331466507,0.3170622315,
         0.2976328675,0.1079264465,0.1430305638,0.08839649167,0.1141926188,0.2498288454,0.06907271895,0.1255305307,
         0.157750778,0.03581725196,0.03130929428)
std_2 = c(0.09187100648,0.0428655765,0.2819651521,0.1961841936,0.08960781894,0.07661668292,0.08448538592,
         0.119190479,0.4272137846,0.1958825141,0.1227554818,0.03339757723,0.1241180043,0.1069134232,0.1919008195,
         0.1509042573,0.09500118224,0.1824406968,0.04010477678)

df <- data.frame(time_1,exp1,exp2,std_1,std_2)

df1 <- data.frame(time_1,exp1)
df2 <- data.frame(time_1,exp2)

df_stats <- data.frame(time_1,exp1,exp2)

df_stats <- pivot_longer(df_stats,cols=c(exp1,exp2),names_to='exp',values_to='bmat')

spline_df1 <- as.data.frame(spline(df$time_1, df$exp1))
spline_df2 <- as.data.frame(spline(df$time_1,df$exp2))

plot_exp_points <- ggplot(df, aes(x = time_1)) +
  geom_point(aes(y=exp1,colour='exp1')) +
  geom_point(aes(y=exp2,colour='exp2')) +
  geom_errorbar(ymin=exp1-std_1,ymax=exp1+std_1,colour='red') + 
  geom_errorbar(ymin=exp2-std_2,ymax=exp2+std_2,colour='cadetblue3')

plot_exp_points

plot_all <- plot_exp_points + 
  geom_line(data = spline_df1, aes(x = x, y = y),colour='red') +
  geom_line(data = spline_df2, aes(x = x, y = y),color='cadetblue3') +
  xlim(-5,75) +
  ylim(0,2.25) +
  labs(x='Time (Hours)',y='Bma1 rhythmicity',colour='Experiment')

plot_all

new_df <- compare_means(bmat~exp,data = df_stats, method = "wilcox.test")

plot_stats <- ggline(df_stats, x = "time_1", y = "bmat", add = "mean_se",
       color = "exp", palette = "jco") + stat_compare_means(aes(group = exp), label = "p.signif", 
                     label.y = 2.0)

plot_stats_2 <- ggline(df1, x = "time_1", y = "exp1")+
  stat_compare_means() + stat_compare_means(ref.group = "0", label = "p.signif",
                     label.y = 2.0) 

plot_stats_2

plot_stats

###TEST DATA###
#This is some test data to see if the stats program works#

#Section 1 - this is comparison of data from one timepoint to the next

time_test = c(0,0,0,0,0,2,2,2,2,2,4,4,4,4,4,6,6,6,6,6)
bmat_test = c(1,2,3,2,1,14,15,14,13,15,1,1,2,1,2,1,1,1,1,1)

df_test <- data.frame(time_test,bmat_test)

compare_test <- compare_means(time_test~bmat_test,df_test,method='wilcox.test')
print(compare_test)

summary_test <- df_test %>% group_by(time_test) %>% summarize(mean_bmat=mean(bmat_test))
summary

'''plot_test <- ggdotplot(df_test, x='time_test', y='bmat_test',merge=TRUE,add='mean_se') +
  stat_compare_means() + stat_compare_means(ref.group = '0',label='p.signif',label.y = 30)
plot_test'''

plot_test <- ggplot(summary_test, aes(x = time_test)) + geom_point(aes(y=mean_bmat)) +
  geom_errorbar(ymin=c(1,12,50),ymax=c(3,15,53))

plot_test

spline_test <- as.data.frame(spline(df_test$time_test, df_test$bmat_test))

plot_test_2 <- plot_test + 
  geom_line(data = spline_test, aes(x = x, y = y),colour='red')

plot_test_3 <- plot_test_2 + stat_compare_means(aes(y=bmat_test),ref.group = "0", label = "p.signif",label.y = 2.0)

plot_test_3

plot_test_4 <- ggline(df_test, x='time_test',y='bmat_test',plot_type='p',add='mean_se') +
  stat_compare_means(aes(y=bmat_test),ref.group = "0", label = "p.signif",label.y = c(10,20,55)) +
  geom_line(data = spline_test, aes(x = x, y = y),colour='red')

plot_test_4

plot_type = 'p' for ggline...

#Section 2 - this is comparison of two data sets between points
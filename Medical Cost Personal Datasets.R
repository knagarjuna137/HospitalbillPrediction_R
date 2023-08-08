library(readr)
insurance <- read_csv("C:/Users/kajan/Downloads/insurance.csv")
View(insurance)

dim(insurance) #melihat dimensi data (rows x columns)
names(insurance) #melihat nama kolom pada data


head(insurance)


sum(is.na(insurance)) # total keseluruhan NA bila ada
colSums(is.na(insurance)) # total NA per colum


install.packages ("Hmisc")
library (Hmisc)

summary(pressure)
install.packages ("pastecs")
library (pastecs)
stat.desc (insurance)


#EDA visualization
install.packages('ggplot2')
install.packages('dplyr')
library(dplyr)
install.packages("gridExtra")
library(gridExtra)


# berdasarkan jenis kelamin
plot1 = ggplot(insurance, aes(x=age, y=charges, col=sex))+geom_point()+ggtitle("scatter plot age vs charges")
plot2 = ggplot(insurance, aes(x=bmi, y=charges, col=sex))+geom_point()
plot3 = ggplot(insurance, aes(x=children, y=charges, col=sex))+geom_point()

# berdasarkan region
plot4 = ggplot(insurance, aes(x=age, y=charges, col=region))+geom_point()
plot5 = ggplot(insurance, aes(x=bmi, y=charges, col=region))+geom_point()
plot6 = ggplot(insurance, aes(x=children, y=charges, col=region))+geom_point()

# berdasarkan smoker
plot7 = ggplot(insurance, aes(x=age, y=charges, col=smoker))+geom_point()
plot8 = ggplot(insurance, aes(x=bmi, y=charges, col=smoker))+geom_point()
plot9 = ggplot(insurance, aes(x=children, y=charges, col=smoker))+geom_point()

grid.arrange(plot1, plot2, plot3, 
             plot4, plot5, plot6,
             plot7, plot8, plot9,
             nrow = 3, ncol = 3)

# bar plot
#berdasarkan sex
plot10 = ggplot(insurance, aes(x = children)) + geom_bar(aes(fill = sex))
plot11 =ggplot(insurance, aes(x = smoker)) + geom_bar(aes(fill = sex))

#berdasarkan region
plot12 =ggplot(insurance, aes(x = sex)) + geom_bar(aes(fill = region))
plot13 =ggplot(insurance, aes(x = children)) + geom_bar(aes(fill = region))
plot14 =ggplot(insurance, aes(x = smoker)) + geom_bar(aes(fill = region))

grid.arrange(plot10, plot11, plot12, 
             plot13, plot14,
             nrow = 2, ncol = 3)


plot_a=ggplot(insurance, aes(x=age)) + geom_boxplot(fill='blue')+coord_flip()
plot_b=ggplot(insurance, aes(x=bmi)) + geom_boxplot(fill='blue')+coord_flip()
plot_c=ggplot(insurance, aes(x=children)) + geom_boxplot(fill='blue')+coord_flip()
plot_d=ggplot(insurance, aes(x=charges)) + geom_boxplot(fill='blue')+coord_flip()

grid.arrange(plot_a, plot_b, 
             plot_c, plot_d,
             nrow = 2, ncol = 2)



# mendeteksi outlier
# get threshold values for outliers
upper_limit_bmi = quantile(insurance$bmi,0.75)+1.5*IQR(insurance$bmi)
upper_limit_bmi
lower_limit_bmi = quantile(insurance$bmi,0.25)-1.5*IQR(insurance$bmi)
lower_limit_bmi

# find outlier
outlier=insurance[!(insurance$bmi > lower_limit_bmi & insurance$bmi < upper_limit_bmi ),]
dim(outlier)


#outlier
insurance=insurance[(insurance$bmi > lower_limit_bmi & insurance$bmi < upper_limit_bmi ),]
dim(insurance)




#get threshold values for outliers
upper_limit_charges = quantile(insurance$charges,0.75)+1.5*IQR(insurance$charges)
upper_limit_charges
lower_limit_charges = quantile(insurance$charges,0.25)-1.5*IQR(insurance$charges)
lower_limit_charges

# find outlier
outlier=insurance[!(insurance$charges > lower_limit_charges & insurance$charges < upper_limit_charges ),]
dim(outlier)



#Membuat data baru tanpa outlier¶
insurance=insurance[(insurance$charges > lower_limit_charges & insurance$charges < upper_limit_charges),]
dim(insurance)

install.packages("PerformanceAnalytics")
install.packages("xts")
library(PerformanceAnalytics)
chart.Correlation(insurance[,c(1,3,4,7)], histogram = TRUE, method = "pearson")

# linear regression
#analisis regresi berganda
insurance=lm(charges~age+sex+bmi+children+smoker+region,insurance = insurance)

summary(insurance)

hasil = summary(insurance)
hasil$r.squared





#mins Look at the data given below. Plot the data, find the outliers
#and find out mean, sigma and variance.


#Name of company    Measure X
#Allied Signal      24.23%
#Bankers Trust      25.53%
#General Mills      25.41%
#ITT Industries     24.14%
#J.P.Morgan & Co.   29.62%
#Lehman Brothers    28.25%
#Marriott           25.81%
#MCI                24.39%
#Merrill Lynch      40.26%
#Microsoft          32.95%
#Morgan Stanley     91.36%
#Sun Microsystems   25.99%
#Travelers          39.42%
#US Airways         26.71%
#Warner-Lambert     35.00%


Company_Names<-read.csv("E://Back up 1/ACADGILD/Company_Names.csv",header = TRUE,sep=",")
View(Company_Names)

# To verify the data-structures of the given dataset
str(Company_Names)

# Converting the table to a dataframe
df<-data.frame(Company_Names)
View(df)

# To convert the percentages into floating points
#it needs to be divided by 100.
b<-df[,2] <- as.numeric(gsub("%", "",df[,2]))/100
b

View(df)


install.packages("outliers")
install.packages("stringr")
library(outliers)
library(stringr)


# Checking Outliers  within the given dataset by 
#using Box-plot and Histogram Charts as follow:

source("https://goo.gl/4mthoF")
outlierKD (df, Measure.X)


# Identification of the position of outliers within the given dataset----

findOutliers <- function(df) {
  lowerq = quantile(df)[2]
  upperq = quantile(df)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(df > extreme.threshold.upper | df < extreme.threshold.lower)
}


outliers <- findOutlier(df)
outliers

####  Detection of outliers and their values
#Univariate approach
#For a given continuous variable, outliers are those observations 
#that lie outside 1.5 * IQR, where IQR,the 'Inter Quartile Range' 
#is the difference between 75th and 25th quartiles.

outlier_values <- boxplot.stats(df$Measure.X)$out  # outlier values.
outlier_values


# Plotting of Graphs and Charts

boxplot(df$Measure.X, main="Measure_Values",boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=",")), cex=0.6)

plot(df)
boxplot(df)
scatter.smooth(df)


plot(df, type="o", col="black",
     main="Company Valuation", col.main="purple", font.main=4,font.lab=4, font.sub=4)

summary(df)


# The best tool to identify the outliers is the box plot. Through 
#box plots we find the minimum, lower quartile (25th percentile), 
#median (50th percentile), upper quartile (75th percentile), 
#and maximum of an continues variable. 
#The function to build a boxplot is boxplot().

boxplot(df$Measure.X, main="Box plot", ylab="Measure.X")
  

# build the box plot
boxplot(Measure.X ~ Name.of.company , data=df,
     main="Box Plot",
     xlab="Name.of.company",
     ylab="Measure.X")
boxplot(df)

# To find the Mean(or mu-value) of the given dataset
mean(df$Measure.X)

# To find the Standard Deviation of the given dataset
sd(df$Measure.X)

# To find the Variance of the given dataset
var(df$Measure.X)

# Question 2 
#Answer the following three questions based on the box-plot above.
#(i) What is inter-quartile range of this dataset? (please approximate the numbers)
#In one line, explain what this value implies.
#(ii) What can we say about the skewness of this dataset?
#(iii) If it was found that the data point with the value 25 is actually 2.5, how would
#the new box-plot be affected?

#Q 2(i) What is inter-quartile range of this dataset? (please approximate the numbers)
#In one line, explain what this value implies.
#Ans 2(i) 
upper_quartile<-12.5
lower_quartile<-5
IQR<-(upper_quartile-lower_quartile)
IQR

# IQR value is 7.5.
# This value implies the midspread or middle 50%.
# Technically IQR is H-spread and is a measure of statistical dispersion.

# Q2(ii) What can we say about the skewness of this dataset?
# Ans 2(ii)
#  The distribution  of values in the given dataset is skewed 
#towards the left or negatively skewed.

#(iii) If it was found that the data point with the value 25 is actually 2.5, how would
#the new box-plot be affected?
# Ans 2(iii)

# Descriptive Statistical Measures viz. Mean and Median values would significantly differ. Moreover, it will
# tend to decrease the estimate of sample variance.
# The F statistic is based on the sample means and the sample variances, each of which is sensitive to outliers.
# As a result, calculated F statistic for ANOVA (Analysis of Variance) could increase.


# Q3 Answer the following three questions based on the histogram above.
#(i) Where would the mode of this dataset lie?
#(ii) Comment on the skewness of the dataset.
#(iii) Suppose that the above histogram and the box-plot in question 2 are plotted for
# the same dataset. Explain how these graphs complement each other in providing
# information about any dataset.

#Q 3(i) Where would the mode of this dataset lie?
# Ans:  User-defined function to calculate Mode in the given dataset is as follows:-
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

x<-c(0,1,2,3,3,4,4,5,5,6,6,7,7,7.5,7.5,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24,25,26)
x

# Using these values  to calculate Mode then:-
mode<-estimate_mode(x)
mode

round(mode, digits=2)

# Q 3(ii) Comment on the skewness of the dataset.
# Ans The given dataset is positively skewed or right-skewed, as majority of the values contained
# within the datset are on the right side of the mean.

# The skewness is a measure of symmetry. 
# As a rule, negative skewness indicates that the mean of the data values is less than the median, 
# and the data distribution is left-skewed. 
#Positive skewness would indicate that the mean of the data values is larger than the median, and 
#the data distribution is right-skewed.


install.packages("e1071")
library(e1071)


skewness(x)

# Q3 (iii) Suppose that the above histogram and the box-plot in question 2 are plotted for
# the same dataset. Explain how these graphs complement each other in providing
# information about any dataset.

boxplot(x)
hist(x)
scatter.smooth(x)

# Box plots provides Summary of a Distribution.For eg. the median, 25th and 75th percentile, 
# min/max, range  and explicitly separates the points that are considered as outliers within 
# the given dataset.
# Rather there is more focus wherein the distributions lie with regard to one another. 
# Box-plots can be used only for Numerical daata

#A histogram is a type of graph that shows the Frequency Distribution of data within 
#equal intervals (thus, there are no spaces between the bars).
# Huge Datasets  can be easily plotted with histograms.  
# Histograms are only used for numerical data.

# Hence Histograms and Box-Plots complement each other.


#4. AT&T was running commercials in 1990 aimed at luring back customers who had
# switched to one of the other long-distance phone service providers. One such
# commercial shows a businessman trying to reach Phoenix and mistakenly getting Fiji,
# where a half-naked native on a beach responds incomprehensibly in Polynesian. When
# asked about this advertisement, AT&T admitted that the portrayed incident did not
# actually take place but added that this was an enactment of something that "could
# happen." Suppose that one in 200 long-distance telephone calls is misdirected. What is
# the probability that at least one in five attempted telephone calls reaches the wrong
# number? (Assume independence of attempts.)



# Binomial Distribution

#The binomial distribution is a discrete probability distribution. 
#It describes the outcome of n independent trials in an experiment. 
#Each trial is assumed to have only two outcomes, either success or 
#failure. 

#The Binomial distribution f(n,p) is represented R by 
#dbinom(x, size, prob)
#pbinom(x, size, prob)
#qbinom(p, size, prob)
#rbinom(n, size, prob)
#Following is the description of the parameters used ???

#x is a vector of numbers.

#p is a vector of probabilities.

#n is number of observations.

#size is the number of trials.

#prob is the probability of success of each trial.


#In the formula, n is the number of trials of some random process that 
#can take on one of two discrete values, 
#say 1 for success and 0 for failure, and p is the probability of success for each trial. 
#The probability density dbinom and cumulative distribution pbinom are defined 
#on the non-negative integers up to and including n.
# For the example, we'll look at n=200 and p=0.5, like 200 misdirected calls. 
#To figure out a good range for plotting, we will use the qbinom function 
#to find out for a given n and p, what is the least integer that bounds 
#the cumulative Binomial distribution above 99.9%, and what is the 
#greatest integer that bounds below at 0.1%.

lower<-qbinom(0.001, size=200, prob=0.5)
upper<-qbinom(0.999, size=200, prob=0.5)
n<-seq(lower,upper,1)
q<-seq(0.001,0.999,0.001)
dBinom200 <- data.frame(N=n, 
                        Density=dbinom(n, size=200, prob=0.5),
                        Distribution=pbinom(n, size=5, prob=0.5))  
qBinom200 <- data.frame(Q=q, Quantile=qbinom(q, size=200, prob=0.5))  
head(dBinom200)




#5. Returns on a certain business venture, to the nearest $1,000, are known to follow the
#following probability distribution
#x                  P(x)
#-2,000             0.1
#-1,000             0.1
#0                  0.2
#1000               0.2
#2000               0.3
#3000               0.1

#(i) What is the most likely monetary outcome of the business venture?
#(ii) Is the venture likely to be successful? Explain
#(iii) What is the long-term average earning of business ventures of this kind? Explain
#(iv) What is the good measure of the risk involved in a venture of this kind? Compute
#this measure


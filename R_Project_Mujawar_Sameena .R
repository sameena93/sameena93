#1.Call the data
library(class)
library(ggplot2)
library(readr)
bank <- read_csv("C:/Users/moham/DataScience/R/Projects/Final project_part1/bank.csv")
View(bank)


#2.Explore the data
str(bank)
summary(bank)
head(bank,10)
# Check for missing values
colSums(is.na(bank))



#3 Analyzing the each variable of bank marketing data set.


install.packages("gridExtra")
library(gridExtra)

age_boxplot <-ggplot(bank,aes(x=age))+
  geom_boxplot(fill="lavenderblush",col="hotpink4",outlier.shape = 24,outlier.fill="hotpink4")+
  ggtitle("Boxplot Of Age Distribution")+
  labs(x="Age",y="Frequency")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))

balance_histplot<- ggplot(bank,aes(x=balance))+
  geom_histogram(fill="skyblue",color="black")+geom_abline(col="red")+
  ggtitle("Histogram Of Balance With Mean")+
  labs(x="balance",y="Frequency")+
  scale_y_continuous(position = "right")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))

campaign_histplot <- ggplot(bank,aes(x=campaign))+
  geom_histogram(aes(y=(..density..)),binwidth=2,color="black",fill="white")+
  geom_density(alpha=0.2,fill="blue",col="red")+
  ggtitle("Histogram of  Number of contacts during campaign")+
  labs(x="Number of contact",y="Frequency")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))

class(bank$marital)
bank$marital <- as.factor(bank$marital)
marital_barplot <- ggplot(bank,aes(x=marital))+
  geom_bar(fill=c("darkseagreen","aquamarine3","goldenrod"),color="black")+
  ggtitle("Barplot Of Marital Status")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))

class(bank$contact)
bank$contact <- as.factor(bank$contact)
contact_barplot <-ggplot(bank,aes(x=contact))+
  geom_bar(fill=c("paleturquoise","mediumpurple3","plum1"),color="black")+
  ggtitle("Barplot of contact cummunication type")+
  labs(x="Contact type",y="Frequency")+
  scale_y_continuous(position = "right")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))

class(bank$deposit)
bank$deposit <- as.factor(bank$deposit)
deposit_barplot <-ggplot(bank,aes(x=deposit))+
  geom_bar(fill=c("steelblue4","turquoise3"))+
  ggtitle("Barplot of the Deposit Subscription Status")+
  labs(x="Deposit Indicator",y="Frequency")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=12,hjust=0.5),
        axis.title = element_text(face="bold",color='darkslategray',size=10),
        axis.text = element_text(face="bold",color="black"))



grid.arrange(age_boxplot,balance_histplot,campaign_histplot,
             ncol=2,top="BANK NUMERICAL DATA ANALYSIS")
            
grid.arrange(marital_barplot,contact_barplot,deposit_barplot,
             ncol=2,top="BANK CATEGORICAL DATA ANALYSIS")

?grid.arrange()



#4. Summerize the data
#a.check percentile of age and balance variable 
#b.make function which prints values of quantile ,mean,sd of balance and age
#c.correlation of balance and age using cor()

#a.balance
balance_percentile<- quantile(bank$balance , probs=c(0,0.25,0.5,0.75,1))
balance_mean<- mean(bank$balance)
balance_sd<- sd(bank$balance)

#age
age_percentile <- quantile(bank$age ,probs=c(0,0.25,0.5,0.75,1))
age_mean <-mean(bank$age)
sd_mean <- sd(bank$age)



#b.Function to print quantile values in console
percentile <- function(variable,per,mean,sd){
  cat("0th  percentile of",variable,":",per[1], "\n")
  cat("25th percentile of",variable,":",per[2], "\n")
  cat("50th percentile of",variable,":",per[3], "\n")
  cat("75th percentile of",variable,":",per[4], "\n")
  cat("100th percentile of",variable,":",per[5], "\n")
  cat("Mean of",variable,":", mean,"\n")
  cat("Standard deviation of",variable,":",sd,"\n")
}

?cat()

percentile("balance",balance_percentile,balance_mean,balance_sd)
percentile("age",per=age_percentile,age_mean,sd_mean)

#c.Correlation between the balance and age
cor(bank$balance,bank$age)      #[1] 0.08382014




#4. frequency table and mode of deposit,job
table(bank$deposit)
names(sort(-table(bank$deposit)))[1]


table(bank$job)
names(sort(-table(bank$job)))[1]





#5. Bar plot for campaign and job using basic R graph and save the plot

class(bank$job)
bank$job<-as.factor(bank$job)

jpeg("C:\\Users\\moham\\DataScience\\R\\Projects\\Final project\\campaign&job.jpeg")

color=c("yellow","plum1","red4","blue","orange4","skyblue","purple4","white","green"
                 ,"black","lightpink","goldenrod")
campaign.job.table<-xtabs(~job + campaign,data=bank)

barplot(campaign.job.table,main="Campaign by Jobs",xlab="Campaign",ylab="Count",
        col=color,
        legend=rownames(campaign.job.table))

dev.off()




#6.Create new column indicating whether client has subscribed to term deposit 

bank$deposit_indicator <- ifelse(bank$deposit == "yes", "Subscribed", "Not Subscribed")

# Histogram of age with deposit_indictor 

class(bank$deposit_indicator)
bank$deposit_indicator <-as.factor(bank$deposit_indicator)

ggplot(bank,aes(x=age,fill=deposit_indicator))+
  geom_histogram(binwidth = 5,position = "dodge2",color="black",alpha=0.8)+
  ggtitle("Age distribution of clients by deposit status")+
  labs(x="Age",y="Frequency")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=15,hjust=0.5),
        axis.title = element_text(face="bold",color='deeppink4',size=13),
        axis.text = element_text(face="bold",color="black"),
        legend.title = element_text(face="bold"),
        legend.text =element_text(face="bold"))




#7. Visualize the continues variable against the factor
   #dotplot of duration against deposit_indicator

ggplot(data=bank,aes(x=duration,y=deposit_indicator))+
  geom_dotplot(aes(fill=deposit_indicator,stackdir="center",binwidth=2,binaxis="y"))+
  stat_summary(fun.y = mean , fun.ymin = mean , fun.ymax = mean ,geom = 'crossbar',width=0.5)+
  ggtitle("Duration against Deposit Indicator")+
  labs(x="Duration",y="Deposit Indicator")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=15,hjust=0.5),
        axis.title = element_text(face="bold",color='deeppink4',size=13),
        axis.text = element_text(face="bold",color="black"),
        legend.title = element_text(face="bold"),
        legend.text =element_text(face="bold"))


  



#8.Boxplot with density plot of campaign and filled with deposit indicator

#Installing package "ggrides" to work geom_density_ridges()
install.packages("ggridges")
library(ggridges)
library(ggplot2)

ggplot(bank, aes(x=campaign, y=deposit_indicator, fill=deposit_indicator)) +
  geom_density_ridges(alpha=0.7 ,scale=0.8) +
  geom_boxplot(aes(y=deposit_indicator), width=0.2, alpha=0.5) +
  ggtitle("Deposit Indicator by Campaign") +
  labs(x="Campaign", y="Deposit Indicator",fill="Deposit Indicator") +
  theme(plot.title = element_text(face="bold",color="deeppink4",size=15,hjust=0.5),
        axis.title = element_text(face="bold",color='deeppink4',size=13),
        axis.text = element_text(face="bold",color="black"),
        legend.title = element_text(face="bold"),
        legend.text =element_text(face="bold"))




#9.Heatmap of all numeric variables of bank marketing dataset and save the file 

#install package ggcorplot
install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)

# Select only numeric variables
numeric_vars <-sapply(bank , is.numeric)
bank_numeric <- bank[, numeric_vars]
head(bank_numeric,4)

# Calculate correlation matrix
cor_matrix <- cor(bank_numeric)

# Plot heatmap of correlation matrix
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 5, 
             title = " Heatmap of Bank Marketing Numeric Variables")+
  theme(plot.title = element_text(face="bold",color="deeppink4",size=15,hjust=0.5),
        axis.title = element_text(face="bold",color='deeppink4',size=13),
        axis.text = element_text(face="bold",color="black"),
        legend.title = element_text(face="bold"),
        legend.text =element_text(face="bold"))

ggsave("C:\\Users\\moham\\DataScience\\R\\Projects\\Final project\\Heatmap of bank.jpeg")  
  



#10. Create two statistical models using multiple linear regression

library(class)

#normalize the predictors
normalizefun <- function (x){
  result <- (x-min(x))/(max(x)-min(x))
  return(result)
}

bank$balance<-normalizefun(bank$balance)
summary(bank$balance)
bank$age <- normalizefun(bank$age)
summary(bank$age)
bank$campaign <- normalizefun(bank$campaign)
summary(bank$campaign)
bank$duration <- normalizefun(bank$duration)
summary(bank$duration)

bank$job<- as.factor(bank$job)
bank$deposit <- as.factor(bank$deposit)


# Split the bank dataset into train and test data
sample.size <- floor(0.8*nrow(bank))
sample.size

#model1
predictors1 <- c("balance","campaign","duration")
data.predictor <- bank[predictors1] 

data.target1 <- bank$deposit

train1 <- data.predictor[1:sample.size, ]
test1 <- data.predictor[-c(1:sample.size), ]
cl1 <- data.target1[1:sample.size]
num.neighbors1 <- floor(sqrt(nrow(bank)))

knn.test.predict1 <- knn(train1,test1,cl1,k=num.neighbors1)

test.label1 <- data.target1[-c(1:sample.size)]
cm1<-table(test.label1, knn.test.predict1)
cm1
accuracy1 <- (780+17)/(780+94+17+14)*100
accuracy1


#model2
predictors2 <-c("balance","campaign","duration","age")
data.predictor2 <-bank[predictors2]

data.target2 <-bank$deposit

train2 <- data.predictor2[1:sample.size, ]
test2 <- data.predictor2[-c(1:sample.size), ]
cl2 <- data.target2[1:sample.size]
num.neighbors2 <- floor(sqrt(nrow(bank)))
knn.test.predict2 <- knn(train2,test2,cl2,k=num.neighbors2)
test.label2 <- data.target2[-c(1:sample.size)]
cm2<-table(test.label2, knn.test.predict2)
cm2
accuracy2<-(783+11)/(783+100+11+11)*100
accuracy2




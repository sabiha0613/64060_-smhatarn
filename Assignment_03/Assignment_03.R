install.packages("reshape")
install.packages("reshape2")
install.packages("caret")
install.packages("gmodels")

library(caret)
library(reshape2)
library(e1071)
library(gmodels)

#setting the working directory
setwd("~/Desktop/Machine Learning/Assignment /Assignment_03")
bank.df<-read.csv("UniversalBank.csv")

##change numerical variables to categorical first
bank.df$Personal.Loan<-factor(bank.df$Personal.Loan)
bank.df$Online<-factor(bank.df$Online)
bank.df$CreditCard<-factor(bank.df$CreditCard)

##create training and validation sets
set.seed(1)
train.index<-sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
valid.index<-setdiff(row.names(bank.df),train.index)
train.df<-bank.df[train.index,]
valid.df<-bank.df[valid.index,]

##A. Create a pivot table for the training data with Online as a column variable, CC as a row variable,
##and Loan as a secondary row variable. The values inside the table should convey the count. In R
##use functions melt() and cast(), or function table(). In Python, use panda dataframe methods melt()
##and pivot().
meltdata.bank<-melt(train.df,id=c("CreditCard","Personal.Loan"),variable="Online",value.var="Results")
meltdata.bank
##cast the melted data
## cast(data, formula, function)
cast.bank<-dcast(meltdata.bank,CreditCard+Personal.Loan~Online,fun.aggregate = NULL)
cast_bank<-cast.bank[,c(1:2,14)]
cast_bank

##B. Consider the task of classifying a customer who owns a bank credit card and is actively using online banking services.
#Looking at the pivot table, what is the probability that this customer will accept the loan offer? 
#[This is the probability of loan acceptance (Loan = 1) conditional on having a bank credit card (CC = 1) 
#and being an active user of online banking services (Online = 1)].

#Creating a 3 way cross table 
table(train.df[,c(10,13:14)])
##There are 516 records where online = 1 and cc = 1. 47 of them accept the loan. 
#Therefore, the conditional probability is  47/(469+47)516=0.0910=9.10%

##C. Create two separate pivot tables for the training data. One will have Loan (rows) as a function of
##Online (columns) and the other will have Loan (rows) as a function of CC.

meltdata.bank1<-melt(train.df,id="CreditCard",variable="Online")
cast.bank1<-dcast(meltdata.bank1,CreditCard~Online,fun.aggregate = NULL)
cast_bank1<-cast.bank1[,c(1,14)]
cast_bank1

meltdata.bank2<-melt(train.df,id="Personal.Loan",variable="Online")
cast.bank2<-dcast(meltdata.bank2,Personal.Loan~Online,fun.aggregate = NULL)
cast_bank2<-cast.bank2[,c(1,13)]
cast_bank2


##D. Compute the following quantities [P(A | B) means “the probability ofA given B”]:
##i. P(CC = 1 | Loan = 1)
s<-table(train.df[,c(14,10)])
s[2,2]/(s[2,2]+s[1,2])
##ii. P(Online = 1 | Loan = 1)
q<-table(train.df[,c(13,10)])
q[2,2]/(q[2,2]+q[1,2])
##iii.P(Loan = 1) (the proportion of loan acceptors)
r<-table(train.df[,10])
r[2]/(r[2]+r[1])
##iv. P(CC = 1 | Loan = 0)
t<-table(train.df[,c(14,10)])
t[2,1]/(t[2,1]+t[1,1])
##v.  P(Online = 1 | Loan = 0)
x<-table(train.df[,c(13,10)])
x[2,1]/(x[2,1]+x[1,1])
##vi. P(Loan = 0)
y<-table(train.df[,10])
y[1]/(y[1]+y[2])

##E. Use the quantities computed above to compute the naive Bayes probability P(Loan = 1 | CC = 1,Online = 1).
77/77+198=0.28 
166/166+109=0.603  
275/275+2725=0.0916
801/801+1924=0.293
1588/1588+1137=0.582
2725/2725+275=0.908
#Naive Bayes Probability
((77/(77+198))*(166/(166+109))*(275/(275+2725)))/
            (((77/(77+198))*(166/(166+109))*(275/(275+2725)))+((801/(801+1924))*(1588/(1588+1137))*2725/(2725+275)))=0.09055758
##F. Compare this value with the one obtained from the pivot table in (B). Which is a more accurate estimate?

#There is not much of difference between 0.0910 or 9.10% and 0.09055758 or 9.05% . 
#Pivot table value is the more accurate estimate value obtained since it does not make the assumption of the probabilities being independent.

##G. Which of the entries in this table are needed for computing P(Loan = 1 | CC = 1, Online = 1)? Run
##naive Bayes on the data. Examine the model output on training data, and find the entry that
##corresponds to P(Loan = 1 | CC = 1, Online = 1). Compare this to the number you obtained in (E).

##TRAINING SET
table(train.df[,c(10,13:14)])
train.naive<-train.df[,c(10,13:14)]
train.naive
Bank.nb<-naiveBayes(Personal.Loan~.,data=train.naive)
Bank.nb

##Confusion matrix and Statistics
##TRAINING
pred.class<-predict(Bank.nb,newdata=train.df)
confusionMatrix(pred.class,train.df$Personal.Loan)
CrossTable(x=train.df$Personal.Loan,y=pred.class, prop.chisq = FALSE) 

##It is sometimes useful to output the raw prediction probabilities rather than the predicted class. To do that, we use the raw option in the model.
Bank.nb<-naiveBayes(Personal.Loan~.,data=train.naive)
Bank.nb
#Make predictions and return probability of each class
pred.class <-predict(Bank.nb,valid.df, type = "raw")
#show the first few values 
head(pred.class)
#The probability obtained from naive Bayes model is 0.09166667 whereas that obtained from  (E) is 0.09055758



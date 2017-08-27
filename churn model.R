# Import data set
mydata<-read.csv("http://www.dataminingconsultant.com/data/churn.txt", header=TRUE, sep=",")
View(mydata)

#Transform Categorical variables to numerical(binary)
mydata$Churn.=as.integer(mydata$Churn.)
mydata$Int.l.Plan  = as.integer(mydata$Int.l.Plan)
mydata$VMail.Plan = as.integer(mydata$VMail.Plan)


mydata$Churn.[mydata$Churn. == "1"] <- 0
mydata$Churn.[mydata$Churn. == "2"] <- 1

mydata$Int.l.Plan[mydata$Int.l.Plan == "1"] <- 0
mydata$Int.l.Plan[mydata$Int.l.Plan == "2"] <- 1

mydata$VMail.Plan[mydata$VMail.Plan == "1"] <- 0
mydata$VMail.Plan[mydata$VMail.Plan == "2"] <- 1

mydata$State <- "NULL"
mydata$Phone <- "NULL"

na.omit(mydata)

mydata=as.data.frame(mydata)
drops<-c("State","Phone")
mydata1=mydata[,!(names(mydata) %in% drops)]

View(mydata1)
summary(mydata1)
names(mydata1)


project=mydata1

###### Fine Classing ###################
library(Causata)
data(project)
bi<-BinaryCut(project$VMail.Plan,project$Churn.,nbins = 20)
Wi<-Woe(bi,project$Churn.)
Wi
Woe<-data.frame(Wi$woe.levels,Wi$odds,Wi$bin.count,Wi$true.count,Wi$log.density.ratio,Wi$information.value,Wi$prob.i.true,Wi$prob.i.false)
View(Woe)

#Note:rest of variables have to calculate using above way



############### Course Classing #############################
## DayMins:
X4<-c(1:3333)
for(i in 1:length(project$Day.Mins))
{if (0<=project$Day.Mins[i]&project$Day.Mins[i]<=130) X4[i]=-0.182525814
 else if (130<project$Day.Mins[i] & project$Day.Mins[i]<=140) X4[i]=0.062762322
 else if (140<project$Day.Mins[i] & project$Day.Mins[i]<=180) X4[i]=-0.397561148
 else if(180<project$Day.Mins[i] &project$Day.Mins[i]<=218) X4[i]=-1.005155033
 else if(218<project$Day.Mins[i] &project$Day.Mins[i]<=226) X4[i]=-0.037321137
 else if(226<project$Day.Mins[i] &project$Day.Mins[i]<=245) X4[i]=0.257186904
 else if(245<project$Day.Mins[i] &project$Day.Mins[i]<=266) X4[i]=0.842237575
 else if(266<project$Day.Mins[i] &project$Day.Mins[i]<=271) X4[i]=2.245061249
 else if(271<project$Day.Mins[i] &project$Day.Mins[i]<=291) X4[i]=1.796111033
 else if(291<project$Day.Mins[i] &project$Day.Mins[i]<=351) X4[i]=2.833664574}

dd=as.data.frame(X4)

##Day.Charge
X5<-c(1:3333)
for(j in 1:length(project$Day.Charge))
{if (0<=project$Day.Charge[j]&project$Day.Charge[j]<=22.1) X5[j]=-0.182525814
 else if (22.1<project$Day.Charge[j]&project$Day.Charge[j]<=23.7) X5[j]=0.062762322
 else if (23.7<project$Day.Charge[j]&project$Day.Charge[j]<=30.7) X5[j]=-0.397561148
 else if(30.7<project$Day.Charge[j]&project$Day.Charge[j]<=37) X5[j]=-1.005155033
 else if(37<project$Day.Charge[j]&project$Day.Charge[j]<=38.5) X5[j]=-0.037321137
 else if(38.5<project$Day.Charge[j]&project$Day.Charge[j]<=41.7) X5[j]=0.257186904
 else if(41.7<project$Day.Charge[j]&project$Day.Charge[j]<=45.2) X5[j]=0.842237575
 else if(45.2<project$Day.Charge[j]&project$Day.Charge[j]<=46.1) X5[j]=2.245061249
 else if(46.1<project$Day.Charge[j]&project$Day.Charge[j]<=49.5) X5[j]=1.796111033
 else if(49.5<project$Day.Charge[j]&project$Day.Charge[j]<=59.6) X5[j]=2.833664574}

df1=as.data.frame(X5)

##CustServCall
X6<-c(1:3333)
for(j in 1:length(project$CustServ.Calls))
{if (-1<=project$CustServ.Calls[j]&project$CustServ.Calls[j]<=3) X6[j]=-0.2901527024
 else if (3<project$CustServ.Calls[j]&project$CustServ.Calls[j]<=4) X6[j]=1.6059812982
 else if (4<project$CustServ.Calls[j]&project$CustServ.Calls[j]<=9) X6[j]=2.238630359 }

df2=as.data.frame(X6)

##IntlCharge
X7<-c(1:3333)
for(j in 1:length(project$Intl.Charge))
{if (0<=project$Intl.Charge[j]&project$Intl.Charge[j]<=2.3) X7[j]=-0.2517739712
 else if (2.3<project$Intl.Charge[j]&project$Intl.Charge[j]<=2.38) X7[j]= 0.33350058
 else if (2.38<project$Intl.Charge[j]&project$Intl.Charge[j]<=2.48) X7[j]= -0.029947076
 else if(2.48<project$Intl.Charge[j]&project$Intl.Charge[j]<=2.59) X7[j]=0.158746828
 else if(2.59<project$Intl.Charge[j]&project$Intl.Charge[j]<=2.86) X7[j]=-0.1152208293
 else if(2.86<project$Intl.Charge[j]&project$Intl.Charge[j]<=2.97) X7[j]=0.030365661
 else if(2.97<project$Intl.Charge[j]&project$Intl.Charge[j]<=3.06) X7[j]=-0.207757086
 else if(3.06<project$Intl.Charge[j]&project$Intl.Charge[j]<=3.16) X7[j]= 0.057406123
 else if(3.16<project$Intl.Charge[j]&project$Intl.Charge[j]<=3.59) X7[j]=-0.1950472603
 else if(3.59<project$Intl.Charge[j]&project$Intl.Charge[j]<=3.81) X7[j]=1.0340544341
 else if(3.81<project$Intl.Charge[j]&project$Intl.Charge[j]<=5.4) X7[j]=0.2913890955}

df3=as.data.frame(X7)

##Intl.Calls
X8<-c(1:3333)
for(j in 1:length(project$Intl.Calls))
{if (0<=project$Intl.Calls[j]&project$Intl.Calls[j]<=2) X8[j]=0.440435269
 else if (2<project$Intl.Calls[j]&project$Intl.Calls[j]<=3) X8[j]=0.014388737
 else if (3<project$Intl.Calls[j]&project$Intl.Calls[j]<=9) X8[j]=-0.204271481
 else if(9<project$Intl.Calls[j]&project$Intl.Calls[j]<=20) X8[j]=0.028148717}

df4=as.data.frame(X8)

##Intl.Mins
X9<-c(1:3333)
for(j in 1:length(project$Intl.Mins))
{if (0<=project$Intl.Mins[j]&project$Intl.Mins[j]<=8.5) X9[j]=-0.251773971
 else if (8.5<project$Intl.Mins[j]&project$Intl.Mins[j]<=8.8) X9[j]=0.33350058
 else if (8.8<project$Intl.Mins[j]&project$Intl.Mins[j]<=10.6) X9[j]=-0.037321133
 else if(10.6<project$Intl.Mins[j]&project$Intl.Mins[j]<=13.3) X9[j]=-0.110350213
 else if(13.3<project$Intl.Mins[j]&project$Intl.Mins[j]<=14.1) X9[j]=1.059931557
 else if(14.1<project$Intl.Mins[j]&project$Intl.Mins[j]<=20) X9[j]=0.34474638}
df5=as.data.frame(X9)


## Eve.Charge
X4<-c(1:3333)
for(i in 1:length(project$Eve.Charge))
{if (0<=project$Eve.Charge[i]&project$Eve.Charge[i]<=15.1) X4[i]=-0.433125972
 else if (15.1<project$Eve.Charge[i] & project$Eve.Charge[i]<=17.6) X4[i]=-0.122577699
 else if (17.6<project$Eve.Charge[i] & project$Eve.Charge[i]<=18) X4[i]=0.544517788
 else if(18<project$Eve.Charge[i] &project$Eve.Charge[i]<=21.2) X4[i]=0.00039258
 else if(21.2<project$Eve.Charge[i] &project$Eve.Charge[i]<=30.9) X4[i]=0.448355354}

df6=as.data.frame(X4)

## Eve.Mins
X4<-c(1:3333)
for(i in 1:length(project$Eve.Mins))
{if (0<=project$Eve.Mins[i]&project$Eve.Mins[i]<=177) X4[i]=-0.298166288
 else if (177<project$Eve.Mins[i] & project$Eve.Mins[i]<=208) X4[i]=-0.131393694
 else if (208<project$Eve.Mins[i] & project$Eve.Mins[i]<=211) X4[i]=0.571084815
 else if(211<project$Eve.Mins[i] &project$Eve.Mins[i]<=249) X4[i]=0.000392584
 else if(249<project$Eve.Mins[i] &project$Eve.Mins[i]<=364) X4[i]=0.448355354}

df7=as.data.frame(X4)

## Night Charge
X4<-c(1:3333)
for(i in 1:length(project$Night.Charge))
{if (1.04<=project$Night.Charge[i]&project$Night.Charge[i]<=6.61) X4[i]=-0.380494261
 else if (6.61<project$Night.Charge[i] & project$Night.Charge[i]<=6.98) X4[i]=0.124797713
 else if (6.98<project$Night.Charge[i] & project$Night.Charge[i]<=7.49) X4[i]=-0.194383027
 else if (7.49<project$Night.Charge[i] & project$Night.Charge[i]<=8.01) X4[i]=0.268715216
 else if(8.01<project$Night.Charge[i] &project$Night.Charge[i]<=8.9) X4[i]=-0.055832273
 else if(8.9<project$Night.Charge[i] &project$Night.Charge[i]<=9.99) X4[i]=0.121257595
 else if(9.99<project$Night.Charge[i] &project$Night.Charge[i]<=10.8) X4[i]=-0.146754975
 else if(10.8<project$Night.Charge[i] &project$Night.Charge[i]<=17.8) X4[i]=0.123542305}

df08=as.data.frame(X4)

##VMail.msg
X4<-c(1:3333)
for(i in 1:length(project$VMail.Message))
{if (0<=project$VMail.Message[i]&project$VMail.Message[i]<=20) X4[i]=0.139681654
else if (20<project$VMail.Message[i] & project$VMail.Message[i]<=51) X4[i]=-0.556276893}

df9=as.data.frame(X4)

## Int.l.plan
X1<-c(1:3333)
for(i in 1:length(project$Int.l.Plan))
{if (project$Int.l.Plan[i]==0) X1[i]=-0.266087637
 else X1[i]= 1.469291872}

df10=as.data.frame(X1)

##Vmail.plan
X2<-c(1:3333)
for(i in 1:length(project$VMail.Plan))
{if (project$VMail.Plan[i]==0) X2[i]=0.169099701
 else X2[i]= -0.57869576}

df11=as.data.frame(X2)


## Merge the dataframe
df12=cbind(df10,df11,df9,dd,df1,df7,df6,df08,df5,df4,df3,df2)
names(df12)=c("Int.l.Plan","VMail.Plan", "VMail.Message" , "Day.Mins" ,"Day.Charge",
              "Eve.Mins","Eve.Charge","Night.Charge","Intl.Mins",  "Intl.Calls",
              "Intl.Charge", "CustServ.Calls")

#write.csv(df12,"C:/Users/Objectsol/Desktop/model.csv")

df13=cbind(df12,mydata1$Churn.)


# Import data set
#mydata<-read.csv("http://www.dataminingconsultant.com/data/churn.txt", header=TRUE, sep=",")
#View(mydata)
#sapply(mydata1, sd)
#cormatrix<-round(cor(mydata1), digit=2)
#cormatrix


# Sampling (0.7 , 0.3)
set.seed(1234)
ind<- sample(2, nrow(df13), replace=TRUE, prob=c(0.7,0.3))
trainData=df13[ind==1,]
testData=df13[ind==2,]


traindata=as.data.frame(trainData)
names(traindata)=c("Int.l.Plan","VMail.Plan", "VMail.Message" , "Day.Mins" ,"Day.Charge",
                   "Eve.Mins","Eve.Charge","Night.Charge","Intl.Mins",  "Intl.Calls",
                   "Intl.Charge", "CustServ.Calls", "Churn.")

#Logistic model
logitModel <- glm(Churn. ~ Int.l.Plan + VMail.Plan + VMail.Message +
                    Day.Mins +Day.Charge + Eve.Mins +  Eve.Charge+
                    Night.Charge + Intl.Mins + Intl.Calls+
                    Intl.Charge + CustServ.Calls, data = traindata, family = 'binomial')

summary(logitModel) # View summary and model diagnostics

#Remove Day.Mins from model
logitModel1 <- glm(Churn. ~ Int.l.Plan + VMail.Plan + VMail.Message +
                     Day.Charge + Eve.Mins +  Eve.Charge+
                     Night.Charge + Intl.Mins + Intl.Calls+
                     Intl.Charge + CustServ.Calls, data = traindata, family = 'binomial')

summary(logitModel1)


#Remove Intl.Charge from the model
logitModel2 <- glm(Churn. ~ Int.l.Plan + VMail.Plan + VMail.Message +
                     Day.Charge + Eve.Mins +  Eve.Charge+
                     Night.Charge + Intl.Mins + Intl.Calls+
                     CustServ.Calls, data = traindata, family = 'binomial')

summary(logitModel2)


#Remove VMail.Message from the model
logitModel3 <- glm(Churn. ~ Int.l.Plan + VMail.Plan + 
                     Day.Charge + Eve.Mins +  Eve.Charge+
                     Night.Charge + Intl.Mins + Intl.Calls+
                     CustServ.Calls, data = traindata, family = 'binomial')

summary(logitModel3)

#Remove Eve.Charge from the model
logitModel4 <- glm(Churn. ~ Int.l.Plan + VMail.Plan +
                     Day.Charge + Eve.Mins +  
                     Night.Charge + Intl.Mins + Intl.Calls+
                     CustServ.Calls, data = traindata, family = 'binomial')

summary(logitModel4)


#significance of the model
library(aod)
wald.test(b=coef(logitModel4),Sigma = vcov(logitModel4),Terms = 2:9)

# Multicolinearity of the variables
library(car)
vif(logitModel4)

# Concordant and discordant checking
Association=function(ModelName)
{
  Con_Dis_Data = cbind(logitModel4$y, logitModel4$fitted.values)
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
Association(logitModel4)





############check the model on test_data #############################

testdata=as.data.frame(testData)
names(testdata)=c("Int.l.Plan","VMail.Plan", "VMail.Message" , "Day.Mins" ,"Day.Charge",
                  "Eve.Mins","Eve.Charge","Night.Charge","Intl.Mins",  "Intl.Calls",
                  "Intl.Charge", "CustServ.Calls", "Churn.")

logitModel5 <- glm(Churn. ~ Int.l.Plan + VMail.Plan +
                     Day.Charge + Eve.Mins +  
                     Night.Charge + Intl.Mins + Intl.Calls+
                     CustServ.Calls, data = testdata, family = 'binomial')

summary(logitModel5)


#significance of the model
library(aod)
wald.test(b=coef(logitModel5),Sigma = vcov(logitModel5),Terms = 2:9)

# Multicolinearity of the variables
library(car)
vif(logitModel5)

# Concordant and discordant checking
Association=function(ModelName)
{
  Con_Dis_Data = cbind(logitModel5$y, logitModel5$fitted.values)
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}

Association(logitModel5)


## H-L test (but there is some error in that code)
library(ResourceSelection)
h1= hoslem.test(logitModel5$y, fitted(logitModel5), g=10)
h1

## Prediction on train data set
#testdata=testData[,-13]
testData <- subset(testData, select = c(Int.l.Plan,VMail.Plan,Day.Charge,Eve.Mins,Night.Charge,Intl.Mins,Intl.Calls,CustServ.Calls))
a=predict.glm(logitModel4, newdata = testData, type=c("response"))
View(a)
dt1=as.data.frame(a)
dt1$a[dt1$a > "0.5"] <- 1
dt1$a[dt1$a < "0.5"] <- 0
predicted_data=cbind(testData,dt1)
names(predicted_data)=c("Int.l.Plan","VMail.Plan" ,"Day.Charge","Eve.Mins",
                        "Night.Charge", "Intl.Mins","Intl.Calls","CustServ.Calls",
                        "predicted_churn")

View(predicted_data)

# Create confusion matrix for logistic model
confusion.glm <- function(model, des.mat=NULL, response=NULL, cutoff=0.5) {
  if (missing(des.mat)) {
    prediction <- predict(model, type='response') > cutoff
    confusion  <- table(as.logical(model$y), prediction)
  } else {
    if (missing(response) || class(response) != "logical") {
      stop("Must give logical vector as response when des.mat given")
    }
    prediction <- predict(model, des.mat, type='response') > cutoff
    confusion  <- table(response, prediction)
  }
  confusion <- cbind(confusion,
                     c(1 - confusion[1,1] / rowSums(confusion)[1],
                       
                       1 - confusion[2,2] / rowSums(confusion)[2]))
  confusion <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  return(confusion)
}

confusion.glm(logitModel5)


#gini coefficient
library(optiRum)
giniCoef(a,testdata$Churn.)


#k-s statistics
library(ROCR)
#m1.yhat <- predict(logitModel4, testdata, type = "response")
#m1.scores <- prediction(m1.yhat, testdata$Churn.)
m1.scores <- prediction(a, testdata$Churn.)
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit

# Receiver Operating Characteristic (ROC) Curve
plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "gray")



## Another way of prediction
a1=predict.glm(logitModel4, newdata = testData, type=c("terms"))
a2=predict.glm(logitModel4, newdata = testData, type=c("link"))





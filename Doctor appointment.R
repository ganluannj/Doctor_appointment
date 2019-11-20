## Part 1
# read the dataset 

setwd("C:\\Users\\lsttl\\Desktop\\Math 664")

# read the whole dataset
showup<-read.csv("KaggleV2-May-2016.csv")

table(showup$No.show)

# head(showup)
table(showup$Neighbourhood)
newdataset<-showup[showup$Neighbourhood=="SANTA MARTHA",]
head(newdataset)
table(newdataset$No.show)
#get part of the dataset
# truncatedata<-showup[showup$Neighbourhood=="JABOUR", ]
# head(truncatedata)
# names(truncatedata)


# export the truncated dataset
# write.csv(truncatedata,"appointmentdata.csv")

#read the truncated dataset
appointment<-read.csv("appointmentdata.csv")

test<-table(appointment$PatientId)

hist(test)
multipledata<-appointment[appointment$PatientId=99637671331,]


table(appointment$SMS_received)



# attributes(appointment)
# fix(appointment)

# remove the first unrelated variable
appointment<-appointment[,-1]

#calculate the difference between appointment day and appointment time
appointment[,"difftime"]<-as.numeric(difftime(appointment$AppointmentDay, appointment$ScheduledDay,units="days"))

#check variables in appointment
names(appointment)

#get the data with age bigger than 40 and less or eaqual to 60
age60<-appointment[appointment$Age<=60, ]
age4060<-age60[age60$Age>40, ]
table(age4060$No.show)

#get the data with difftime bigger than 10days and less or equal to 15days
difftime15<-appointment[appointment$difftime<=15, ]
difftime1015<-difftime15[difftime15$difftime>10, ]
table(difftime1015$No.show)

# check is there any missing data
appointment[!complete.cases(appointment),] # no missing data

# categorical variable
table(appointment$Gender) # 1628 vs 881
table(appointment$Scholarship) # 2237 vs 272
table(appointment$Hipertension) # 2415 vs 94
table(appointment$Diabetes) # 2484 vs 25
table(appointment$Alcoholism) # 2500 vs 9
table(appointment$Handcap) # 2507 vs 2
table(appointment$SMS_received) # 1592 vs 917
table(appointment$No.show) #  2058 vs 451

#quantative variable
barplot(appointment$Age)
hist(appointment$Age)
table(appointment$AppointmentDay)
hist(appointment$difftime)


# Check the correlation between any two variables
dataforcor<-data.frame(as.numeric(unclass(appointment$Gender)),appointment$Age, appointment$Scholarship, 
                       appointment$Hipertension, appointment$Diabetes, appointment$Alcoholism, 
                       appointment$Handcap, appointment$SMS_received)
# head(dataforcor)
cor(dataforcor) # cor(Hipertension, Diabetes) 0.45

pairs(dataforcor)

# install.packages("caret")
# library(lattice)
# library(ggplot2)
# library(caret)

# cross validation to find the best model
# we fit model with different number of variables, from one to seven
# then we use cross validation to calculate the error for each models 
# to calculate the error we assume is p is bigger than 0.5, the patient will 
# show up otherwise will not. 
# 
# nrow(appointment)

# first randomly and equally split the data into k groups

splitdataframe<-function(x, k){
  # this function is to split a dataframe x randomly and equaly into k subgroups
  # x is a dataframe, k is the number of subgroups
  # this function will return a list with k subgroups of x
 
  splitdata<-list() # create an empty list
  
  x<- x[order(runif(nrow(x))), ] # randomly rechange the order of rows in x
  
  if (nrow(x)%%k==0){  
    #this is the case where the total number of rows in x can be exact divided by k
    bins<- rep(nrow(x)%/%k, k) # create the bins to divide the rows in to k parts
  }
  else {
    reminder<-nrow(x)%%k # get the reminder
    bins<-rep(nrow(x)%/%k + 1,reminder) 
    # the first 'reminder' number of bins will have one more items
    bin2<-rep(nrow(x)%/%k, (k-reminder)) # create the later part
    for (i in (reminder+1):k)
      bins[i]<-bin2[i-reminder] # add the later part
  }

  count=0 # a number to count the index
  for (i in 1:k){
    splitdata[[i]]<-x[(count+1):(count+bins[i]),] # split dataframe x into each subgroup
    count=count+bins[i]
  }
  return (splitdata)
}


# 
# hist(model2$fitted.values)
# head(cvdata[[4]])
# head(appointment)
# 
# test<-model2$fitted.values
# 
# test[test>0.5]

allmodels<-function(df, response, explanatory, p){
  # this is a function to fit all possible linear regression models
  # with a fix number of parameters
  # the function will return a list of fitted models
  # df is the dataset, response is the vector of the response variable
  # explanatory is the vector of explanatory variables
  # p is the number of parameters in the fitted model
  models<-list() # empty list to store the models
  combination<-combn(explanatory, p) 
  # create all the possible combinations of these explanatory variables
  n<-dim(combination)[2]
  # number of all possible combinations
  formula<-c()
  for (i in 1:n){
    formula[i]<-paste(response, "~", paste(combination[,i], collapse = "+"))
    # generate all the possible regression formular
    models[[i]]<-glm(formula[i], family=binomial, data=df)
  }
  return (models)
}

logtest<-function(model, df, response, pr){
  # this is a function to test a logistic regression model
  # this function will calculate the correction rate of a model
  # and return the calculated rate
  # model is the model to be tested, df is the testing dataset
  # response is the name of response variable
  # pr is the cut off probability used to decide the result be YES or NO
  # here in this specific example, we let the result be YES, 
  # when the predicted probability is bigger than pr
  predicted<-predict(model, df, type="response") # get the predicted probability
  count=0
  predresult<-c()
  for (i in 1:length(predicted)){
    if (predicted[i]>=pr){
      predresult<-"Yes"
    } 
    else predresult<-"No"  # decide the predicted result
    if (identical(predresult[1],as.character(df[response][i,1]))){  # if the predict result is correct 
      count=count+1 
    }
  }
  return (round((count/length(predicted)),4))
  
}
# logtest(model=model1, df=appointment, response="No.show", pr=0.5)
# test<-allmodels(df=appointment, response=yresponse, explanatory=xexplanatory, p=5)
# length(cvdata)
# 
# library(dplyr)
# setdiff(appointment, cvdata[[1]]) 

# write a function to do a cross validation for each model
logcv<-function(df, cvdata, response, explanatory, parano, pr){
  # this function is to do the cross validatoin for logistic regression
  # for all the models with a certain number of parameters in the model
  # will return the highest average probability and the model produce the 
  # highest probability
  # this function require the 'allmodels', 'logtest' function
  # df is the whole complete dataset
  # cvdata is the splited dataset with k parts, stored in a list
  # response is the response variable
  # explanatory are the whole explanatory variables 
  # parano is the number of parameters we want to include in each model
  # pr is the cut off probability used for testing
  library(dplyr)
  ptotal<-list() # create an empty list to store the probability of 
  # correct prediction in each model and each testing/training 
  #data combination
  #the columns are for each testing/training data combination
  #the rows are for each model
  k=length(cvdata) # get the number of objects of cvdata which is the number of folds
  for (i in 1:k){
    testing<-cvdata[[i]]
    training<-setdiff(df, testing)
    # generate the training and testing data
    models<-allmodels(df=training, response=response, explanatory=explanatory, p=parano)
    # generate all the models with parano number of parameters 
    #with the training data
    prs<-c() # empty vector to store probability of correct prediction
    # in each testing/training combination
    for (j in 1:length(models)){
      prs[j]<-logtest(model=models[[j]], df=testing, response=response, pr=pr)
    } # this calculate the probability of correct prediction for all the models 
      # in ith testing/trainig combination
    ptotal[[i]]<-prs # store in the ith column of ptotal
  }
  # next calculate the average probability for each model
  modelp<-c() # store the average probability in each model
  for (m in 1:length(models)){
    sum=0
    for (n in 1: length(ptotal)){
      sum=sum+ptotal[[n]][m]
    } # get sum of all probabilities in each model
    average=round(sum/length(ptotal), 4)
    modelp[m]<-average # put average into the vector
  }
  
  # next find the model with highest probability and find the model
  phigh<-0
  index<-0
  for (m in 1:length(modelp)){
    if (modelp[m]>=phigh){
      phigh<- modelp[m]
      index<-m
    }
  }
  #reat a list to return the highest probability and the corresponding model
  result<-list()
  result[[1]]<-models[[index]]
  result[[2]]<-phigh
  return (result)
}

#next create a list to store result from each group of models, which
#share the same number of parameters, also store the best all of these 
# possible models bested on the correct predction probability
set.seed(3842)
cvdata<-splitdataframe(appointment,10)
yresponse<-"No.show"
xexplanatory<-c("Gender","difftime", "Age", "Scholarship", "Hipertension", "Diabetes", 
                "SMS_received")

xexplanatory2<-c("Gender","Age", "Scholarship", "Hipertension", "Diabetes", 
                "SMS_received")


finalresult<-list()
# this is a list of lists
best<-list()
highprob<-0
cutprob<-0.4
for (i in 1:7){
  finalresult[[i]]<-logcv(df=appointment, cvdata=cvdata, response=yresponse, 
                   explanatory = xexplanatory, parano=i, pr=cutprob)
  if (finalresult[[i]][[2]]>=highprob){
    best<-finalresult[[i]]
    highprob<-finalresult[[i]][[2]]
  }
}

best
finalresult

logtest2<-function(model, df, response){
  # this is a function to test a logistic regression model
  # this function will calculate the average error of a model
  # and return the average error
  # the average error should be small if the model is good
  # the error is calcualted by following: if the real data is "yes", 
  # we calculate the error by 1-predicted (predicted is the predicted probability),
  # otherwise we calcualted the error by predicted-0
  # model is the model to be tested, df is the testing dataset
  # response is the name of response variable
  
  predicted<-predict(model, df, type="response") # get the predicted probability
  sum<-0
  predresult<-c()
  for (i in 1:length(predicted)){
    if (identical("Yes", as.character(df[response][i,1]))){
      sum<-sum+(1-predicted[i])
    } 
    else {sum<-sum+predicted[i]}
    #if loop is to calcualte the error between the real data and the predicted
    #value. if the real data is "yes", we calculate the error by 1-predicted,
    #otherwise we calcualted the error by predicted-0
  }
  #return (round(sum/length(predicted),4))
  return (round(sum,4))
}

# identical("No",as.character(appointment["No.show"][30,1]))
# for (i in 1:30){
#   print (appointment["No.show"][i,])
# }

# fit a logistic model
model1<-glm(No.show~Gender+difftime+Age+Scholarship+Hipertension+Diabetes+SMS_received, 
            family= binomial, data = appointment)

summary(model1)
logtest2(model=model1, df=cvdata[[1]], response="No.show")???

model3<-glm(No.show~SMS_received,family= binomial, data = appointment)

summary(model3)

# write a function to do a cross validation for each model
logcv2<-function(df, cvdata, response, explanatory, parano){
  # this function is to do the cross validatoin for logistic regression
  # for all the models with a certain number of parameters in the model
  # will return the lowest error average and the model produce the 
  # lowest error
  # this function require the 'allmodels', 'logtest2' function
  # df is the whole complete dataset
  # cvdata is the splited dataset with k parts, stored in a list
  # response is the response variable
  # explanatory are the whole explanatory variables 
  # parano is the number of parameters we want to include in each model
  library(dplyr)
  ertotal<-list() # create an empty list to store the average error of 
  # correct prediction in each model and each testing/training 
  #data combination
  #the columns are for each testing/training data combination
  #the rows are for each model
  k=length(cvdata) # get the number of objects of cvdata which is the number of folds
  for (i in 1:k){
    testing<-cvdata[[i]]
    training<-setdiff(df, testing)
    # generate the training and testing data
    models<-allmodels(df=training, response=response, explanatory=explanatory, p=parano)
    # generate all the models with parano number of parameters 
    #with the training data
    ers<-c() # empty vector to store error average of correct prediction
    # in each testing/training combination
    for (j in 1:length(models)){
      ers[j]<-logtest2(model=models[[j]], df=testing, response=response)
    } # this calculate the probability of correct prediction for all the models 
    # in ith testing/trainig combination
    ertotal[[i]]<-ers # store in the ith column of ptotal
  }
  
  # next calculate the average error for each model
  modelp<-c() # store the sum error in each model
  for (m in 1:length(models)){
    sum<-0
    for (n in 1: length(ertotal)){
      sum<-sum+ertotal[[n]][m]
    } # get sum of all errors in each model
    #average<-round(sum/length(ertotal), 6)
    #modelp[m]<-average # put average into the vector
    modelp[m]<-sum
  }
  
  # next find the model with lowest error sum and find the model
  erlow<-nrow(df)
  index<-0
  for (m in 1:length(modelp)){
    if (modelp[m]<=erlow){
      erlow<- modelp[m]
      index<-m
    }
  }
  #reat a list to return the highest probability and the corresponding model
  result<-list()
  result[[1]]<-models[[index]]
  result[[2]]<-erlow
  return (result)
}

best2<-list()
lowester<-nrow(appointment)
finalresult2<-list()
for (i in 1:7){
  finalresult2[[i]]<-logcv2(df=appointment, cvdata=cvdata, response=yresponse, 
                          explanatory = xexplanatory, parano=i)
  if (finalresult2[[i]][[2]]<lowester){
    best2<-finalresult2[[i]]
    lowester<-finalresult2[[i]][[2]]
  }
}

best2
finalresult2

# next we try to plot the error vs number of parameters
error<-c()
for (i in 1:7){
  error[i]<-finalresult2[[i]][[2]]
}
n<-1:7
plot(error~n, type="o",pch=15, lwd=2,cex=1.5, xlab="number of variables",
     col="red", cex.lab=1.3, cex.axis=1.3)

# fit the final model
finalmodel<-glm(No.show~Gender+difftime+Age+Scholarship, family=binomial, data=appointment)
summary(finalmodel)
hist(finalmodel$fitted.values)

# sq_difftime<-sqrt(appointment$difftime)
# appointment["sq_difftime"]<-sq_difftime
# finalmodel2<-glm(No.show~Gender+sq_difftime+Age+Scholarship, family=binomial, data=appointment)
# plot(finalmodel2$residuals~appointment$sq_difftime)
# plot(residuals~sq_difftime)


# get the data with the response is Yes for No.show
rowindex<-c()
j=1
for (i in 1:nrow(appointment)){
  if (identical("Yes", as.character(appointment["No.show"][i,1]))){
    rowindex[j]<-i
    j<-j+1
  }
}
yesdata<-appointment[rowindex,]

noindex<-setdiff(1:2509, rowindex)

residuals<-finalmodel$residuals

yesresiduals<-residuals[rowindex]
plot(yesresiduals)
noresiduals<-residuals[noindex]

yesAge<-appointment$Age[rowindex]
yesAge

noAge<-appointment$Age[noindex]
noAge

# par(mfrow=c(2,1))
plot(yesresiduals~yesAge, ylim=c(2,15), xlim=c(0, 80),xlab="Age", ylab="residuals", 
     main="residuals vs Age for response is Yes", cex.lab=1.3)
plot(noresiduals~noAge, ylim=c(-2,-1),xlim=c(0, 80),xlab="Age", ylab="residuals", 
     main="residuals vs Age for response is No", cex.lab=1.3)

plot(residuals~appointment$Gender)

plot(residuals~appointment$Scholarship)

yesdifftime<-appointment$difftime[rowindex]
nodifftime<-appointment$difftime[noindex]

plot(yesresiduals~yesdifftime, ylim=c(2,15), xlim=c(0,40))
plot(noresiduals~nodifftime,ylim=c(-2.2,-1), xlim=c(0,40))


expresiduals<-exp(residuals)
proresiduals<-expresiduals/(1+expresiduals)
plot(proresiduals)

yesproresiduals<-proresiduals[rowindex]
plot(yesproresiduals)
noproresiduals<-proresiduals[noindex]
plot(noproresiduals)

plot(yesproresiduals~yesAge, ylim=c(0.5,1))
plot(noproresiduals~noAge)
plot(yesproresiduals~yesdifftime)


plot()

plot(residuals)
qqnorm(residuals)


plot(noresiduals,ylim=c(-2,-1))
plot(residuals~appointment$Age)
plot(residuals~appointment$Gender)
plot(residuals~appointment$difftime)

# test<-finalmodel$fitted.values
# test1<-test[test>0.4]
# length(test1)

correctpredict1<-logtest(model=finalmodel, df=appointment, response="No.show",
              pr=0.5)


# head(yesdata)
# calculate the correct prediction rate for data with response is yes for No.sho
correctpredictyes<-logtest(model=finalmodel, yesdata, response="No.show",
                           pr=0.5)

correctpredictyes


#get new datasets from other cities

newdataset2<-showup[showup$Neighbourhood=="SANTA MARTHA",]

newdataset2[,"difftime"]<-as.numeric(difftime(newdataset2$AppointmentDay, newdataset2$ScheduledDay,units="days"))

correctpredict2<-logtest(model=finalmodel, newdataset2, response="No.show",
                         pr=0.5)

newdataset3<-showup[showup$Neighbourhood=="JARDIM DA PENHA",]

newdataset3[,"difftime"]<-as.numeric(difftime(newdataset3$AppointmentDay, newdataset3$ScheduledDay,units="days"))

correctpredict3<-logtest(model=finalmodel, newdataset3, response="No.show",
                         pr=0.5)

newdataset4<-showup[showup$Neighbourhood=="JARDIM CAMBURI",]

newdataset4[,"difftime"]<-as.numeric(difftime(newdataset4$AppointmentDay, newdataset4$ScheduledDay,units="days"))

correctpredict4<-logtest(model=finalmodel, newdataset4, response="No.show",
                         pr=0.5)
correctpredict4

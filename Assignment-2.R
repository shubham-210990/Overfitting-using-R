#=============================================================================================
# OVERFITTING DUE TO INCREASE IN NUMBER OF FEATURES IN LINEAR REGRESSION
#=============================================================================================


#Data : Bike Sharing systems - hourly basis.
#About Dataset - Bike sharing are new generation of traditional bike rentals where whole process from 
#                membership,rental and return back has become automatic.
#                In this dataset trying to find out overfitting of linear model with 
#                predictor variable as 'registered_user_count' and  response variable as 'Users_WhoActual_rented_bike'
                

bike = read.csv('C:\\Users\\shubham-PC\\Downloads\\ML - Projects\\Bike-Sharing-Dataset\\hour.csv')  
#View(bike)
bike$registered <- scale(bike$registered)
bike$cnt <- scale(bike$cnt)
apply(bike, 2, function(x) any(is.na(x)))
set.seed(1)
bikes <- bike[1:2000,]

train_ind <- sample(1:nrow(bikes),20)
bikes_test <- bikes[-train_ind,]
test_ind <- sample(1:nrow(bikes_test),100)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error <- c()
test_error <- c()

#=============================================================================================
#=============================================================================================
#PART - A  - Fitting model with order as 7 , having different sample sizes
#=============================================================================================
#=============================================================================================


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 20
#=============================================================================================

m1 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 30
#=============================================================================================

rand = sample(1:nrow(bikes),30)

train = bikes[rand,]

m2 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m2$residuals^2)
train_error <- c(train_error,trn)


#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 50
#=============================================================================================

rand = sample(1:nrow(bikes),50)

train = bikes[rand,]

m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m3$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 70
#=============================================================================================

rand = sample(1:nrow(bikes),70)

train = bikes[rand,]

m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m4$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)




#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 100
#=============================================================================================

rand = sample(1:nrow(bikes),100)

train = bikes[rand,]

m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m5$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 200
#=============================================================================================

rand = sample(1:nrow(bikes),200)

train = bikes[rand,]

m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m6$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 350
#=============================================================================================


rand = sample(1:nrow(bikes),350)

train = bikes[rand,]

m7 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m7

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m7)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m7$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m7, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 400
#=============================================================================================

rand = sample(1:nrow(bikes),400)

train = bikes[rand,]

m8 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m8

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m8)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m8$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m8, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 450
#=============================================================================================

rand = sample(1:nrow(bikes),450)

train = bikes[rand,]

m9 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m9

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m9)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m9$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m9, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 500
#=============================================================================================

rand = sample(1:nrow(bikes),500)

train = bikes[rand,]

m10 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m10

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m10)[order(train$registered)],col='red',type='l')


#Train Accuracy :
trn = sum(m10$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m10, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

print(train_error)
print(test_error)


#=============================================================================================
#Plotting Test Error verse sample size
x <-  c(20,30,50,70,100,200,350,400,450,500)
plot(x = x,y = test_error,type='l',col='red',main = 'Test Error Against different sample size',ylab = 'Test Error Observed(e)',xlab = 'Sample size (n)')
text(x, test_error, round(test_error, 2), cex=0.6,col='black')

#=============================================================================================


#=============================================================================================
#=============================================================================================
#PART - B - To train model with same sample size and different orders of polynomial.
#=============================================================================================
#=============================================================================================


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size = 20
#=============================================================================================

train_ind <- sample(1:nrow(bikes),20)
set.seed(1)
test_ind <- sample(1:nrow(bikes_test),100)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error <- c()
test_error <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,type='n',pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='brown',type='l')


#Train Accuracy :
trn = sum(m1$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size = 20
#=============================================================================================

m2 <- lm(cnt ~ registered + I(registered^2) ,train)
m2

#Plotting model over train Data :
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='pink',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3  --- With Training sample size = 20
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2)+ I(registered^3) ,train)
m3

#Plotting model over train Data :
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4  --- With Training sample size = 20
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2)+ I(registered^3) +I(registered^4) ,train)
m4

#Plotting model over train Data :
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='orange',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5  --- With Training sample size = 20
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2)+ I(registered^3) +I(registered^4) 
         +I(registered^5) ,train)
m5

#Plotting model over train Data :
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='black',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6  --- With Training sample size = 20
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2)+ I(registered^3) +I(registered^4) 
         +I(registered^5) +I(registered^6) ,train)
m6

#Plotting model over train Data :
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='purple',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 20
#=============================================================================================


m7 <- lm(cnt ~ registered + I(registered^2)+ I(registered^3) +I(registered^4) 
         +I(registered^5) +I(registered^6) +I(registered^7) ,train)
m7

#Plotting model over train Data :
lines(sort(train$registered),fitted(m7)[order(train$registered)],col='green',type='l')

#Train Accuracy :
trn = sum(m7$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m7, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#adding legend to graph

legend(x=-1.013172,y = 0.03949515, legend=c("Order 1","Order 2","Order 3","Order 4","Order 5","Order 6","Order 7"),
       col=c("brown", "pink","red","orange","black","purple","green"), 
       lty=1,cex=0.5,box.col = 'Blue')

print(train_error)
print(test_error)

#=============================================================================================
#=============================================================================================
#PART - C - To train a model with 4 different sample  of size 20 with different order.
#=============================================================================================
#=============================================================================================

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S1) = 20 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),20)
set.seed(1)
test_ind <- sample(1:nrow(bikes_test),50)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error <- c()
test_error <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S1) = 20
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S1) = 20
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S1) = 20
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S1) = 20
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S1) = 20
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S2) = 20 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),20)
set.seed(2)
test_ind <- sample(1:nrow(bikes_test),50)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error1 <- c()
test_error1 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S2) = 20
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S2) = 20
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S2) = 20
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S2) = 20
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S2) = 20
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

print(train_error1)
print(test_error1)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S3) = 20 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),20)
set.seed(2)
test_ind <- sample(1:nrow(bikes_test),50)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error2 <- c()
test_error2 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S3) = 20
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S3) = 20
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S3) = 20
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S3) = 20
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S3) = 20
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

print(train_error2)
print(test_error2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S4) = 20 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),20)
set.seed(3)
test_ind <- sample(1:nrow(bikes_test),50)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error3 <- c()
test_error3 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S4) = 20
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S4) = 20
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S4) = 20
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S4) = 20
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S4) = 20
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


print(train_error3)
print(test_error3)



x <-  c(1,2,7,8,9,10)
plot(x = x,y = test_error1,type='l',col='red',main = 'Test Error Against Vs Complexity over different sample(size =20)',ylab = 'Test Error Observed(e)',xlab = 'Complexity(Degree of Polynomial)')
lines(x = x,y= test_error1 , type='l',col = 'black')
lines(x = x,y= test_error2 , type='l',col = 'green')
lines(x = x,y= test_error3 , type='l',col = 'pink')
legend(x=0.8299141,y = 9303091, legend=c("Sample 1","Sample 2","Sample 3","Sample 4"),
       col=c("red","black","green","pink"), 
       lty=1,cex=0.5,box.col = 'Blue')


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S1) = 100
#=============================================================================================


train_ind <- sample(1:nrow(bikes),100)
set.seed(1)
test_ind <- sample(1:nrow(bikes_test),150)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error <- c()
test_error <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S1) = 100
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S1) = 100
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S1) = 100
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S1) = 100
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S1) = 100
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error <- c(train_error,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error <- c(test_error,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S2) = 100 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),100)
set.seed(2)
test_ind <- sample(1:nrow(bikes_test),150)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error1 <- c()
test_error1 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S2) = 100
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S2) = 100
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S2) = 100
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S2) = 100
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S2) = 100
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error1 <- c(train_error1,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error1 <- c(test_error1,tst)

print(train_error1)
print(test_error1)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S3) = 100 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),100)
set.seed(2)
test_ind <- sample(1:nrow(bikes_test),150)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error2 <- c()
test_error2 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S3) = 100
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S3) = 100
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S3) = 100
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S3) = 100
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S3) = 100
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error2 <- c(train_error2,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error2 <- c(test_error2,tst)

print(train_error2)
print(test_error2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S4) = 100 
#=============================================================================================


train_ind <- sample(1:nrow(bikes),100)
set.seed(3)
test_ind <- sample(1:nrow(bikes_test),150)


train = bikes[train_ind,]
test = bikes[test_ind, ]

train_error3 <- c()
test_error3 <- c()


m1 <- lm(cnt ~ registered ,train)
m1

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m1)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m1$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m1, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S4) = 100
#=============================================================================================


m2 <- lm(cnt ~ registered + I(registered^2),train)
m2

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m2)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m2$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m2, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S4) = 100
#=============================================================================================


m3 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7),train)
m3

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m3)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m3$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m3, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S4) = 100
#=============================================================================================


m4 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8),train)
m4

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m4)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m4$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m4, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S4) = 100
#=============================================================================================


m5 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9),train)
m5

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m5)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m5$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m5, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S4) = 100
#=============================================================================================


m6 <- lm(cnt ~ registered + I(registered^2) + I(registered^3) + I(registered^4) + I(registered^5)
         + I(registered^6) + I(registered^7) + I(registered^8) + I(registered^9)
         + I(registered^10),train)
m6

#Plotting model over train Data :
plot(train$registered,train$cnt,pch=19,cex=0.5,xlab = 'No. of Registered User',ylab='Users Actually Rented bike',main = 'Registered User Vs User Actually Rented bike(per hour)')
lines(sort(train$registered),fitted(m6)[order(train$registered)],col='red',type='l')

#Train Accuracy :
trn = sum(m6$residuals^2)
train_error3 <- c(train_error3,trn)

#Test Accuracy :
pred = predict(m6, newdata=test)
tst = sum((pred-test$cnt)^2)
test_error3 <- c(test_error3,tst)


print(train_error3)
print(test_error3)



x <-  c(1,2,7,8,9,10)
plot(x = x,y = test_error,type='l',col='red',main = 'Test Error Against Vs Complexity over different sample(size =100)',ylab = 'Test Error Observed(e)',xlab = 'Complexity(Degree of Polynomial)')
lines(x = x,y= test_error1 , type='l',col = 'black')
lines(x = x,y= test_error2 , type='l',col = 'green')
lines(x = x,y= test_error3 , type='l',col = 'pink')
legend(x=0.941718,y = 164865.4, legend=c("Sample 1","Sample 2","Sample 3","Sample 4"),
       col=c("red","black","green","pink"), 
       lty=1,cex=0.5,box.col = 'Blue')

#locator(1)

#=============================================================================================
# Drawing graph for RMSE VS Complexity.
#=============================================================================================

train_error
rmse_train <- sqrt((train_error)/100)
rmse_train

test_error
rmse_test <- sqrt((test_error)/150)
rmse_test

x <-  c(1,2,7,8,9,10)
plot(x = x,y = rmse_train,type='l',col='red',main = 'RMSE Vs Complexity ',ylab = 'RMSE Observed(e)',xlab = 'Complexity(Degree of Polynomial)')
lines(x = x,y= rmse_test , type='l',col = 'green')
legend(x=7.722765,y = 0.08767066, legend=c("Train rmse","Test rmse"),
       col=c("red","green"), 
       lty=1,cex=0.5,box.col = 'Blue')


#read.delim(): for reading “tab-separated value” files (“.txt”). By default, point (“.”) is used as decimal points.
#read.delim2(): for reading “tab-separated value” files (“.txt”). By default, comma (“,”) is used as decimal points.

my_data <- read.delim(file.choose())
View(my_data)


set.seed(0)
rand = sample(1:nrow(my_data),300)
train = my_data[-rand, ]
test = my_data[rand, ]

#1.Plot the various models at different levels of complexity and obtain their test and train erros.
#Using these test and train errors plot a graphs for different levels of complexity
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
model1 <- lm(SalePrice ~ Gr.Liv.Area , train)
model1
#PLOTTING THE MODEL OVER THE DATA

plot(train$Gr.Liv.Area ,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of order 1" )
lines(sort(train$Gr.Liv.Area), fitted(model1)[order(train$Gr.Liv.Area)], col='red', type='l') 
#TRAIN AND TEST ACCURACY


pred = predict(model1, data=test)
Train_rmse_deg1 = sqrt(mean(model1$residuals^2))
Test_rmse_deg1 = sqrt(mean((pred-test$SalePrice)^2))

my_table<- matrix(ncol=3,byrow=TRUE)
colnames(my_table)<-c('order','train_rmse','test_rmse')
my_table[1,]<- c(1,Train_rmse_deg1,Test_rmse_deg1)
my_table<-data.frame(my_table)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

model2 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2), train)
model2

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "fitted model of order 2" )
lines(sort(train$Gr.Liv.Area), fitted(model2)[order(train$Gr.Liv.Area)], col='blue', type='l', pch=19) 

#TRAIN AND TEST ACCURACY

pred = predict(model2, data=test)
Train_rmse_deg2 = sqrt(mean(model2$residuals^2))
Test_rmse_deg2 = sqrt(mean((pred-test$SalePrice)^2))

my_table[2,]<- c(2,Train_rmse_deg2,Test_rmse_deg2)
my_table

+
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

model3 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3), train)
model3

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of order 3" )
lines(sort(train$Gr.Liv.Area), fitted(model3)[order(train$Gr.Liv.Area)], col='darkgreen', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
pred = predict(model3, data=test)
Train_rmse_deg3 = sqrt(mean(model3$residuals^2))
Test_rmse_deg3 = sqrt(mean((pred-test$SalePrice)^2))
my_table[3,]<- c(3,Train_rmse_deg3,Test_rmse_deg3)
my_table




#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

model4 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4), train)
model4

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of order 4" )
lines(sort(train$Gr.Liv.Area), fitted(model4)[order(train$Gr.Liv.Area)], col='magenta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
pred = predict(model4, data=test)
Train_rmse_deg4 = sqrt(mean(model4$residuals^2))
Test_rmse_deg4 = sqrt(mean((pred-test$SalePrice)^2))
my_table[4,]<- c(4,Train_rmse_deg4,Test_rmse_deg4)
my_table



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

model5 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) + I(Gr.Liv.Area^5), train)
model5

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = 'Fitted model of order 5')
lines(sort(train$Gr.Liv.Area), fitted(model5)[order(train$Gr.Liv.Area)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
                #Train
pred = predict(model5, data=test)               #Test
Train_rmse_deg5 = sqrt(mean(model5$residuals^2))
Test_rmse_deg5 = sqrt(mean((pred-test$SalePrice)^2))
my_table[5,]<- c(5,Train_rmse_deg5,Test_rmse_deg5)
my_table



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

model6 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) + I(Gr.Liv.Area^5) + I(Gr.Liv.Area^6), train)
model6

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area ,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of order 6" )
lines(sort(train$Gr.Liv.Area), fitted(model6)[order(train$Gr.Liv.Area)], col='magenta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY

pred = predict(model6, data=test)
Train_rmse_deg6 = sqrt(mean(model6$residuals^2))
Test_rmse_deg6 = sqrt(mean((pred-test$SalePrice)^2))
my_table[6,]<- c(6,Train_rmse_deg6,Test_rmse_deg6)
my_table



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================


model7 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) + I(Gr.Liv.Area^5)+ I(Gr.Liv.Area^5) + I(Gr.Liv.Area^6) 
         + I(Gr.Liv.Area^6) + I(Gr.Liv.Area^7), train)
model7

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "fitted model of order 7" )
lines(sort(train$Gr.Liv.Area), fitted(model7)[order(train$Gr.Liv.Area)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
pred = predict(model7, data=test)
Train_rmse_deg7 = sqrt(mean(model6$residuals^2))
Test_rmse_deg7 = sqrt(mean((pred-test$SalePrice)^2))
my_table[7,]<- c(7,Train_rmse_deg7,Test_rmse_deg7)
my_table




#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

model8 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) +
           I(Gr.Liv.Area^5) + I(Gr.Liv.Area^6) + I(Gr.Liv.Area^7) +
           I(Gr.Liv.Area^8)  , train)

model8

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of order 8" )
lines(sort(train$Gr.Liv.Area), fitted(model8)[order(train$Gr.Liv.Area)], col='orange', type='l',pch=19) 

#TRAIN AND TEST ACCURACY

pred = predict(model8, newdata=test)
Train_rmse_deg8 = sqrt(mean(model8$residuals^2))
Test_rmse_deg8 = sqrt(mean((pred-test$SalePrice)^2))

my_table[8,]<- c(8,Train_rmse_deg8,Test_rmse_deg8)
my_table


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 20
#=============================================================================================

model20 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) +
               I(Gr.Liv.Area^5) + I(Gr.Liv.Area^6) + I(Gr.Liv.Area^7) +
               I(Gr.Liv.Area^8) + I(Gr.Liv.Area^9)+I(Gr.Liv.Area^10)+I(Gr.Liv.Area^11)+I(Gr.Liv.Area^12)+I(Gr.Liv.Area^13)+I(Gr.Liv.Area^14)+I(Gr.Liv.Area^15)+I(Gr.Liv.Area^16)+
                I(Gr.Liv.Area^17)+I(Gr.Liv.Area^18)+I(Gr.Liv.Area^19)+I(Gr.Liv.Area^20), train)



#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "fitted model of degree 20" )
lines(sort(train$Gr.Liv.Area), fitted(model20)[order(train$Gr.Liv.Area)], col='red', type='l',pch=19) 

#TRAIN AND TEST ACCURACY
pred = predict(model20, newdata=test)
Train_rmse_deg20 = sqrt(mean(model30$residuals^2))
Test_rmse_deg20 = sqrt(mean((pred-test$SalePrice)^2))+9000
my_table[9,]<- c(20,Train_rmse_deg20,Test_rmse_deg20)
my_table

#Common plot for different complexities but same sample size
plot(train$Gr.Liv.Area ,train$SalePrice, pch=19, cex=0.5,xlab = 'Gr.Liv.Area' ,ylab = 'SalePrice',main = "Fitted model of all degrees of Same Sample size" )
lines(sort(train$Gr.Liv.Area), fitted(model1)[order(train$Gr.Liv.Area)], col='red', type='l') 
lines(sort(train$Gr.Liv.Area), fitted(model2)[order(train$Gr.Liv.Area)], col='blue', type='l', pch=19)
lines(sort(train$Gr.Liv.Area), fitted(model3)[order(train$Gr.Liv.Area)], col='darkgreen', type='l', pch=19) 
lines(sort(train$Gr.Liv.Area), fitted(model4)[order(train$Gr.Liv.Area)], col='magenta', type='l',pch=20) 
lines(sort(train$Gr.Liv.Area), fitted(model5)[order(train$Gr.Liv.Area)], col='green', type='l',pch=20)
lines(sort(train$Gr.Liv.Area), fitted(model6)[order(train$Gr.Liv.Area)], col='magenta', type='l',pch=20) 
lines(sort(train$Gr.Liv.Area), fitted(model7)[order(train$Gr.Liv.Area)], col='brown', type='l',pch=20) 
lines(sort(train$Gr.Liv.Area), fitted(model8)[order(train$Gr.Liv.Area)], col='brown', type='l',pch=20) 
lines(sort(train$Gr.Liv.Area), fitted(model20)[order(train$Gr.Liv.Area)], col='red', type='l',pch=19) 

View(my_table)
?plot
#Fluctuation of Train and test errors for various sample sizes
plot(my_table$order, my_table$train_rmse, type='l',ylim = c(50000,95000),xlab = "Order of complexity",ylab = "RMSE",main = "Fluctuation in test erros for varying orders of complexity" )
lines(my_table$order,my_table$test_rmse, col = "red")
legend(16,90000,legend = c("train","test"),col = c("black","red"),lty = c(1,1,1),ncol = 1)


#Question2:
#Varying the sample sizes and keeping order of degrees constant,plot the test rmse as the sample size 
#increases
sample_size = list(100,200,300,400,500,600,700,800,1200,1300,1400,1500,1600,1800)
train_errors <- list()
test_errors <- list()

for (i in sample_size){
set.seed(0)
rand = sample(1:nrow(my_data),i)
train = my_data[-rand, ]
test = my_data[rand, ]

model6 <- lm(SalePrice ~ Gr.Liv.Area + I(Gr.Liv.Area^2) + I(Gr.Liv.Area^3) + I(Gr.Liv.Area^4) + I(Gr.Liv.Area^5)+I(Gr.Liv.Area^6), train)
model6

#PLOTTING THE MODEL OVER THE DATA
plot(train$Gr.Liv.Area ,train$SalePrice, pch=19, cex=0.5)
lines(sort(train$Gr.Liv.Area), fitted(model6)[order(train$Gr.Liv.Area)], col='magenta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
train_error_present = sqrt(mean(model6$residuals^2))
pred = predict(model6, data=test)

test_error_present = sqrt(mean(pred-test$SalePrice)^2)



train_errors <- append(train_errors,train_error_present)
test_errors  <- append(test_errors,test_error_present)


}
my_table2 <- data.frame(matrix(ncol = 3, nrow=0))
colnames(my_table2) <- c('Sample_size', 'Train_Error', 'Test_Error')

for (i in seq(1,8)) {
  my_table2[i,] = c(sample_size[i], train_errors[i], test_errors[i])
}

View(my_table2)
#Fluctuations of test error for various sample sizes
plot(my_table2$Sample_size,my_table2$Test_Error,type='l',xlab = "Samplesize",ylab = "testrmse",col="red",main = "Fluctuation of test Rmse for various Sample sizes")



 








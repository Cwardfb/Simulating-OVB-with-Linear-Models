#load packages
library(MASS)
library(stats)

#set # of obeservations per sample and number of simulations
obs <- 50
sims <- 1000
#unique observations
total <- obs*sims
# make covariance matrices for values of p
p0.2 <- rbind(c(1,0.2),c(0.2,1))
p0.5 <- rbind(c(1,0.5),c(0.5,1))
p0.8 <- rbind(c(1,0.8),c(0.8,1))
#make vector of means
zeromeans <-c(0,0)
#draw error terms from normal distribution with mean 0 and var of 1,5,10
u_1 <- matrix(rnorm(total,0,1), nrow = obs, ncol = sims)
u_5 <- matrix(rnorm(total,0,sqrt(5)), nrow = obs, ncol = sims)
u_10 <- matrix(rnorm(total,0,sqrt(10)), nrow = obs, ncol = sims)

#generate values for x1 and x2
x1_0.2 <- matrix(,obs,sims)
x2_0.2 <- matrix(,obs,sims)

for (i in 1:sims) {
  X0.2 <- mvrnorm(obs,zeromeans,p0.2)
    x1_0.2[,i]<-X0.2[,1]
    x2_0.2[,i]<-X0.2[,2]
}

x1_0.5 <- matrix(,obs,sims)
x2_0.5 <- matrix(,obs,sims)

for (i in 1:sims) {
  X0.5 <- mvrnorm(obs,zeromeans,p0.5)
  x1_0.5[,i]<-X0.5[,1]
  x2_0.5[,i]<-X0.5[,2]
}

x1_0.8 <- matrix(,obs,sims)
x2_0.8 <- matrix(,obs,sims)

for (i in 1:sims) {
  X0.8 <- mvrnorm(obs,zeromeans,p0.8)
  x1_0.8[,i]<-X0.8[,1]
  x2_0.8[,i]<-X0.8[,2]
}


# generate y's for simulations 
y_0.2_1 <- 1+x1_0.2+x2_0.2+u_1
y_0.2_5 <- 1+x1_0.2+x2_0.2+u_5
y_0.2_10 <- 1+x1_0.2+x2_0.2+u_10

y_0.5_1 <- 1+x1_0.5+x2_0.5+u_1
y_0.5_5 <- 1+x1_0.5+x2_0.5+u_5
y_0.5_10 <- 1+x1_0.5+x2_0.5+u_10

y_0.8_1 <- 1+x1_0.8+x2_0.8+u_1
y_0.8_5 <- 1+x1_0.8+x2_0.8+u_5
y_0.8_10 <- 1+x1_0.8+x2_0.8+u_10

# generate matrices to store results
resultslabel <- c("x1 Ols beta 1","x2 Ols beta 2","tscore x2 beta" ,"biased beta for X1", "Procedure 3 beta 1","Procedure 3 beta 2" , "Type of Ols from procedure 3"
                  ,"Rsquared procedure 1", "Rsquared procedure 2", "Rsquared Procedure 3"
                  ,"B1 Tscore procedure 1", "B1 Tscore procedure 2", "B1 tscore procedure 3")

results_sim1 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim1)<- resultslabel

results_sim2 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim2)<- resultslabel

results_sim3 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim3)<- resultslabel

results_sim4 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim4)<- resultslabel

results_sim5 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim5)<- resultslabel

results_sim6 <- matrix(,nrow = sims , ncol = 13)
colnames(results_sim6)<- resultslabel

results_sim7 <- matrix(,nrow = sims, ncol = 13)
colnames(results_sim7)<- resultslabel

results_sim8 <- matrix(,nrow = sims, ncol = 13)
colnames(results_sim8) <- resultslabel

results_sim9 <-matrix(,nrow = sims, ncol = 13)
colnames(results_sim9) <- resultslabel


#Run First Simulation

for (i in 1:sims) {
  a = lm(y_0.2_1[,i]~x1_0.2[,i]+x2_0.2[,i])
  results_sim1[i,1]<-a$coefficients[2]
  results_sim1[i,2]<-a$coefficients[3]
  results_sim1[i,3] <- summary(a)$coefficients[3,3]
  results_sim1[i,8] <- summary(a)$r.squared
  results_sim1[i,11]<- summary(a)$coefficients[2,3]
  
  b = lm(y_0.2_1[,i]~x1_0.2[,i])
  results_sim1[i,4]<- b$coefficients[2]
  results_sim1[i,9]<- summary(b)$r.squared
  results_sim1[i,12]<- summary(b)$coefficients[2,3]
}

tscores1 <- as.numeric(results_sim1[,3])

for (i in 1:sims) {
  ifelse(abs(tscores1[i])>2,
         results_sim1[i,5]<-results_sim1[i,1] ,
         results_sim1[i,5]<-results_sim1[i,4])
  ifelse(abs(tscores1[i])>2,
         results_sim1[i,10]<-results_sim1[i,8],
         results_sim1[i,10]<-results_sim1[i,9])
  ifelse(abs(tscores1[i])>2,
         results_sim1[i,13]<-results_sim1[i,11],
         results_sim1[i,13]<-results_sim1[i,12])
  
  
  
  ifelse(abs(tscores1[i])>2,
         results_sim1[i,7]<- "OLS" ,
         results_sim1[i,7]<- "biased estimate")
  ifelse(abs(tscores1[i])>2,
         results_sim1[i,6] <- results_sim1[i,2],
         results_sim1[i,6] <- 0)
  
}



#graph results from first simulation
d_sim1_OLS_b1 <- density(as.numeric(results_sim1[,1]))
d_sim1_Biased_b1 <- density(as.numeric(results_sim1[,4]))
d_sim1_p3_b1 <- density(as.numeric(results_sim1[,5]))
plot(d_sim1_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 1:Density of Beta 1 (p=0.2 var=1)" , xlab="Beta")
lines(d_sim1_Biased_b1, col="red",lty=2)
lines(d_sim1_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim1_OLS_b2 <- density(as.numeric(results_sim1[,2]))
d_sim1_p3_b2 <- density(as.numeric(results_sim1[,6]))
plot(d_sim1_OLS_b2,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 1:Density of Beta 2 (p=0.2, var=1)" , xlab="Beta")
lines(d_sim1_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run Second Simulation

for (i in 1:sims) {
  a1 = lm(y_0.2_5[,i]~x1_0.2[,i]+x2_0.2[,i])
  results_sim2[i,1]<-a1$coefficients[2]
  results_sim2[i,2]<-a1$coefficients[3]
  results_sim2[i,3] <- summary(a1)$coefficients[3,3]
  results_sim2[i,8] <- summary(a1)$r.squared
  results_sim2[i,11]<- summary(a1)$coefficients[2,3]
  
  b1 = lm(y_0.2_5[,i]~x1_0.2[,i])
  results_sim2[i,4]<- b1$coefficients[2]
  results_sim2[i,12]<- summary(b1)$coefficients[2,3]
  results_sim2[i,9]<- summary(b1)$r.squared}

tscores2 <- as.numeric(results_sim2[,3])

for (i in 1:sims) {
  ifelse(abs(tscores2[i])>2,
         results_sim2[i,5]<-results_sim2[i,1],
         results_sim2[i,5]<-results_sim2[i,4])
  ifelse(abs(tscores2[i])>2,
         results_sim2[i,10]<-results_sim2[i,8],
         results_sim2[i,10]<-results_sim2[i,9])
  ifelse(abs(tscores2[i])>2,
         results_sim2[i,13]<-results_sim2[i,11],
         results_sim2[i,13]<-results_sim2[i,12])
  
  ifelse(abs(tscores2[i])>2,
         results_sim2[i,7]<- "OLS" ,
         results_sim2[i,7]<- "biased estimate")
  ifelse(abs(tscores2[i])>2,
         results_sim2[i,6] <- results_sim2[i,2],
         results_sim2[i,6] <- 0)
  
}

#graph results from second simulation
d_sim2_OLS_b1 <- density(as.numeric(results_sim2[,1]))
d_sim2_Biased_b1 <- density(as.numeric(results_sim2[,4]))
d_sim2_p3_b1 <- density(as.numeric(results_sim2[,5]))
plot(d_sim2_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 2:Density of Beta 1 (p=0.2 , var=5)" , xlab="Beta")
lines(d_sim2_Biased_b1, col="red",lty=2)
lines(d_sim2_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim2_OLS_b2 <- density(as.numeric(results_sim2[,2]))
d_sim2_p3_b2 <- density(as.numeric(results_sim2[,6]))
plot(d_sim2_OLS_b2,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 2:Density of Beta 2 (p=0.2 , var=5)" , xlab="Beta")
lines(d_sim2_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run Third Simulation

for (i in 1:sims) {
  a2 = lm(y_0.2_10[,i]~x1_0.2[,i]+x2_0.2[,i])
  results_sim3[i,1]<-a2$coefficients[2]
  results_sim3[i,2]<-a2$coefficients[3]
  results_sim3[i,3] <- summary(a2)$coefficients[3,3]
  results_sim3[i,8] <- summary(a2)$r.squared
  results_sim3[i,11]<- summary(a2)$coefficients[2,3]
  
  b2 = lm(y_0.2_10[,i]~x1_0.2[,i])
  results_sim3[i,4]<- b2$coefficients[2]
  results_sim3[i,9]<- summary(b2)$r.squared
  results_sim3[i,12]<- summary(b2)$coefficients[2,3]}

tscores3 <- as.numeric(results_sim3[,3])

for (i in 1:sims) {
  ifelse(abs(tscores3[i])>2,
         results_sim3[i,5]<-results_sim3[i,1],
         results_sim3[i,5]<-results_sim3[i,4])
  ifelse(abs(tscores3[i])>2,
         results_sim3[i,10]<-results_sim3[i,8],
         results_sim3[i,10]<-results_sim3[i,9])
  ifelse(abs(tscores3[i])>2,
         results_sim3[i,13]<-results_sim3[i,11],
         results_sim3[i,13]<-results_sim3[i,12])
  
  ifelse(abs(tscores3[i])>2,
         results_sim3[i,7]<- "OLS" ,
         results_sim3[i,7]<- "biased estimate")
  ifelse(abs(tscores3[i])>2,
         results_sim3[i,6] <- results_sim3[i,2],
         results_sim3[i,6] <- 0)
  
}

#graph results from third simulation
d_sim3_OLS_b1 <- density(as.numeric(results_sim3[,1]))
d_sim3_Biased_b1 <- density(as.numeric(results_sim3[,4]))
d_sim3_p3_b1 <- density(as.numeric(results_sim3[,5]))
plot(d_sim3_OLS_b1,ylim= c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 3:Density of Beta 1 (p=0.2 ,var=10)" , xlab="Beta")
lines(d_sim3_Biased_b1, col="red",lty=2)
lines(d_sim3_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim3_OLS_b2 <- density(as.numeric(results_sim3[,2]))
d_sim3_p3_b2 <- density(as.numeric(results_sim3[,6]))
plot(d_sim3_OLS_b2,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 3:Density of Beta 2 (p=0.2 ,var=10)" , xlab="Beta")
lines(d_sim3_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run fourth simulation


for (i in 1:sims) {
  a3 = lm(y_0.5_1[,i]~x1_0.5[,i]+x2_0.5[,i])
  results_sim4[i,1]<-a3$coefficients[2]
  results_sim4[i,2]<-a3$coefficients[3]
  results_sim4[i,3] <- summary(a3)$coefficients[3,3]
  results_sim4[i,8] <- summary(a3)$r.squared
  results_sim4[i,11]<- summary(a3)$coefficients[2,3]
  
  b3 = lm(y_0.5_1[,i]~x1_0.5[,i])
  results_sim4[i,4]<- b3$coefficients[2]
  results_sim4[i,9]<- summary(b3)$r.squared
  results_sim4[i,12]<- summary(b3)$coefficients[2,3]
  }

tscores4 <- as.numeric(results_sim4[,3])

for (i in 1:sims) {
  ifelse(abs(tscores4[i])>2,
         results_sim4[i,5]<-results_sim4[i,1],
         results_sim4[i,5]<-results_sim4[i,4])
  ifelse(abs(tscores4[i])>2,
         results_sim4[i,10]<-results_sim4[i,8],
         results_sim4[i,10]<-results_sim4[i,9])
  ifelse(abs(tscores4[i])>2,
         results_sim4[i,13]<-results_sim4[i,11],
         results_sim4[i,13]<-results_sim4[i,12])
  
  ifelse(abs(tscores4[i])>2,
         results_sim4[i,7]<- "OLS" ,
         results_sim4[i,7]<- "biased estimate")
  ifelse(abs(tscores4[i])>2,
         results_sim4[i,6] <- results_sim4[i,2],
         results_sim4[i,6] <- 0)
  
}

#graph results from fourth simulation
d_sim4_OLS_b1 <- density(as.numeric(results_sim4[,1]))
d_sim4_Biased_b1 <- density(as.numeric(results_sim4[,4]))
d_sim4_p3_b1 <- density(as.numeric(results_sim4[,5]))
plot(d_sim4_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 4:Density of Beta 1 (p=0.5, var=1)" , xlab="Beta")
lines(d_sim4_Biased_b1, col="red",lty=2)
lines(d_sim4_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim4_OLS_b2 <- density(as.numeric(results_sim4[,2]))
d_sim4_p3_b2 <- density(as.numeric(results_sim4[,6]))
plot(d_sim4_OLS_b2,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 4:Density of Beta 2 (p=0.5, var=1)" , xlab="Beta")
lines(d_sim4_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run fifth simulation

for (i in 1:sims) {
  a4 = lm(y_0.5_5[,i]~x1_0.5[,i]+x2_0.5[,i])
  results_sim5[i,1]<-a4$coefficients[2]
  results_sim5[i,2]<-a4$coefficients[3]
  results_sim5[i,3] <- summary(a4)$coefficients[3,3]
  results_sim5[i,8] <- summary(a4)$r.squared
  results_sim5[i,11]<- summary(a4)$coefficients[2,3]
  
  b4 = lm(y_0.5_5[,i]~x1_0.5[,i])
  results_sim5[i,4]<- b4$coefficients[2]
  results_sim5[i,9]<- summary(b4)$r.squared
  results_sim5[i,12]<- summary(b4)$coefficients[2,3]}

tscores5 <- as.numeric(results_sim5[,3])

for (i in 1:sims) {
  ifelse(abs(tscores5[i])>2,
         results_sim5[i,5]<-results_sim5[i,1],
         results_sim5[i,5]<-results_sim5[i,4])
  ifelse(abs(tscores5[i])>2,
         results_sim5[i,10]<-results_sim5[i,8],
         results_sim5[i,10]<-results_sim5[i,9])
  ifelse(abs(tscores5[i])>2,
         results_sim5[i,13]<-results_sim5[i,11],
         results_sim5[i,13]<-results_sim5[i,12])
  
  ifelse(abs(tscores5[i])>2,
         results_sim5[i,7]<- "OLS" ,
         results_sim5[i,7]<- "biased estimate")
  ifelse(abs(tscores5[i])>2,
         results_sim5[i,6] <- results_sim5[i,2],
         results_sim5[i,6] <- 0)
  
}

#graph results from fifth simulation
d_sim5_OLS_b1 <- density(as.numeric(results_sim5[,1]))
d_sim5_Biased_b1 <- density(as.numeric(results_sim5[,4]))
d_sim5_p3_b1 <- density(as.numeric(results_sim5[,5]))
plot(d_sim5_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 5:Density of Beta 1 (p=0.5, var=5)" , xlab="Beta")
lines(d_sim5_Biased_b1, col="red",lty=2)
lines(d_sim5_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim5_OLS_b2 <- density(as.numeric(results_sim5[,2]))
d_sim5_p3_b2 <- density(as.numeric(results_sim5[,6]))
plot(d_sim5_OLS_b2,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 5:Density of Beta 2 (p=0.5, var=5)" , xlab="Beta")
lines(d_sim5_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run sixth simulation

for (i in 1:sims) {
  a5 = lm(y_0.5_10[,i]~x1_0.5[,i]+x2_0.5[,i])
  results_sim6[i,1]<-a5$coefficients[2]
  results_sim6[i,2]<-a5$coefficients[3]
  results_sim6[i,3] <- summary(a5)$coefficients[3,3]
  results_sim6[i,8] <- summary(a5)$r.squared
  results_sim6[i,11]<- summary(a5)$coefficients[2,3]
  
  b5 = lm(y_0.5_10[,i]~x1_0.5[,i])
  results_sim6[i,4]<- b5$coefficients[2]
  results_sim6[i,9]<- summary(b5)$r.squared
  results_sim6[i,12]<- summary(b5)$coefficients[2,3]}

tscores6 <- as.numeric(results_sim6[,3])

for (i in 1:sims) {
  ifelse(abs(tscores6[i])>2,
         results_sim6[i,5]<-results_sim6[i,1],
         results_sim6[i,5]<-results_sim6[i,4])
  ifelse(abs(tscores6[i])>2,
         results_sim6[i,10]<-results_sim6[i,8],
         results_sim6[i,10]<-results_sim6[i,9])
  ifelse(abs(tscores6[i])>2,
         results_sim6[i,13]<-results_sim6[i,11],
         results_sim6[i,13]<-results_sim6[i,12])
  
  ifelse(abs(tscores6[i])>2,
         results_sim6[i,7]<- "OLS" ,
         results_sim6[i,7]<- "biased estimate")
  ifelse(abs(tscores6[i])>2,
         results_sim6[i,6] <- results_sim6[i,2],
         results_sim6[i,6] <- 0)
  
}

#graph results from sixth simulation
d_sim6_OLS_b1 <- density(as.numeric(results_sim6[,1]))
d_sim6_Biased_b1 <- density(as.numeric(results_sim6[,4]))
d_sim6_p3_b1 <- density(as.numeric(results_sim6[,5]))
plot(d_sim6_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 6:Density of Beta 1 (p=0.5, var=10)" , xlab="Beta")
lines(d_sim6_Biased_b1, col="red",lty=2)
lines(d_sim6_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim6_OLS_b2 <- density(as.numeric(results_sim6[,2]))
d_sim6_p3_b2 <- density(as.numeric(results_sim6[,6]))
plot(d_sim6_OLS_b2,ylim= c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 6:Density of Beta 2 (p=0.5, var=10)" , xlab="Beta")
lines(d_sim6_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run seventh simulation

for (i in 1:sims) {
  a6 = lm(y_0.8_1[,i]~x1_0.8[,i]+x2_0.8[,i])
  results_sim7[i,1]<-a6$coefficients[2]
  results_sim7[i,2]<-a6$coefficients[3]
  results_sim7[i,3] <- summary(a6)$coefficients[3,3]
  results_sim7[i,8] <- summary(a6)$r.squared
  results_sim7[i,11]<- summary(a6)$coefficients[2,3]
  
  b6 = lm(y_0.8_1[,i]~x1_0.8[,i])
  results_sim7[i,4]<- b6$coefficients[2]
  results_sim7[i,9]<- summary(b6)$r.squared
  results_sim7[i,12]<- summary(b6)$coefficients[2,3]}

tscores7 <- as.numeric(results_sim7[,3])

for (i in 1:sims) {
  ifelse(abs(tscores7[i])>2,
         results_sim7[i,5]<-results_sim7[i,1],
         results_sim7[i,5]<-results_sim7[i,4])
  ifelse(abs(tscores7[i])>2,
         results_sim7[i,10]<-results_sim7[i,8],
         results_sim7[i,10]<-results_sim7[i,9])
  ifelse(abs(tscores7[i])>2,
         results_sim7[i,13]<-results_sim7[i,11],
         results_sim7[i,13]<-results_sim7[i,12])
  
  ifelse(abs(tscores7[i])>2,
         results_sim7[i,7]<- "OLS" ,
         results_sim7[i,7]<- "biased estimate")
  ifelse(abs(tscores7[i])>2,
         results_sim7[i,6] <- results_sim7[i,2],
         results_sim7[i,6] <- 0)
  
}

#graph results from seventh simulation
d_sim7_OLS_b1 <- density(as.numeric(results_sim7[,1]))
d_sim7_Biased_b1 <- density(as.numeric(results_sim7[,4]))
d_sim7_p3_b1 <- density(as.numeric(results_sim7[,5]))
plot(d_sim7_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 7:Density of Beta 1 (p=0.8, var=1)" , xlab="Beta")
lines(d_sim7_Biased_b1, col="red",lty=2)
lines(d_sim7_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim7_OLS_b2 <- density(as.numeric(results_sim7[,2]))
d_sim7_p3_b2 <- density(as.numeric(results_sim7[,6]))
plot(d_sim7_OLS_b2,ylim= c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 7:Density of Beta 2 (p=0.8, var=1)" , xlab="Beta")
lines(d_sim7_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Run eighth simulation

for (i in 1:sims) {
  a7 = lm(y_0.8_5[,i]~x1_0.8[,i]+x2_0.8[,i])
  results_sim8[i,1]<-a7$coefficients[2]
  results_sim8[i,2]<-a7$coefficients[3]
  results_sim8[i,3] <- summary(a7)$coefficients[3,3]
  results_sim8[i,8] <- summary(a7)$r.squared
  results_sim8[i,11]<- summary(a7)$coefficients[2,3]
  
  b7 = lm(y_0.8_5[,i]~x1_0.8[,i])
  results_sim8[i,4]<- b7$coefficients[2]
  results_sim8[i,9]<- summary(b7)$r.squared
  results_sim8[i,12]<- summary(b7)$coefficients[2,3]}

tscores8 <- as.numeric(results_sim8[,3])

for (i in 1:sims) {
  ifelse(abs(tscores8[i])>2,
         results_sim8[i,5]<-results_sim8[i,1],
         results_sim8[i,5]<-results_sim8[i,4])
  ifelse(abs(tscores8[i])>2,
         results_sim8[i,10]<-results_sim8[i,8],
         results_sim8[i,10]<-results_sim8[i,9])
  ifelse(abs(tscores8[i])>2,
         results_sim8[i,13]<-results_sim8[i,11],
         results_sim8[i,13]<-results_sim8[i,12])
  
  ifelse(abs(tscores8[i])>2,
         results_sim8[i,7]<- "OLS" ,
         results_sim8[i,7]<- "biased estimate")
  ifelse(abs(tscores8[i])>2,
         results_sim8[i,6] <- results_sim8[i,2],
         results_sim8[i,6] <- 0)
  
}

#graph results from eigth simulation
d_sim8_OLS_b1 <- density(as.numeric(results_sim8[,1]))
d_sim8_Biased_b1 <- density(as.numeric(results_sim8[,4]))
d_sim8_p3_b1 <- density(as.numeric(results_sim8[,5]))
plot(d_sim8_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 8:Density of Beta 1 (p=0.8, var=5)" , xlab="Beta")
lines(d_sim8_Biased_b1, col="red",lty=2)
lines(d_sim8_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

d_sim8_OLS_b2 <- density(as.numeric(results_sim8[,2]))
d_sim8_p3_b2 <- density(as.numeric(results_sim8[,6]))
plot(d_sim8_OLS_b2,ylim= c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 8:Density of Beta 2 (p=0.8, var=5)" , xlab="Beta")
lines(d_sim8_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)


#Run ninth simulation

for (i in 1:sims) {
  a8 = lm(y_0.8_10[,i]~x1_0.8[,i]+x2_0.8[,i])
  results_sim9[i,1]<-a8$coefficients[2]
  results_sim9[i,2]<-a8$coefficients[3]
  results_sim9[i,3] <- summary(a8)$coefficients[3,3]
  results_sim9[i,8] <- summary(a8)$r.squared
  results_sim9[i,11]<- summary(a8)$coefficients[2,3]
  
  b8 = lm(y_0.8_10[,i]~x1_0.8[,i])
  results_sim9[i,4]<- b7$coefficients[2]
  results_sim9[i,9]<- summary(b8)$r.squared
  results_sim9[i,12]<- summary(b8)$coefficients[2,3]
  }

tscores9 <- as.numeric(results_sim9[,3])

for (i in 1:sims) {
  ifelse(abs(tscores9[i])>2,
         results_sim9[i,5]<-results_sim9[i,1],
         results_sim9[i,5]<-results_sim9[i,4])
  ifelse(abs(tscores9[i])>2,
         results_sim9[i,10]<-results_sim9[i,8],
         results_sim9[i,10]<-results_sim9[i,9])
  ifelse(abs(tscores1[i])>2,
         results_sim9[i,13]<-results_sim9[i,11],
         results_sim9[i,13]<-results_sim9[i,12])
  
  ifelse(abs(tscores9[i])>2,
         results_sim9[i,7]<- "OLS" ,
         results_sim9[i,7]<- "biased estimate")
  ifelse(abs(tscores9[i])>2,
         results_sim9[i,6] <- results_sim9[i,2],
         results_sim9[i,6] <- 0)
  
}

#graph results from ninth simulation
d_sim9_OLS_b1 <- density(as.numeric(results_sim9[,1]))
d_sim9_Biased_b1 <- density(as.numeric(results_sim9[,4]))
d_sim9_p3_b1 <- density(as.numeric(results_sim9[,5]))
plot(d_sim9_OLS_b1,ylim=c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 9:Density of Beta 1 (p=0.8, var=10)" , xlab="Beta")
lines(d_sim9_Biased_b1, col="red",lty=2)
lines(d_sim8_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Biased Estimate","Procedure 3 Estimate"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)


d_sim9_OLS_b2 <- density(as.numeric(results_sim9[,2]))
d_sim9_p3_b2 <- density(as.numeric(results_sim9[,6]))
plot(d_sim9_OLS_b2,ylim= c(0,3), xlim = c(0,2), col = "blue" ,main = "Simulation 9:Density of Beta 2 (p=0.8, var=10)" , xlab="Beta")
lines(d_sim9_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("Unbiased OLS Estimate", "Procedure 3 Estimate"),
       col=c("blue", "green"), lty=1:2, cex=0.6)

#Generate Density for r2 and tscores for b1 across scenarios var = 1 and var = 10
d_sim1_p1_r2 <- density(as.numeric(results_sim1[,8]))
d_sim1_p2_r2 <- density(as.numeric(results_sim1[,9]))
d_sim1_p3_r2 <- density(as.numeric(results_sim1[,10]))
d_sim2_p1_r2 <- density(as.numeric(results_sim2[,8]))
d_sim2_p2_r2 <- density(as.numeric(results_sim2[,9]))
d_sim2_p3_r2 <- density(as.numeric(results_sim2[,10]))
d_sim3_p1_r2 <- density(as.numeric(results_sim3[,8]))
d_sim3_p2_r2 <- density(as.numeric(results_sim3[,9]))
d_sim3_p3_r2 <- density(as.numeric(results_sim3[,10]))
d_sim7_p1_r2 <- density(as.numeric(results_sim7[,8]))
d_sim7_p2_r2 <- density(as.numeric(results_sim7[,9]))
d_sim7_p3_r2 <- density(as.numeric(results_sim7[,10]))
d_sim8_p1_r2 <- density(as.numeric(results_sim8[,8]))
d_sim8_p2_r2 <- density(as.numeric(results_sim8[,9]))
d_sim8_p3_r2 <- density(as.numeric(results_sim8[,10]))
d_sim9_p1_r2 <- density(as.numeric(results_sim9[,8]))
d_sim9_p2_r2 <- density(as.numeric(results_sim9[,9]))
d_sim9_p3_r2 <- density(as.numeric(results_sim9[,10]))

d_sim1_p1_t <- density(as.numeric(results_sim1[,11]))
d_sim1_p2_t <- density(as.numeric(results_sim1[,12]))
d_sim1_p3_t <- density(as.numeric(results_sim1[,13]))
d_sim2_p1_t <- density(as.numeric(results_sim2[,11]))
d_sim2_p2_t <- density(as.numeric(results_sim2[,12]))
d_sim2_p3_t <- density(as.numeric(results_sim2[,13]))
d_sim3_p1_t <- density(as.numeric(results_sim3[,11]))
d_sim3_p2_t <- density(as.numeric(results_sim3[,12]))
d_sim3_p3_t <- density(as.numeric(results_sim3[,13]))
d_sim4_p1_t <- density(as.numeric(results_sim4[,13]))
d_sim7_p1_t <- density(as.numeric(results_sim7[,11]))
d_sim7_p2_t <- density(as.numeric(results_sim7[,12]))
d_sim7_p3_t <- density(as.numeric(results_sim7[,13]))
d_sim8_p1_t <- density(as.numeric(results_sim8[,11]))
d_sim8_p2_t <- density(as.numeric(results_sim8[,12]))
d_sim8_p3_t <- density(as.numeric(results_sim8[,13]))
d_sim9_p1_t <- density(as.numeric(results_sim9[,11]))
d_sim9_p2_t <- density(as.numeric(results_sim9[,12]))
d_sim9_p3_t <- density(as.numeric(results_sim9[,13]))

beta2_p3_dropped_sim9 <- matrix(,nrow = 1000,ncol = 1)
beta2_p3_kept_sim9 <- matrix(,nrow = 1000,ncol = 1)

for (i in 1:sims) {
  ifelse(abs(tscores9[i])>2 
  , beta2_p3_kept_sim9[i] <- results_sim9[i,2],
  beta2_p3_dropped_sim9[i] <- results_sim9[i,2])}

beta2_p3_dropped_sim9 <- as.numeric(na.omit(beta2_p3_dropped_sim9))
beta2_p3_kept_sim9 <- as.numeric(na.omit(beta2_p3_kept_sim9))
beta2_p3_dropped_sim9 <- density(beta2_p3_dropped_sim9)
beta2_p3_kept_sim9 <- density(beta2_p3_kept_sim9)

beta2_p3_dropped_sim6 <- matrix(,nrow = 1000,ncol = 1)
beta2_p3_kept_sim6 <- matrix(,nrow = 1000, ncol =1)

for (i in 1:sims) {
  ifelse(abs(tscores6[i])>2 
         , beta2_p3_kept_sim6[i] <- results_sim6[i,2],
         beta2_p3_dropped_sim6[i] <- results_sim6[i,2])}

beta2_p3_dropped_sim6 <- as.numeric(na.omit(beta2_p3_dropped_sim6))
beta2_p3_kept_sim6 <- as.numeric(na.omit(beta2_p3_kept_sim6))
beta2_p3_dropped_sim6 <- density(beta2_p3_dropped_sim6)
beta2_p3_kept_sim6 <- density(beta2_p3_kept_sim6)


beta2_p3_dropped_sim4 <- matrix(,nrow = 1000,ncol = 1)
beta2_p3_kept_sim4 <- matrix(,nrow = 1000, ncol =1)

for (i in 1:sims) {
  ifelse(abs(tscores4[i])>2 
         , beta2_p3_kept_sim4[i] <- results_sim4[i,2],
         beta2_p3_dropped_sim4[i] <- results_sim4[i,2])}

beta2_p3_dropped_sim4 <- as.numeric(na.omit(beta2_p3_dropped_sim4))
beta2_p3_kept_sim4 <- as.numeric(na.omit(beta2_p3_kept_sim4))
beta2_p3_dropped_sim4 <- density(beta2_p3_dropped_sim4)
beta2_p3_kept_sim4 <- density(beta2_p3_kept_sim4)

beta2_p3_dropped_sim5 <- matrix(,nrow = 1000,ncol = 1)
beta2_p3_kept_sim5 <- matrix(,nrow = 1000, ncol =1)

for (i in 1:sims) {
  ifelse(abs(tscores5[i])>2 
         , beta2_p3_kept_sim5[i] <- results_sim5[i,2],
         beta2_p3_dropped_sim5[i] <- results_sim5[i,2])}

beta2_p3_dropped_sim5 <- as.numeric(na.omit(beta2_p3_dropped_sim5))
beta2_p3_kept_sim5 <- as.numeric(na.omit(beta2_p3_kept_sim5))
beta2_p3_dropped_sim5 <- density(beta2_p3_dropped_sim5)
beta2_p3_kept_sim5 <- density(beta2_p3_kept_sim5)
  
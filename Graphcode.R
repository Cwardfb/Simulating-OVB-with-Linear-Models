
#beta densitys
plot(d_sim1_OLS_b1,ylim=c(0,4), xlim = c(0,2), col = "blue" ,main = "Procedure 1: Density of b1 with p=0.2,0.5,0.8 and var= 1" , xlab="Beta 1")
lines(d_sim4_OLS_b1, col="red",lty=2)
lines(d_sim7_OLS_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.2 var=1", "b1 p=0.5 var=1","b1 p=0.8 var=1"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim1_OLS_b2,ylim=c(0,4), xlim = c(0,2), col = "blue" ,main = "Procedure 1: Density of b2 with p=0.2,0.5,0.8 and var= 1" , xlab="Beta 2")
lines(d_sim4_OLS_b2, col="red",lty=2)
lines(d_sim7_OLS_b2, col= "green", lty=2)
legend("topleft", legend=c("b2 p=0.2 var=1", "b2 p=0.5 var=1","b2 p=0.8 var=1"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim1_OLS_b1,ylim=c(0,4), xlim = c(0,2), col = "blue" ,main = "Procedure 1: Density of b1 with p=0.2 and var= 1,5,10" , xlab="Beta 1")
lines(d_sim2_OLS_b1, col="red",lty=2)
lines(d_sim3_OLS_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.2 var=1", "b1 p=0.2 var=5","b1 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim1_OLS_b2,ylim=c(0,4), xlim = c(0,2), col = "blue" ,main = "Procedure 1: Density of b2 with p=0.2 and var= 1,5,10" , xlab="Beta 2")
lines(d_sim2_OLS_b2, col="red",lty=2)
lines(d_sim3_OLS_b2, col= "green", lty=2)
legend("topleft", legend=c("b2 p=0.2 var=1", "b2 p=0.2 var=5","b2 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim4_Biased_b1,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 2: Density of b1 with p=0.5 and var=1,5,10" , xlab="Beta 1")
lines(d_sim5_Biased_b1, col="red",lty=2)
lines(d_sim6_Biased_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.5 var=1", "b1 p=0.5 var=5","b1 p=0.5 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim1_Biased_b1,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 2: Density of b1 with p=0.2,0.5,0.8 and var=1" , xlab="Beta 1")
lines(d_sim4_Biased_b1, col="red",lty=2)
lines(d_sim7_Biased_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.2 var=1", "b1 p=0.5 var=1","b1 p=0.8 var=1"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)



plot(d_sim1_p3_b1,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b1 with p=0.2,0.5,0.8 and var=1" , xlab="Beta 1")
lines(d_sim4_p3_b1, col="red",lty=2)
lines(d_sim7_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.2 var=1", "b1 p=0.5 var=1","b1 p=0.8 var=1"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim4_p3_b1,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b1 with p=0.5 and var=1,5,10" , xlab="Beta 1")
lines(d_sim5_p3_b1, col="red",lty=2)
lines(d_sim6_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.5 var=1", "b1 p=0.5 var=5","b1 p=0.5 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim3_p3_b1,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b1 with p=0.2,0.5,0.8 and var=10" , xlab="Beta 1")
lines(d_sim6_p3_b1, col="red",lty=2)
lines(d_sim9_p3_b1, col= "green", lty=2)
legend("topleft", legend=c("b1 p=0.2 var=10", "b1 p=0.5 var=10","b1 p=0.8 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim1_p3_b2,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b2 with p=0.2 and var=1,5,10" , xlab="Beta 2")
lines(d_sim2_p3_b2, col="red",lty=2)
lines(d_sim3_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("b2 p=0.2 var=1", "b2 p=0.2 var=5","b2 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)
plot(d_sim4_p3_b2,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b2 with p=0.5 and var=1,5,10" , xlab="Beta 2")
lines(d_sim5_p3_b2, col="red",lty=2)
lines(d_sim6_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("b2 p=0.2 var=1", "b2 p=0.2 var=5","b2 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim3_p3_b2,ylim=c(0,4), xlim = c(0,4), col = "blue" ,main = "Procedure 3: Density of b2 with p=0.2,0.5,0.8 and var=10" , xlab="Beta 2")
lines(d_sim6_p3_b2, col="red",lty=2)
lines(d_sim9_p3_b2, col= "green", lty=2)
legend("topleft", legend=c("b2 p=0.2 var=10", "b2 p=0.5 var=10","b2 p=0.8 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)
#tscores
plot(d_sim1_p1_t,ylim=c(0,.6), xlim = c(-1.5,15), col = "blue" ,main = "Procedure 1: Density of t with p=0.2,0.8 and var= 1,10" , xlab="tscore")
lines(d_sim7_p1_t, col="red",lty=2)
lines(d_sim3_p1_t, col= "green", lty=2)
lines(d_sim9_p1_t, col="orange",lty=2)
legend("topleft", legend=c("B1_t p=0.2 var=1", "B1_t p=0.8 var=1","B1_t p=0.2 var=10","B1_t p=0.8 var=10"),
       col=c("blue", "red","green","orange"), lty=1:2, cex=0.6)

plot(d_sim1_p2_t,ylim=c(0,.6), xlim = c(-1.5,15), col = "blue" ,main = "Procedure 2: Density of t with p=0.2,0.8 and var= 1,10" , xlab="tscore")
lines(d_sim7_p2_t, col="red",lty=2)
lines(d_sim3_p2_t, col= "green", lty=2)
lines(d_sim9_p2_t, col="orange",lty=2)
legend("topleft", legend=c("B1_t p=0.2 var=1", "B1_t p=0.8 var=1","B1_t p=0.2 var=10","B1_t p=0.8 var=10"),
       col=c("blue", "red","green","orange"), lty=1:2, cex=0.6)

plot(d_sim1_p3_t,ylim=c(0,.6), xlim = c(-1.5,15), col = "blue" ,main = "Procedure 3: Density of t with p=0.2,0.8 and var= 1,10" , xlab="tscore")
lines(d_sim7_p3_t, col="red",lty=2)
lines(d_sim3_p3_t, col= "green", lty=2)
lines(d_sim9_p3_t, col="orange",lty=2)
legend("topleft", legend=c("B1_t p=0.2 var=1", "B1_t p=0.8 var=1","B1_t p=0.2 var=10","B1_t p=0.8 var=10"),
       col=c("blue", "red","green","orange"), lty=1:2, cex=0.6)

#rsquared
#procedure 1
plot(d_sim1_p1_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 1: Density of R2 with p=0.2 and var= 1,5,10" , xlab="R2")
lines(d_sim2_p1_r2, col="red",lty=2)
lines(d_sim3_p1_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.5 var=1", "r2 p=0.5 var=5","r2 p=0.5 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)
plot(d_sim7_p1_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 1: Density of R2 with p=0.8 and var= 1,5,10" , xlab="R2")
lines(d_sim8_p1_r2, col="red",lty=2)
lines(d_sim9_p1_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.8 var=1", "r2 p=0.8 var=5","r2 p=0.8 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

#procedure 2
plot(d_sim1_p2_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 2: Density of R2 with p=0.2 and var= 1,5,10" , xlab="R2")
lines(d_sim2_p2_r2, col="red",lty=2)
lines(d_sim3_p2_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.2 var=1", "r2 p=0.2 var=5","r2 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)


plot(d_sim7_p2_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 2: Density of R2 with p=0.8 and var= 1,5,10" , xlab="R2")
lines(d_sim8_p2_r2, col="red",lty=2)
lines(d_sim9_p2_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.8 var=1", "r2 p=0.8 var=5","r2 p=0.8 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

#procedure 3
plot(d_sim1_p3_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 3: Density of R2 with p=0.2 and var= 1,5,10" , xlab="R2")
lines(d_sim2_p3_r2, col="red",lty=2)
lines(d_sim3_p3_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.2 var=1", "r2 p=0.2 var=5","r2 p=0.2 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(d_sim7_p3_r2,ylim=c(0,10), xlim = c(0,1), col = "blue" ,main = "Procedure 3: Density of R2 with p=0.8 and var= 1,5,10" , xlab="R2")
lines(d_sim8_p3_r2, col="red",lty=2)
lines(d_sim9_p3_r2, col= "green", lty=2)
legend("topleft", legend=c("r2 p=0.8 var=1", "r2 p=0.8 var=5","r2 p=0.8 var=10"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)




plot(beta2_p3_kept_sim4,ylim=c(0,4), xlim = c(0,3), col = "blue" ,main = "Procedure 3: Density of b2 with p = 0.5 and var= 1" , xlab="Beta 2")
lines(beta2_p3_dropped_sim4, col="red",lty=2)
legend("topleft", legend=c("b2 kept", "b2 dropped"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(beta2_p3_kept_sim4,ylim=c(0,4), xlim = c(0,3), col = "blue" ,main = "Procedure 3: Density of b2 with p = 0.5 and var= 5" , xlab="Beta 2")
lines(beta2_p3_dropped_sim5, col="red",lty=2)
legend("topleft", legend=c("b2 kept", "b2 dropped"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)


plot(beta2_p3_kept_sim6,ylim=c(0,4), xlim = c(0,3), col = "blue" ,main = "Procedure 3: Density of b2 with p = 0.5 and var= 10" , xlab="Beta 2")
lines(beta2_p3_dropped_sim6, col="red",lty=2)
legend("topleft", legend=c("b2 kept", "b2 dropped"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)

plot(beta2_p3_kept_sim9,ylim=c(0,4), xlim = c(0,3), col = "blue" ,main = "Procedure 3: Density of b2 with p = 0.8 and var= 10" , xlab="Beta 2")
lines(beta2_p3_dropped_sim9, col="red",lty=2)
legend("topleft", legend=c("b2 kept", "b2 dropped"),
       col=c("blue", "red","green"), lty=1:2, cex=0.6)


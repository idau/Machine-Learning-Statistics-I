#code for coursework 2

x = c(0,3,5,10)
px = c(0.2, 0.1,0.1, 0.6)

#1 

#Numerical estimation of expected value
expectedV <- sum(x*px)
expectedV

#Numerical estimation of variance
variance <- sum((x^2)*px) - sum(x*px)^2
variance

#2 Simulate 500 values from the given distribution 
mysample <- c()	
mysample <- sample(x, size = 500, replace = TRUE, prob = px)
mysample[1:20]              #just to see parts of the sample

#3 Draw histogram of 500 simulated values
par(las = 1, cex.lab = 1.2)  
hist(mysample, main = "500 simulated values plotted")

#4 Generate 500 simulated values for the mean with 4 observations per mean
drawmeans = apply(matrix(sample(x, size = 4 * 500, replace = TRUE, prob = px), 4), 2, mean)
drawmeans                                        #to see the output

#5 Compute variance with n = 4
varianceofmean <- variance/4                                       
varianceofmean

#6 Visualize the results from 4
par(las = 1, cex.lab = 1.2)  
hist(drawmeans, main = "Task 6: 500 simulated means plotted with 4 observations per mean")
var_drawmeans <- var(drawmeans)                  #calculating the variance of the sample
var_theoretical <- variance/4                    #theoretical variance of the sample

#7
drawmeans2 = apply(matrix(sample(x, size = 16 * 500, replace = TRUE, prob = px), 16), 2, mean)
drawmeans2                                      #to see the output

length(drawmeans2)
par(las = 1, cex.lab = 1.2)  
hist(drawmeans2, main = "Task 7: 500 sampled means plotted with 16 observations per mean")
 
var_drawmeans2 <- var(drawmeans2)                  #calculating the variance of the sample
var_theoretical2 <- variance/16                    #theoretical variance of the sample

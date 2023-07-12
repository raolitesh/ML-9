# codes are written in R studio
```r
# installing package
install.packages("neuralnet")
library("neuralnet")
```
```r
# reading the data
startup <- read.csv(file.choose())
attach(startup)
summary(startup)
sum(is.na(startup))
dim(startup)
names(startup)
startup1 <- startup[,-4] # removing the column 4, this is not required
```
```r
# calculating correlation matrix, SD and variance
cor(startup1)
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)
```
```r
# scatter plot
plot(R.D.Spend,Administration, type = "p")
plot(Marketing.Spend,Profit, main = "Marketing Expense vs Profit")
```
```r
# standardize the data
normalise <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

startup_norm <- as.data.frame(lapply(startup1, normalise))
```
```r
# train and test data
split1 <- sort(sample(nrow(startup_norm),nrow(startup_norm)*0.7)) 
train1 <- startup_norm[split1,]
test1 <- startup_norm[-split1,]
```
```r
# building the neural network model
n1 <- neuralnet(formula = Profit~ R.D.Spend + Administration + Marketing.Spend, data = train1)
# plotting the model
plot(n1)
```
```r
# executing the prediction and checking the accuracy
n1_result <- compute(n1, test1[1:3])#here in the test set we will exclude the Profit Values
profit1 <- n1_result$net.result
round(cor(test1$Profit,profit1)*100, digits = 2)
```
```r
# improving the model accuracy by adding extra hidden layers of neurons

# 1 layer with 3 neurons
n2 <- neuralnet(formula = Profit~. , data = train1, hidden = 3)
plot(n2) #Here the error rate is 0.02744
n2_results <- compute(n2, test1[1:3])
profit2 <- n2_results$net.result
round(cor(test1[,4],profit2)*100, digits = 2)

# 2 layers with 5 and 2 neurons respectively
n3 <- neuralnet(formula = Profit~. , data = train1, hidden = c(5,2))
plot(n3)#Here the error rate is 0.025102
n3_results <- compute(n3,test1[1:3])
profit3 <- n3_results$net.result
round(cor(test1[,4], profit3)*100, digits = 2)
```



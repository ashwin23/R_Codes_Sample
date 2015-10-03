#Code to perform Data Imputation, Split data into training and test, 10 fold CV
#Build linear model, Perform and view performance of principal component factors


# read data into R
sampledata <- read.csv("data.csv")

# Next use the function summary to inspect the data
summary(sampledata)

# Data Imputation
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
data_mean <- sapply(sampledata,mean, na.rm=TRUE)

(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data
for(i in 10:41) {
  sampledata[,i]<-sampledata(election_data[,i],data_mean[i-9])
}

summary(sampledata)

# Create two separate data sets from the data in electionData.
library(caTools)

set.seed(123)

split = sample.split(sampledata$testvar, SplitRatio = 0.7)

train = subset(sampledata$testvar, splitfunc == TRUE)

test = subset(sampledata$testvar, split == FALSE)

#Building Linear Model

model <-lm(train$response ~ #Relevant Vars,
             data = train)


summary(model)

summary(model)$coefficients[,4] 

library(boot)

#K-Fold Cross Validation for Current Model

cv.error.10 = rep(0,10)

for (i in 1:10){
  model <-glm(response ~ #relevantvars)
  cv.error.10[i]=cv.glm(train,model,K=10)$delta[1]
}

model_cv_vals <-cv.error.10

#Creat PCA and check factor rotations, plot PC

apply(train,2,mean)

#Create principal component dataset without variables not important for model

model_pr <- train[ -c(1:6#Remove vars not useful for prediction)]

#Calculating Principal Components                      
                      
pr.out = prcomp(train, center = TRUE, scale = TRUE)

# print method
print(pr.out)

#Plot PCA
plot(pr.out, type = "l")

#Summary of PCA
summary(pr.out)

#Factor Rotations
head(pr.out$rotation)




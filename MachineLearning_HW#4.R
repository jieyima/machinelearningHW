#####################
#####################Exercises: 1.22
#####################

#####################
######Question1######
#####################
install.packages("freqparcoord")
library(freqparcoord)
data(mlb)
##############
# Linear Model
##############

xvalpart <- function(data, p){
  n <- nrow(mlb)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs,])
}
#arguments:
# data: full data
# ycol : column number of resp. var .
# predvars : column numbers of predictors
# p: prop. for training set 
# meanabs : see ’ value ’ below
# value: if meanabs is TRUE, the mean absolute prediction error; otherwise, an R list containing pred., real Y

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  #fit model to training data
  trainy <- train[, ycol]
  trainpreds <- train[, predvars]
  # using matrix form in lm() call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  # apply fitted model to validation data; note
  # that %∗% works only on matrices , not data frames
  validpreds <- as.matrix(valid[, predvars])
  predy <- cbind(1, validpreds) %*% coef(lmout)
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

lm_result <- round(xvallm(mlb, 5, c(4,6), 2/3),3)


###########
# KNN Model
###########

# data: full data
# ycol : column number of resp. var.
#k : number of nearest neighbors 
#p: prop. for training set 
#meanabs: see 'value' below
#value: if meanabs is TRUE, the mean absolute prediction error; otherwise , an R list containing pred., real Y


xvalknn <- function(data, ycol, predvars, k, p, meanabs=TRUE){
  # cull out just Y and the Xs
  data <- data[, c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[, -ycol], TRUE)
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list (predy = predy , realy = realy)
}

install.packages("regtools")
library(regtools)
knn_result <- round(xvalknn(mlb, 5, c(4,6), 30, 2/3),3)

print(paste("Linear model result:", lm_result))
print(paste("KNN model result:", knn_result))


#####################
######Question2######
#####################
## include interaction terms for age and gender, and age2 and gender. 
## Run the new model, and find the estimated effect of being female, for a 32-year-old person with a Master’s degree.
data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]

# interaction terms
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
#pe <− as.matrix(pe)

# model
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = pe)
summary(model)

# Predict income for a 32-year-old female with a Master's degree
age <- 32
age2 <- 32^2
wkswrkd <- 52
ms <- 1
phd <- 0
fem <- 1
agefem <- age*fem
age2fem <- age2*fem
input <- data.frame(age, age2, wkswrkd, ms, phd, fem, agefem, age2fem)
predict(model, input, interval = "prediction", level = 0.95)


#####################
######Question3######
#####################
##Use lm() to form a prediction equation for density from the other variables (skipping the first three), 
##and comment on whether use of indirect methods in this way seems feasible.

#install.packages("mfp")
library (mfp)
data(bodyfat)
model_density = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(model_density)

# Indirect measures can be related to population density using regression analysis (linear or non-linear)


#####################
######Question4######
#####################
# Overall mean height of all people is a weighted average of male mean height and female mean height.
# Overall proportion of people taller than 70 inches is a weighted proportion of male taller than 70 inches and female taller than 70 inches.




#####################
#####################Exercises: 2.14
#####################

#####################
######Question5######
#####################
data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
# interaction terms
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
pe$msfem <- pe$ms * pe$fem
# model
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem + msfem, data = pe)
model_summ <- summary(model)
# confident intervals
beta6 <- model_summ$coefficients['fem',]
beta7 <- model_summ$coefficients['msfem',]
t_value <- qt(0.975, nrow(prgeng)-1)
beta6_h <- beta6[1] + t_value*beta6[2]
beta6_l <- beta6[1] - t_value*beta6[2]
beta7_h <- beta7[1] + t_value*beta7[2]
beta7_l <- beta7[1] - t_value*beta7[2]
paste0("Question5: 95% confidence interval for beta6: (",beta6_l,', ', beta6_h,')')
paste0("Question5: 95% confidence interval for beta6 + beta7: (",beta6_l+beta7_l,', ', beta6_h+beta7_h,')')


#####################
######Question6######
#####################
day <- read.csv('day.csv')
day$temp2 <- day$temp^2
day$clearday <- as.integer(day$weathersit == 1)
bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
bike_summ <- summary(bike)
t_value <- qt(0.975, nrow(day)-1)
yr <- bike_summ$coefficients['yr',]
yr_l <- yr[1] - t_value * yr[2]
yr_h <- yr[1] + t_value * yr[2]
paste0("Question6: 95% confidence interval: (",yr_l,', ', yr_h,')')
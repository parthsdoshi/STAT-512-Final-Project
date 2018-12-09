setwd("M:/My Files/Other/Uiversity files/Purdue/Year 3/2018 Fall/STAT 512/Project")
library("car")
library("MASS")

q4 <- read.table(
  "finaltr4.dat",
  header = F,
  as.is = T,
  col.names = c('x', 'response')
)
c = ncol(q4)
r = nrow(q4)

head(q4)

train = q4[1:65,]
train$response = as.numeric(train$response)
test_x = q4[66:nrow(q4),][c("x")]
test_y = read.table("finalprq4.dat", header = F, as.is = T, col.names = c('response'))

tail(train)
head(test_x)
head(test_y)

train$index = 1:nrow(train)
train$logx = log(train$x)
train$log1x = log(train$x+1)
train$sqrtx = sqrt(train$x)

plot(1:nrow(train), train$response)
plot(train$x, train$response)
plot(train$log1x, train$response)
plot(train$logx, train$response)
plot(train$sqrtx, train$response)
plot(sqrt(train$log1x), train$response)
cor(train)

response_shift = abs(min(train$response)) + 10e-5
train$shifted_response = train$response + response_shift
bc = boxcox(shifted_response ~ sqrtx[-10], data=train)
which.max(bc$y)
bc$x[68]

train$y23 = train$shifted_response ** (0.7)
which.min(train$logx)
response_shift = abs(min(train$response[-10])) + 10e-5
train$shifted_response = train$response + response_shift
logx_shift2 = abs(min(train$logx[-10])) + 10e-5
plot(lm(response[-10] ~ sqrt(logx[-10]+logx_shift2), data=train))

plot(response ~ log(x), data=train)
summary(lm(response[-10] ~ sqrt(log1x[-10]), data=train))
#
model = lm(response ~ (log(x)), data=train[c(-1, -10, -56),])
model1 = lm(response ~ (log(x+1)), data=train[-10,])
plot(response ~ log(x), data=train[-10,])
boxplot(log(train[c(-1, -10, -56),1]))

which.max(log(train[c(-1, -10, -56),1]))
abline(model)

pred = predict(model, test_x)
pred1 = predict(model1, test_x)
sum((pred[-17] - test_y[-17,])**2)
sum((pred - test_y)**2)
summary(model)
anova(model)
plot(log(test_x$x[-17]), test_y[-17,])

plot(smooth.spline(pred[-17], test_y[-17,]), col="blue", type="l")
lines(smooth.spline(pred1[-17], test_y[-17,]), col="red")
abline(model)
abline(model1)
abline(0,1)

plot(log(train$x), train$response)
abline(model)

plot((train$x), train$response)
abline(model)
#

simple_model = lm(response ~ x, data=train)
summary(simple_model)
anova(simple_model)

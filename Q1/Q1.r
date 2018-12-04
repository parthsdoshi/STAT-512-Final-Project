
library("car")
library("MASS")

q1 <- read.table(
  "finaltr1.dat",
  header = F,
  as.is = T,
  col.names = c('a', 'b', 'c', 'response')
)
c = ncol(q1)
r = nrow(q1)

head(q1)

train = q1[1:100,]
train$response = as.numeric(train$response)
test_x = q1[101:nrow(q1),][,c("a","b","c")]
test_y = read.table("finalprq1.dat", header = F, as.is = T, col.names = c('response'))

tail(train)
head(test_x)
head(test_y)

plot(train$a, train$response)
plot(train$b, train$response)
plot(train$c, train$response)

simple_model = lm(response ~ a + b + c, data=train)
summary(simple_model)
anova(simple_model)

# we see that response can be negative, we should shift it
plot(train$response, simple_model$residuals)

# right skewed residuals although still normal mostly
hist(simple_model$residuals)
qqnorm(simple_model$residuals)
qqline(simple_model$residuals)

plot(train$a, simple_model$residuals)
plot(train$b, simple_model$residuals)
plot(train$c, simple_model$residuals)

# we add 10^(-5) because we need every value to be positive, not just 0
response_shift = abs(min(train$response)) + 10e-5
train$shifted_response = train$response + response_shift

# looks like lambda \approx 0.4
# aka perform sqrt transformation
boxcox(shifted_response ~ a + b + c, data=train)

# some correlation between a and c
# c holds the most correlation towards our response variable...
cor(train)

model2 = lm(sqrt(shifted_response) ~ a + b + c, data=train)
# model2 = lm(sqrt(train$shifted_response) ~ train$a + train$b + train$c)
summary(model2)
anova(model2)

# much better normality
hist(model2$residuals, breaks=10)

# however, not nearly constant variance still
plot(train$a, model2$residuals)
plot(train$b, model2$residuals)
plot(train$c, model2$residuals)
plot(exp(train$c), model2$residuals)

# we take the prediction values and square them since we did a sqrt transform
# then we add the minimum shift back and subtract our epsilon from before
predict_test = (predict(model2, test_x) ** 2) - response_shift

# now we compare between our initial model and the new model
sum((predict_test - test_y)**2)
sum((predict(simple_model, test_x) - test_y)**2)

data.frame("predicted"=predict_test, "real"=test_y$response)

model3 = lm(sqrt(shifted_response) ~ a + c, data=train)
summary(model3)
anova(model3)

predict_test_model3 = (predict(model3, test_x) ** 2) - response_shift
sum((predict_test_model3 - test_y)**2)

model4 = lm(sqrt(shifted_response) ~ exp(c) + a*c, data=train)
anova(model4)
Anova(model4)

predict_test_model4 = (predict(model4, test_x) ** 2) - response_shift

# SSE
sum((predict_test_model4 - test_y)^2)

# MSE of test
mean(((predict_test_model4 - test_y)^2)$response)
# MSE of model (residuals) against training
anova(model4)["Mean Sq"][5,1]

data.frame("predicted"=predict_test_model4, "real"=test_y$response)

write.table(predict_test_model4, file="PR1_submit.dat", col.names = F, row.names = F)

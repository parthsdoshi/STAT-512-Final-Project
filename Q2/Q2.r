
library("car")
library("MASS")

q2 <- read.table(
  "finaltr2.dat",
  header = F,
  as.is = T,
  col.names = c('a','b','c','d','e','f','g','h','i','j','response')
)
c = ncol(q2)
r = nrow(q2)

head(q2)

train = q2[1:150,]
train$response = as.numeric(train$response)
test_x = q2[151:nrow(q2),][,c('a','b','c','d','e','f','g','h','i','j')]
test_y = read.table("finalprq2.dat", header = F, as.is = T, col.names = c('response'))

tail(train)
head(test_x)
head(test_y)

test_x$index = 151:200

# plot(train$a, train$response)
# plot(train$b, train$response)
# plot(train$c, train$response)
# plot(train$d, train$response)
# plot(train$e, train$response)
# plot(train$f, train$response)
# plot(train$g, train$response)
# plot(train$h, train$response)
# plot(train$i, train$response)
# plot(train$j, train$response)
# plot(1:nrow(train), train$response)

# this shows us that we don't need the first 100 data points at all since it's not indicative of the test data
plot(1:nrow(train), train$response)

train$index = 1:nrow(train)

cor(train)

model2 = lm(response ~ a + index, data=train)
anova(model2)

# pick only where a is 1
# this is fine since our test data only contains a=1
# if we want to know about a=0, we can train 2 different models
train[101:150,]

# model3 = lm(log(response) ~ as.factor(a) + f + index, data=train)
model3 = lm(response ~ b + c + d + e + f + g + h + i + j, data=train[101:150,])
anova(model3)
Anova(model3)
summary(model3)

hist(model3$residuals)
qqnorm(model3$residuals)
qqline(model3$residuals)

cor(train[101:150,])

model4 = lm(response ~ b + e + f + g + i + j + index, data=train[101:150,])
anova(model4)
Anova(model4)
summary(model4)

hist(model4$residuals)
qqnorm(model4$residuals)
qqline(model4$residuals)

model5 = lm(response ~ index, data=train[101:150,])
anova(model5)
Anova(model5)
summary(model5)

hist(model5$residuals)
qqnorm(model5$residuals)
qqline(model5$residuals)

train_subset = train[101:150,]

plot(train_subset$index, model5$residuals)

plot(train_subset$a, train_subset$response)
plot(train_subset$b, train_subset$response)
plot(train_subset$c, train_subset$response)
plot(train_subset$d, train_subset$response)
plot(train_subset$e, train_subset$response)
plot(train_subset$f, train_subset$response)
plot(train_subset$g, train_subset$response)
plot(train_subset$h, train_subset$response)
plot(train_subset$i, train_subset$response)
plot(train_subset$j, train_subset$response)

predict_test_model2 = predict(model2, test_x)
predict_test_model3 = predict(model3, test_x)
predict_test_model4 = predict(model4, test_x)
predict_test_model5 = predict(model5, test_x)

sum((predict_test_model2 - test_y) ** 2)
mean(((predict_test_model2 - test_y) ** 2)$response)

sum((predict_test_model3 - test_y) ** 2)
mean(((predict_test_model3 - test_y) ** 2)$response)

sum((predict_test_model4 - test_y) ** 2)
mean(((predict_test_model4 - test_y) ** 2)$response)

sum((predict_test_model5 - test_y) ** 2)
mean(((predict_test_model5 - test_y) ** 2)$response)

data.frame("predicted"=predict_test_model5, "real"=test_y$response)

write.table(predict_test_model5, file="PR2_submit.dat", col.names = F, row.names = F)

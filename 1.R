library('ISLR')
library('GGally')
library('MASS')

my.seed <- 12345
train.percent <- 0.75
options("ggmatrix.progress.bar" = FALSE)

?Default
head(Default)
str(Default)

# графики разброса
png(filename = 'pic01.png', bg = 'transparent')
ggp <- ggpairs(Default)
print(ggp, progress = FALSE)
dev.off()

# доли наблюдений в столбце default
table(Default$default) / sum(table(Default$default))

set.seed(my.seed)
inTrain <- sample(seq_along(Default$default),
                  nrow(Default)*train.percent)
df <- Default[inTrain, ]
# фактические значения на обучающей выборке
Факт <- df$default

# Логистическая регрессия
model.logit <- glm(default ~ balance + student + income, data = df, family = 'binomial')
summary(model.logit)
# после исключения income
model.logit <- glm(default ~ balance + student, data = df, family = 'binomial')
summary(model.logit)

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.logit <- predict(model.logit, df, type = 'response')
Прогноз <- factor(ifelse(p.logit > 0.5, 2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))
# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])

# специфичность
conf.m[1, 1] / sum(conf.m[1, ])

# верность
sum(diag(conf.m)) / sum(conf.m)

#QDA

model.qda <- qda(default ~ balance + student + income, data = Default[inTrain, ])
model.qda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))
# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])

# специфичность
conf.m[1, 1] / sum(conf.m[1, ])

# верность
sum(diag(conf.m)) / sum(conf.m)

#------------------------------------------------------------------------
# ROC-кривая =========================================================== 

# считаем 1-SPC и TPR для всех вариантов границы отсечения 
x <- NULL # для (1 - SPC) 
y <- NULL # для TPR 
x1 <- NULL 
y1 <- NULL 
# заготовка под матрицу неточностей 
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2)) 
rownames(tbl) <- c('fact.No', 'fact.Yes') 
colnames(tbl) <- c('predict.No', 'predict.Yes') 
# вектор вероятностей для перебора 
p.vector <- seq(0, 1, length = 501) 
# цикл по вероятностям отсечения 
for (p in p.vector){ 
  # прогноз 
  Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > p, 
                           2, 1), 
                    levels = c(1, 2), 
                    labels = c('No', 'Yes')) 
  
  # фрейм со сравнением факта и прогноза 
  df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз) 
  
  # заполняем матрицу неточностей 
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'No', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'Yes', ]) 
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'Yes', ]) 
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'No', ]) 
  
  # считаем характеристики 
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1]) 
  y <- c(y, TPR) 
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2]) 
  x <- c(x, 1 - SPC) 
} 
for (p in p.vector){ 
  # прогноз 
  Прогноз1 <- factor(ifelse(p.logit > p, 
                            2, 1), 
                     levels = c(1, 2), 
                     labels = c('No', 'Yes')) 
  
  # фрейм со сравнением факта и прогноза 
  df.compare <- data.frame(Факт = Факт, Прогноз1 = Прогноз1) 
  
  #заполняем матрицу неточностей 
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'No', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'Yes', ]) 
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'Yes', ]) 
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'No', ])
  
  # считаем характеристики 
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1]) 
  y1 <- c(y1, TPR) 
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2]) 
  x1 <- c(x1, 1 - SPC) 
} 
png(filename = 'pic02.png', bg = 'transparent')
# строим ROC-кривую 
par(mar = c(5, 5, 1, 1)) 
# кривая 
plot(x, y, main = 'Обучающая выборка',
     type = 'l', col = 'blue', lwd = 2, #qda
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1)) 
lines(x1, y1, type = 'l', col = 'red', lwd = 2, #logit
      xlab = '(1 - SPC)', ylab = 'TPR', 
      xlim = c(0, 1), ylim = c(0, 1)) 
# прямая случайного классификатора 
abline(a = 0, b = 1, lty = 3, lwd = 2) 
# точка для вероятности 0.5 
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16) 
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5(qda)', pos = 4) 
# точка для вероятности 0.2 
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16) 
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2(qda)', pos = 4) 

# точка для вероятности 0.5 
points(x1[p.vector == 0.5], y1[p.vector == 0.5], pch = 16) 
text(x1[p.vector == 0.5], y1[p.vector == 0.5], 'p = 0.5(logit)', pos = 4) 
# точка для вероятности 0.2 
points(x1[p.vector == 0.2], y1[p.vector == 0.2], pch = 16) 
text(x1[p.vector == 0.2], y1[p.vector == 0.2], 'p = 0.2(logit)', pos = 4) 
dev.off()

Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.2, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])

# специфичность
conf.m[1, 1] / sum(conf.m[1, ])

# верность
sum(diag(conf.m)) / sum(conf.m)

#--------------------------------------------------------------------------

df <- Default[-inTrain, ]
# фактические значения на обучающей выборке 
Факт <- df$default

# Логистическая регрессия
model.logit <- glm(default ~ balance + student + income, data = df, family = 'binomial')
summary(model.logit)
# исключаем income
model.logit <- glm(default ~ balance + student, data = df, family = 'binomial')
summary(model.logit)

p.logit <- predict(model.logit, df, type = 'response')
Прогноз <- factor(ifelse(p.logit > 0.5, 2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))


# QDA =========================================================================
model.qda <- qda(default ~ balance + student + income, data = df)
model.qda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# ROC-кривая =========================================================== 

# считаем 1-SPC и TPR для всех вариантов границы отсечения 
x <- NULL # для (1 - SPC) 
y <- NULL # для TPR 
x1 <- NULL 
y1 <- NULL 
# заготовка под матрицу неточностей 
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2)) 
rownames(tbl) <- c('fact.No', 'fact.Yes') 
colnames(tbl) <- c('predict.No', 'predict.Yes') 
# вектор вероятностей для перебора 
p.vector <- seq(0, 1, length = 501) 
# цикл по вероятностям отсечения 
for (p in p.vector){ 
  # прогноз 
  Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > p, 
                           2, 1), 
                    levels = c(1, 2), 
                    labels = c('No', 'Yes')) 
  
  # фрейм со сравнением факта и прогноза 
  df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз) 
  
  # заполняем матрицу неточностей 
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'No', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'Yes', ]) 
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'Yes', ]) 
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'No', ]) 
  
  # считаем характеристики 
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1]) 
  y <- c(y, TPR) 
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2]) 
  x <- c(x, 1 - SPC) 
} 
for (p in p.vector){ 
  # прогноз 
  Прогноз1 <- factor(ifelse(p.logit > p, 
                            2, 1), 
                     levels = c(1, 2), 
                     labels = c('No', 'Yes')) 
  
  # фрейм со сравнением факта и прогноза 
  df.compare <- data.frame(Факт = Факт, Прогноз1 = Прогноз1) 
  
  #заполняем матрицу неточностей 
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'No', ]) 
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'Yes', ]) 
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'Yes', ]) 
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'No', ])
  
  # считаем характеристики 
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1]) 
  y1 <- c(y1, TPR) 
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2]) 
  x1 <- c(x1, 1 - SPC) 
} 
png(filename = 'pic03.png', bg = 'transparent')
# строим ROC-кривую 
par(mar = c(5, 5, 1, 1)) 
# кривая 
plot(x, y, main = 'Тестовая выборка',
     type = 'l', col = 'blue', lwd = 2, #qda
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1)) 
lines(x1, y1, type = 'l', col = 'red', lwd = 2, #logit
      xlab = '(1 - SPC)', ylab = 'TPR', 
      xlim = c(0, 1), ylim = c(0, 1)) 
# прямая случайного классификатора 
abline(a = 0, b = 1, lty = 3, lwd = 2) 
# точка для вероятности 0.5 
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16) 
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5(qda)', pos = 4) 
# точка для вероятности 0.2 
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16) 
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2(qda)', pos = 4) 

# точка для вероятности 0.5 
points(x1[p.vector == 0.5], y1[p.vector == 0.5], pch = 16) 
text(x1[p.vector == 0.5], y1[p.vector == 0.5], 'p = 0.5(logit)', pos = 4) 
# точка для вероятности 0.2 
points(x1[p.vector == 0.2], y1[p.vector == 0.2], pch = 16) 
text(x1[p.vector == 0.2], y1[p.vector == 0.2], 'p = 0.2(logit)', pos = 4) 
dev.off()

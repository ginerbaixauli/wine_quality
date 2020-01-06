library(readr)
library(corrplot)
library(Hmisc)

data <- read_csv("C:/Users/Carlos/Desktop/Tipología y ciclo de vida de los datos/PRA2/winequality-red.csv")

colnames(data) <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar",
                    "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
                    "pH", "sulphates", "alcohol", "quality")


# Descripción del dataset
dim(data)
str(data)
summary(data)


# Comprobamos que no hay NA
table(data$fixed_acidity, useNA = "always")
table(data$volatile_acidity, useNA = "always")
table(data$citric_acid, useNA = "always")
table(data$residual_sugar, useNA = "always")
table(data$chlorides, useNA = "always")
table(data$free_sugar_dioxide, useNA = "always")
table(data$total_sulfur_dioxide, useNA = "always")
table(data$density, useNA = "always")
table(data$pH, useNA = "always")
table(data$sulphates, useNA = "always")
table(data$alcohol, useNA = "always")
table(data$quality, useNA = "always")

# Comprobación de outliers
boxplot.stats(data$fixed_acidity)$out
boxplot.stats(data$volatile_acidity)$out
boxplot.stats(data$citric_acid)$out
boxplot.stats(data$residual_sugar)$out
boxplot.stats(data$chlorides)$out
boxplot.stats(data$free_sugar_dioxide)$out
boxplot.stats(data$total_sulfur_dioxide)$out
boxplot.stats(data$density)$out
boxplot.stats(data$pH)$out
boxplot.stats(data$sulphates)$out
boxplot.stats(data$alcohol)$out
boxplot.stats(data$quality)$out

# Análisis de la correlación
M <- cor(data)
corrplot(M, method="circle")

# Selección de variables
data <- data[,c("citric_acid", "residual_sugar", "total_sulfur_dioxide", "sulphates", "alcohol", "quality")]

# Pruebas de normalidad
shapiro.test(data$citric_acid)
shapiro.test(data$residual_sugar)
shapiro.test(data$total_sulfur_dioxide)
shapiro.test(data$sulphates)
shapiro.test(data$alcohol)
shapiro.test(data$quality)

# Pruebas de homogeneidad
data$peralcohol <- ifelse(data$alcohol >= 10, "high", "low")
fligner.test(quality ~ peralcohol, data)

# Test de calidad por grado de alcohol
kruskal.test(quality ~ peralcohol, data)
data$peralcohol <- NULL

# Test de correlación
cor(data)
mcor <- rcorr(as.matrix(data), type="pearson")
View(mcor$P)

# Modelo de regresión lineal
model1 <- lm(quality ~ citric_acid + residual_sugar + total_sulfur_dioxide + sulphates + alcohol, data)
summary(model1)
model2 <- lm(quality ~ citric_acid + total_sulfur_dioxide + sulphates + alcohol, data)
summary(model2)

# Representación gráfica de los resultados
corrplot(M, method="circle")

data$peralcohol <- ifelse(data$alcohol >=10, "high", "low")
boxplot(quality ~ peralcohol, data, main="Calidad por nivel de alcohol")

hist(data$citric_acid, breaks = 40)
hist(data$residual_sugar, breaks = 40)
hist(data$total_sulfur_dioxide, breaks = 40)
hist(data$sulphates, breaks = 40)
hist(data$alcohol, breaks = 40)
hist(data$quality, breaks = 40)
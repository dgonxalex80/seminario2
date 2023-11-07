library(readxl)
data1 <- read_excel("data1.xlsx")
data1$C2E100 <- as.numeric(data1$C2E100)
#-----------------------------------------
boxplot(data1$C1M50 ~ data1$ExpTec)
boxplot(data1$C3M150 ~ data1$ExpTec)
boxplot(data1$C1E40 ~ data1$ExpTec)
boxplot(data1$C2E100 ~ data1$ExpTec)
boxplot(data1$C3E150 ~ data1$ExpTec)
boxplot(data1$C4E300 ~ data1$ExpTec)

# --------------------------------------
resumen = aggregate(C1M50 ~ ExpTec, data = data1,
                    FUN = function(x) c(media = mean(x), varianza = var(x), cv = sd(x)/mean(x)*100))
resumen

data_4= subset(data1, ExpTec ==4)
boxplot(data_4[,8:11], las=1)

posicion <- which(data_4$C4E300 >= 400)
posicion

data_41 = data_4[-10,]
boxplot(data_41[,8:11], las=1)


baseZ = data.frame(
   y =  c(data_41$C1E40, data_41$C2E100, data_41$C3E150, data_41$C4E300),
   x = rep(c(40,100,150,300), each = 42 ))

modelo1 = lm(y ~ x, data = baseZ) 
summary(modelo1)

plot(baseZ$x,baseZ$y)
abline(modelo1, col = "red")




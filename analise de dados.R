dados <- read.csv('vgsales.csv', header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE, na.strings = "NA")

head(dados)
summary(dados)
str(dados)

dados_filtrados <- subset(dados, Year >= 2000 & Year <= 2016)

valor_minimo <- min(dados_filtrados$Global_Sales)

valor_maximo <- max(dados_filtrados$Global_Sales)

media <- mean(dados_filtrados$Global_Sales)

mediana <- median(dados_filtrados$Global_Sales)

moda <- names(sort(-table(dados_filtrados$Global_Sales)))[1]

# Remover valores NA das vendas globais
dados_sem_na <- subset(dados_filtrados, Year != "NA")

summary(dados_sem_na)

str(dados_sem_na)

sum(is.na(dados_sem_na$Year))



library(ggplot2)

#______________________________________________________________________________

# Gráfico de barras comparativo entre Plataforma e Vendas Globais
ggplot(dados, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Plataforma")

ggplot(dados_sem_na, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Plataforma")

#______________________________________________________________________________

# Gráfico de barras comparativo entre Gênero e Vendas Globais
ggplot(dados, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Gênero")

# Gráfico de barras comparativo entre Gênero e Vendas Globais
ggplot(dados_sem_na, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Gênero")

#______________________________________________________________________________

# Gráfico de barras comparativo entre Ano e Vendas Globais
ggplot(dados, aes(x = Year, y = Global_Sales, fill = as.factor(Year))) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Ano")


# Gráfico de barras comparativo entre Editora e Vendas Globais
ggplot(dados, aes(x = Publisher, y = Global_Sales, fill = Publisher)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Editora")


# Gráfico de dispersão comparativo entre Vendas NA e Vendas EU
ggplot(dados, aes(x = NA_Sales, y = EU_Sales, color = Genre)) +
  geom_point() +
  labs(title = "Vendas na América do Norte vs Vendas na Europa")


plot(dados_sem_na$Genre, dados_sem_na$Year)





#______________________________________________________________________________
#Matheus

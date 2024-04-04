dados <- read.csv('vgsales.csv', header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE, na.strings = "NA")

head(dados)
summary(dados)
str(dados)

dados_filtrados <- subset(dados, Year >= 2000 & Year <= 2016)

boxplot(Global_Sales ~ Year, data = dados_filtrados, horizontal = TRUE,
        xlab = "Vendas Globais", ylab = "Ano",
        main = "Distribuição das Vendas Globais por Ano")

ggplot(dados_filtrados, aes(x = as.factor(Year), y = Global_Sales, fill = as.factor(Year))) +
  geom_boxplot(color = "red") +
  labs(x = "Ano", y = "Vendas Globais", title = "Distribuição das Vendas Globais por Ano (2006-2019)") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

boxplot(Global_Sales ~ Year, data = dados_filtrados,horizontal = TRUE,
        xlab = "Vendas Europa", ylab = "Ano",
        main = "Distribuição das Vendas Globais por Ano")

boxplot(EU_Sales ~ Year, data = dados_filtrados,
        xlab = "Vendas Europa", ylab = "Ano",
        main = "Distribuição das Vendas Globais por Ano")

ggplot(dados_filtrados, aes(x = as.factor(Year), fill = Global_Sales)) +
  geom_bar() +
  labs(x = "Ano", y = "Vendas Globais", title = "Distribuição das Vendas Globais por Gênero (2006-2019)") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")

valor_minimo <- min(dados_filtrados$Global_Sales)

valor_maximo <- max(dados_filtrados$Global_Sales)

media <- mean(dados_filtrados$Global_Sales)

mediana <- median(dados_filtrados$Global_Sales)

moda <- names(sort(-table(dados_filtrados$Global_Sales)))[1]

# Remover valores NA das vendas globais
dados_sem_na <- subset(dados_filtrados, Year != "NA")

sum(is.na(dados_sem_na$Year))

summary(dados_sem_na)

str(dados_sem_na)

# Definir o número de breaks (intervals) desejado
num_breaks <- 50


# Calcular os breaks de forma dinâmica
breaks <- seq(0, valor_maximo, length.out = num_breaks)

# Definir o intervalo máximo das vendas globais para visualização
intervalo_maximo <- 5  # em milhões

hist(dados_sem_na$Global_Sales,
     breaks = breaks,
     col = "skyblue",
     main = "Distribuição das Vendas Globais",
     xlab = "Vendas Globais",
     ylab = "Frequência")

hist(dados_filtrados$Global_Sales,
     breaks = breaks,
     col = "skyblue",
     main = "Distribuição das Vendas Globais",
     xlab = "Vendas Globais",
     ylab = "Frequência")

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

julião

plot(dados_sem_na$Genre, dados_sem_na$Year)



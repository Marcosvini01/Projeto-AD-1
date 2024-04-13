install.packages("ggplot2")
install.packages("e1071")

library(e1071)
library(ggplot2)

#______________________________________________________________________________
# Leitura dos dados ___________________________________________________________
vgsales_raw <- read.csv('vgsales.csv', header = TRUE, sep = ",", dec = ".", 
                        stringsAsFactors = TRUE, na.strings = "NA")

#______________________________________________________________________________
# Exibição dos dados brutos ___________________________________________________
head(vgsales_raw)
summary(vgsales_raw)
str(vgsales_raw)
View(vgsales_raw)

#Transformação da var Year [FACTOR] em [Int]
vgsales_raw$Year <- as.integer(as.character(vgsales_raw$Year)) 
str(vgsales_raw)

#______________________________________________________________________________
# Observação de outliers para 'Year' antes de 2000 e depois de 2016 ___________

ggplot(vgsales_raw, aes(x=Genre, y=Year))+geom_boxplot()+
  labs(title="Boxplot de Genre e Year", x="Genre", y="Year")

#______________________________________________________________________________
# Filtro de ano (2000 a 2016) para diminuição de outliers _____________________

vgsales <- subset(vgsales_raw, Year >= 2000 & Year <= 2016)

#______________________________________________________________________________
# Verificação de colunas com dado vazio _______________________________________

sum(is.na(vgsales$Rank))                                  # 0
sum(is.na(vgsales$Name))                                  # 0
sum(is.na(vgsales$Platform))                              # 0
sum(is.na(vgsales$Year))                                  # 271
sum(is.na(vgsales$Genre))                                 # 0
sum(is.na(vgsales$Publisher))                             # 36
sum(is.na(vgsales$NA_Sales))                              # 0
sum(is.na(vgsales$EU_Sales))                              # 0
sum(is.na(vgsales$JP_Sales))                              # 0
sum(is.na(vgsales$Other_Sales))                           # 0
sum(is.na(vgsales$Global_Sales))                          # 0

#______________________________________________________________________________
# Remoção de NA na coluna 'Year' e 'Publisher' ________________________________

vgsales <- subset(vgsales, Year != "NA" & Publisher != "NA")
sum(is.na(vgsales$Year))
sum(is.na(vgsales$Publisher))
#______________________________________________________________________________
# Re-ordenando rank de vendas _________________________________________________

vgsales_size <- nrow(vgsales)
for (idx in 1:vgsales_size){
  vgsales$Rank[idx] <- idx
}

#______________________________________________________________________________
# Visualizando o boxplot dos dados após limpeza _______________________________

ggplot(vgsales, aes(x=Genre, y=Year))+geom_boxplot()+
  labs(title="Boxplot de Genre e Year", x="Genre", y="Year")

#______________________________________________________________________________




#______________________________________________________________________________
# Dados quantitativos _________________________________________________________
## Global Sales: ______________________________________________________________

gs_min <- min(vgsales$Global_Sales)
gs_max <- max(vgsales$Global_Sales)
gs_mean <- mean(vgsales$Global_Sales)
gs_median <- median(vgsales$Global_Sales)
gs_moda <- names(sort(-table(vgsales$Global_Sales)))[1]
gs_amplitude <- gs_max - gs_min
gs_AIQ <- quantile(vgsales$Global_Sales, 0.75) - quantile(vgsales$Global_Sales, 0.25)
gs_var <- var(vgsales$Global_Sales)
gs_dp <- sd(vgsales$Global_Sales)
gs_achatamento <- kurtosis(vgsales$Global_Sales)

## NA Sales: __________________________________________________________________

na_sales_min <- min(vgsales$NA_Sales)
na_sales_max <- max(vgsales$NA_Sales)
na_sales_mean <- mean(vgsales$NA_Sales)
na_sales_median <- median(vgsales$NA_Sales)
na_sales_moda <- names(sort(-table(vgsales$NA_Sales)))[1]
na_sales_amplitude <- na_sales_max - na_sales_min
na_sales_AIQ <- quantile(vgsales$NA_Sales, 0.75) - quantile(vgsales$NA_Sales, 0.25)
na_sales_var <- var(vgsales$NA_Sales)
na_sales_dp <- sd(vgsales$NA_Sales)
na_sales_achatamento <- kurtosis(vgsales$NA_Sales)

## EU Sales: __________________________________________________________________

eu_sales_min <- min(vgsales$EU_Sales)
eu_sales_max <- max(vgsales$EU_Sales)
eu_sales_mean <- mean(vgsales$EU_Sales)
eu_sales_median <- median(vgsales$EU_Sales)
eu_sales_moda <- names(sort(-table(vgsales$EU_Sales)))[1]
eu_sales_amplitude <- eu_sales_max - eu_sales_min
eu_sales_AIQ <- quantile(vgsales$EU_Sales, 0.75) - quantile(vgsales$EU_Sales, 0.25)
eu_sales_var <- var(vgsales$EU_Sales)
eu_sales_dp <- sd(vgsales$EU_Sales)
eu_sales_achatamento <- kurtosis(vgsales$EU_Sales)

## JP Sales: __________________________________________________________________

jp_sales_min <- min(vgsales$JP_Sales)
jp_sales_max <- max(vgsales$JP_Sales)
jp_sales_mean <- mean(vgsales$JP_Sales)
jp_sales_median <- median(vgsales$JP_Sales)
jp_sales_moda <- names(sort(-table(vgsales$JP_Sales)))[1]
jp_sales_amplitude <- jp_sales_max - jp_sales_min
jp_sales_AIQ <- quantile(vgsales$JP_Sales, 0.75) - quantile(vgsales$JP_Sales, 0.25)
jp_sales_var <- var(vgsales$JP_Sales)
jp_sales_dp <- sd(vgsales$JP_Sales)
jp_sales_achatamento <- kurtosis(vgsales$JP_Sales)

## Other Sales: _______________________________________________________________

other_sales_min <- min(vgsales$Other_Sales)
other_sales_max <- max(vgsales$Other_Sales)
other_sales_mean <- mean(vgsales$Other_Sales)
other_sales_median <- median(vgsales$Other_Sales)
other_sales_moda <- names(sort(-table(vgsales$Other_Sales)))[1]
other_sales_amplitude <- other_sales_max - other_sales_min
other_sales_AIQ <- quantile(vgsales$Other_Sales, 0.75) - quantile(vgsales$Other_Sales, 0.25)
other_sales_var <- var(vgsales$Other_Sales)
other_sales_dp <- sd(vgsales$Other_Sales)
other_sales_achatamento <- kurtosis(vgsales$Other_Sales)

## Year: ______________________________________________________________________

year_min <- min(vgsales$Year)
year_max <- max(vgsales$Year)
year_mean <- mean(vgsales$Year)
year_median <- median(vgsales$Year)
year_moda <- names(sort(-table(vgsales$Year)))[1]
year_amplitude <- year_max - year_min
year_AIQ <- quantile(vgsales$Year, 0.75) - quantile(vgsales$Year, 0.25)
year_var <- var(vgsales$Year)
year_dp <- sd(vgsales$Year)
year_achatamento <- kurtosis(vgsales$Year)

## Tabela com todas as analises dos dados quantitativos: ______________________
options(scipen = 999)
quantit_stats <- data.frame(Mínimo = c(year_min, gs_min, na_sales_min, eu_sales_min, jp_sales_min, other_sales_min),
                            Máximo = c(year_max, gs_max, na_sales_max, eu_sales_max, jp_sales_max, other_sales_max),
                            Média = c(year_mean, gs_mean, na_sales_mean, eu_sales_mean, jp_sales_mean, other_sales_mean),
                            Mediana = c(year_median, gs_median, na_sales_median, eu_sales_median, jp_sales_median, other_sales_median),
                            Moda = c(year_moda, gs_moda, na_sales_moda, eu_sales_moda, jp_sales_moda, other_sales_moda),
                            Amplitude = c(year_amplitude, gs_amplitude, na_sales_amplitude, eu_sales_amplitude, jp_sales_amplitude, other_sales_amplitude),
                            AIQ = c(year_AIQ, gs_AIQ, na_sales_AIQ, eu_sales_AIQ, jp_sales_AIQ, other_sales_AIQ),
                            Variância = c(year_var, gs_var, na_sales_var, eu_sales_var, jp_sales_var, other_sales_var),
                            Desvio_Padrão = c(year_dp, gs_dp, na_sales_dp, eu_sales_dp, jp_sales_dp, other_sales_dp),
                            Achatamento = c(year_achatamento, gs_achatamento, na_sales_achatamento, eu_sales_achatamento, jp_sales_achatamento, other_sales_achatamento))


rownames(quantit_stats) <- c("Ano", "Global_Sales", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
View(quantit_stats)
#______________________________________________________________________________
#Visualização de dados quantitativos pelo Boxplot
#Boxplot-Year
boxplot(vgsales$Year,horizontal=TRUE)
grid()

#Boxplot-Vendas Regionais e Globais
boxplot(vgsales$NA_Sales, vgsales$EU_Sales, vgsales$JP_Sales, vgsales$Other_Sales, vgsales$Global_Sales, xaxt = "n", outline=FALSE)
axis(1, at=1:5, labels=c("NA", "EU", "JP", "Other", "Global"))
grid()
#______________________________________________________________________________
#Gráfico de Linhas/Séries Temporais - Vendas Globais por Ano
ggplot(vgsales, aes(x = Year, y = Global_Sales)) + geom_line() + labs(x = "Year", y = "Global_Sales", title = "Vendas Globais por Ano")

#Gráfico de dispersão comparativo entre regiões e global
plot(vgsales$NA_Sales,vgsales$Global_Sales,xlab = "NA_Sales", ylab = "Global_Sales", main="Vendas NA e Global", col="blue")
plot(vgsales$EU_Sales,vgsales$Global_Sales,xlab = "EU_Sales", ylab = "Global_Sales", main="Vendas EU e Global", col="purple")
plot(vgsales$JP_Sales,vgsales$Global_Sales,xlab = "JP_Sales", ylab = "Global_Sales", main="Vendas JP e Global", col="red")
plot(vgsales$Other_Sales,vgsales$Global_Sales,xlab = "Other_Sales", ylab = "Global_Sales", main="Vendas Other e Global", col="orange")

#Cálculo do coeficiente de Pearson
cor(vgsales$NA_Sales,vgsales$Global_Sales)
cor(vgsales$EU_Sales,vgsales$Global_Sales)
cor(vgsales$JP_Sales,vgsales$Global_Sales)
cor(vgsales$Other_Sales,vgsales$Global_Sales)

#______________________________________________________________________________
# Dados qualitativos___________________________________________________________
## Gráfico de pizza____________________________________________________________

genre_fa <-table(vgsales$Genre)
genre_fpct <- prop.table(genre_fa) * 100
pie(genre_fpct, main='Frequência relativa percentual')

#______________________________________________________________________________
# Gráfico de Global_Sales x Platform:

sales_by_platform <- aggregate(Global_Sales ~ Platform, data = vgsales, sum)
ggplot(sales_by_platform, aes(x = Platform, y = Global_Sales,label = Global_Sales)) +
  geom_bar(stat = "identity", aes(fill = Platform), color="black") +
  geom_text(vjust=2, size=4) + 
  labs(x="Plataforma", y="Vendas Globais (em milhões de unidades)", title="Vendas Globais de video-jogos por Plataforma") + 
  theme_bw()+guides(fill = FALSE)

#______________________________________________________________________________
# Gráfico de vendas totais por Gênero:
ggplot(vgsales, aes(x = reorder(Genre, - Global_Sales), y = Global_Sales)) +
  geom_bar(stat = "identity", color="green", fill="white") +
  labs(x="Gênero", y="Vendas totais", title="Vendas totais por Gênero") + 
  theme_bw()

#______________________________________________________________________________
# Gráfico de Global_Sales x Publisher:
# Obtendo apenas para o Top 10 publicadoras pois ao todo temos mais de 400 publishers
# o que para plotar ficaria com uma visualização ruim

sales_by_publisher <- aggregate(Global_Sales ~ Publisher, data = vgsales, sum)
sales_by_publisher <- sales_by_publisher[order(-sales_by_publisher$Global_Sales), ]
sales_by_publisher_top_20_publishers <- head(sales_by_publisher, 10)
ggplot(sales_by_publisher_top_20_publishers, aes(x = Publisher, y = Global_Sales,label = Global_Sales)) +
  geom_bar(stat = "identity", color="green", fill="white") +
  geom_text(vjust=2, size=4) + 
  labs(x="Publicadora", y="Vendas Globais (em milhões de unidades)", title="Vendas Globais de video-jogos por Publicadora") + 
  theme_bw()

# Gráfico de Top 10 Maiores 1000 Ranks por Publisher:

vgsales_Rank1000 <- vgsales[vgsales$Rank <= 1000, ]
publishers_count_Rank1000 <- table(vgsales_Rank1000$Publisher)
publishers_count_Rank1000 <- sort(publishers_count_Rank1000, decreasing = TRUE)
top_publishers_Rank1000 <- names(publishers_count_Rank1000)[1:10]
df_Rank1000 <- data.frame(Publisher = names(publishers_count_Rank1000), Count = as.numeric(publishers_count_Rank1000))
df_Rank1000 <- head(df_Rank1000, 10)
View(df_Rank1000)

ggplot(df_Rank1000, aes(x = Publisher, y = Count,label = Count)) +
  geom_bar(stat = "identity", color="green", fill="white") +
  geom_text(vjust=2, size=4) + 
  labs(x="Publicadora", y="Qntde. de jogos dentro do Rank Top 1000", title="Qntde. de jogos dentro do Rank Top 1000 por Publicadora") + 
  theme_bw()

#______________________________________________________________________________

#gráficos de associações qualitativa

#Gráfico de Barras para a Frequência de Gêneros de Jogos
genre_fa <- table(vgsales$Genre)
barplot(genre_fa, main='Frequência de Gêneros de Jogos', xlab='Gênero', ylab='Frequência', col='blue')

#Gráfico de Barras para as Vendas Globais por Plataforma
sales_by_platform <- aggregate(Global_Sales ~ Platform, data = vgsales, sum)
barplot(sales_by_platform$Global_Sales, names.arg = sales_by_platform$Platform, 
        main='Vendas Globais por Plataforma', xlab='Plataforma', ylab='Vendas Globais', col='green')

#Gráfico de Barras para as Vendas Totais por Gênero
total_sales_by_genre <- aggregate(Global_Sales ~ Genre, data = vgsales, sum)
barplot(total_sales_by_genre$Global_Sales, names.arg = total_sales_by_genre$Genre, 
        main='Vendas Totais por Gênero', xlab='Gênero', ylab='Vendas Totais', col='purple')

#Gráfico de Barras para as Vendas Globais por Ano
library(ggplot2)
ggplot(vgsales, aes(x = Year, y = Global_Sales)) +
  geom_bar(stat = "summary", fun = "sum", fill = "blue") +
  labs(x = "Ano", y = "Vendas Globais", title = "Vendas Globais por Ano") +
  theme_minimal()


#Gráficos de Associação Quantitativa
#Gráfico de Dispersão para Comparar Vendas na América do Norte com Vendas Globais

plot(vgsales$NA_Sales, vgsales$Global_Sales, xlab='Vendas na América do Norte', ylab='Vendas Globais', 
     main='Vendas na América do Norte vs. Vendas Globais', col='blue')

#Gráfico de Linhas para Visualizar as Vendas Globais ao Longo dos Anos
ggplot(vgsales, aes(x = Year, y = Global_Sales)) + geom_line() + 
  labs(x = 'Ano', y = 'Vendas Globais', title = 'Vendas Globais ao Longo dos Anos')


#Gráficos de Associação Quantitativa

#Este gráfico de dispersão compara as vendas globais de jogos com as 
#vendas em outras regiões (América do Norte, Europa, Japão e outras regiões). 
#Cada ponto representa um jogo, onde as coordenadas representam as vendas globais 
#e as vendas nas outras regiões. Isso ajuda a identificar padrões de vendas em 
#diferentes partes do mundo.

#Gráfico de Dispersão para Comparar Vendas na Europa com Vendas Globais
plot(vgsales$EU_Sales, vgsales$Global_Sales, xlab='Vendas na Europa', ylab='Vendas Globais', 
     main='Vendas na Europa vs. Vendas Globais', col='purple')

#Gráfico de Dispersão para Comparar Vendas no Japão com Vendas Globais
plot(vgsales$JP_Sales, vgsales$Global_Sales, xlab='Vendas no Japão', ylab='Vendas Globais', 
     main='Vendas no Japão vs. Vendas Globais', col='red')


plot(vgsales$NA_Sales, vgsales$Global_Sales, xlab='Vendas na América do Norte', ylab='Vendas Globais', 
     main='Vendas na América do Norte vs. Vendas Globais', col='blue')
#Gráfico de Dispersão para Comparar Vendas Globais com Vendas em outras Regiões
points(vgsales$EU_Sales, vgsales$Global_Sales, col='red')
points(vgsales$JP_Sales, vgsales$Global_Sales, col='green')
points(vgsales$Other_Sales, vgsales$Global_Sales, col='purple')

legend('topright', legend=c('América do Norte', 'Europa', 'Japão', 'Outras Regiões'), 
       col=c('blue', 'red', 'green', 'purple'), pch=1)


#Neste gráfico, as linhas representam as vendas globais de jogos ao longo dos anos para 
#cada gênero de jogo. Cada linha representa um gênero, permitindo visualizar como as vendas 
#desse gênero evoluíram ao longo do tempo.

#Gráfico de Linhas para Visualizar Vendas Globais ao Longo dos Anos por Gênero
ggplot(vgsales, aes(x = Year, y = Global_Sales, color = Genre)) + geom_line() + 
  labs(x = 'Ano', y = 'Vendas Globais', title = 'Vendas Globais ao Longo dos Anos por Gênero')


#Gráfico de Dispersão para Comparar Vendas na Europa e América do Norte
plot(vgsales$EU_Sales, vgsales$NA_Sales, xlab = "Vendas na Europa", ylab = "Vendas na América do Norte", 
     main = "Vendas na Europa vs. Vendas na América do Norte", col = "purple")

#Gráfico de Boxplot para Visualizar Distribuição de Vendas por Gênero
ggplot(vgsales, aes(x = Genre, y = Global_Sales)) +
  geom_boxplot(fill = "green") +
  labs(x = "Gênero", y = "Vendas Globais", title = "Distribuição de Vendas por Gênero") +
  theme_minimal()

#Gráfico de Área para Visualizar Evolução das Vendas por Plataforma
ggplot(vgsales, aes(x = Year, y = Global_Sales, fill = Platform)) +
  geom_area() +
  labs(x = "Ano", y = "Vendas Globais", title = "Evolução das Vendas por Plataforma") +
  theme_minimal()


#______________________________________________________________________________

# 1. Associação de Global_Sales com as outras variaveis numéricas (NA,JP,EU,Other,Year)
# 2. Gráficos: Vendas totais por Região, Vendas por Genero 
# 3. Escrever

#______________________________________________________________________________

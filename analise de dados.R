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
sum(is.na(vgsales$Publisher))                             # 58
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
# Re-ordenando ranking de vendas _________________________________________________

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

## Year: ______________________________________________________________________

year_mean <- mean(vgsales$Year)
year_median <- median(vgsales$Year)
year_moda <- names(sort(-table(vgsales$Year)))[1]
#______________________________________________________________________________




#______________________________________________________________________________
# Dados qualitativos nominais _________________________________________________
## Genre: _____________________________________________________________________

genre_fa <-table(vgsales$Genre)
View(genre_fa)

#Gráfico de freq. absoluta
cores = rainbow(12)
genre_fa_barplot <- barplot(genre_fa,las=2,main='Frequência absoluta', xlab='Gênero', 
                            ylab='Quantidade',legend.text = FALSE, col = cores)
View(genre_fa_barplot)

#Gráfico de pizza
genre_fpct <- prop.table(genre_fa) * 100
pie(genre_fpct, main='Frequência relativa percentual')


#______________________________________________________________________________
# Gráfico de barras comparativo entre Plataforma e Vendas Globais

ggplot(vgsales_raw, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Plataforma")

ggplot(vgsales, aes(x = Platform, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Plataforma")

#______________________________________________________________________________
# Gráfico de barras comparativo entre Gênero e Vendas Globais

ggplot(vgsales_raw, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Gênero")

# Gráfico de barras comparativo entre Gênero e Vendas Globais
ggplot(vgsales, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Gênero")

#______________________________________________________________________________
# Gráfico de barras comparativo entre Ano e Vendas Globais

ggplot(vgsales_raw, aes(x = Year, y = Global_Sales, fill = as.factor(Year))) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Ano")


# Gráfico de barras comparativo entre Editora e Vendas Globais
ggplot(vgsales_raw, aes(x = Publisher, y = Global_Sales, fill = Publisher)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Vendas Globais por Editora")


# Gráfico de dispersão comparativo entre Vendas NA e Vendas EU
ggplot(vgsales_raw, aes(x = NA_Sales, y = EU_Sales, color = Genre)) +
  geom_point() +
  labs(title = "Vendas na América do Norte vs Vendas na Europa")


#______________________________________________________________________________

# 1. Associação de Global_Sales com as outras variaveis numéricas (NA,JP,EU,Other,Year)
# 2. Gráficos: Vendas Globais por Ano, Vendas por Plataforma, Vendas por Publisher, Maiores 1000 Ranks por Publisher,
#               Menores 1000 Ranks por Publisher, Vendas totais por Região, Vendas por Genero, Jogos mais vendidos, 
#
# 3. Escrever


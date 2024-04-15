# Projetos realizado com o conjunto de dados do IBGE. A princípio alguns tratamentos forma realizados 
# nos dados originais. 

#  Mostrar as características da máquina usada 
sessionInfo()

# leitura do conjunto de dados
dados_ibge <- read.csv("dados.csv")

# Carregar pacote para uso
library(dplyr)

# Visualiazação do dataframe
dados_ibge

# Visualizar a estrutura do Dataset
str(dados_ibge)

# Visualizar as informações iniciais no dataset
head(dados_ibge)

# Selecionar a variável a ser estudada
select(dados_ibge, Anos.de.Estudo)

# Analisar as categorias presentes na variável selecionada
unique(select(dados_ibge, Anos.de.Estudo))

# Organizar as categorias em ordem da variáável em foco
arrange(unique(select(dados_ibge, Anos.de.Estudo)), Anos.de.Estudo)

# Ordenar o dataframe inteiro conforme a variável em destaque
arrange(dados_ibge, Anos.de.Estudo)

# criar uma VETOR para melhor visualização das categorias
c(arrange(unique(select(dados_ibge, Anos.de.Estudo)), Anos.de.Estudo))

# Análise Descritiva
summary(dados_ibge)

# Variáveis qualitativas nominais (não podem ser ordenadas ou hierarquizadas)
c(arrange(unique(select(dados_ibge, UF)), UF))

c(arrange(unique(select(dados_ibge, Sexo)), Sexo))

c(arrange(unique(select(dados_ibge, Cor)), Cor))

# Variável anos (Máximo e mínimo)
sprintf('De %s até %s anos', min(dados_ibge$Idade), max(dados_ibge$Idade))

#Outra maneira de visualizar 
min(dados_ibge$Idade)
max(dados_ibge$Idade)
summary(dados_ibge$Idade)

# Variáveis Quantitativas contínuas (altura)
min(dados_ibge$Altura)
max(dados_ibge$Altura)
summary(dados_ibge$Altura)

# ----------- Distribuição de Frequência para variáveis QUALITATIVAS-------------------

library(psych)

# Obter um resumo estatístico mais detalhado
resumo_estatistico <- describe(dados_ibge)
resumo_estatistico

# Distribuição de Frequência em UMA variáveis categóricas
table(dados_ibge$Sexo)

# Representar a contagem acima em porcentagem 
prop.table(table(dados_ibge$Sexo)) * 100

# Juntar as variáveis qualitativas para melhor visualização de gráficos Futuros
freq_qualitativa <- cbind(freq = table(dados_ibge$Sexo), percent = prop.table(table(dados_ibge$Sexo)) * 100)

# Nomear as colunas para melhor entendimento
colnames(freq_qualitativa) <- c('Frequência', 'Porcentagem (%)')
rownames(freq_qualitativa) <- c('Masculino', 'Feminino')

# Histograma
hist(dados_ibge$UF)
hist(dados_ibge$Idade)
hist(dados_ibge$Anos.de.Estudo)
hist(dados_ibge$Renda)
hist(dados_ibge$Altura)


# Visualização com Gráficos de Barras (barplot)
sexo_view <- data.frame(dados_ibge$Sexo)

barplot(table(sexo_view), main = 'Distribuição', xlab = 'Sexo', ylab = 'Frequência')

# -------------------Duas variáveis 

# Distribuição de frequência CRUZANDO duas variáveis diferentes
frequencia2 <- table(dados_ibge$Sexo, dados_ibge$Cor)
frequencia2

prop.table(table(dados_ibge$Sexo, dados_ibge$Cor))
row.names(frequencia2) <- c('Masculino', 'Feminino')
colnames(frequencia2) <- c ('Indígena', 'Branco', 'Preto', 'Amarelo', 'Parda')

frequencia2 <- cbind(frequencia2)

percentual_freq <- prop.table(frequencia2) * 100

dados_selecionados <- dados_ibge[, c("Sexo", "Cor")]

ggplot(dados_selecionados, aes(x = Cor, fill = Sexo)) +
  geom_bar() +
  labs(title = "Distribuição de Cores por Sexo", x = "Cor", y = "Frequência") +
  theme_minimal()


# -------------- Três variáveis 

# Distribuição de frequência CRUZANDO três variáveis
medias <- tapply(dados_ibge$Renda, list(dados_ibge$Sexo, dados_ibge$Cor), mean)

rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c ('Indígena', 'Branco', 'Preto', 'Amarelo', 'Parda')
medias

# --------------Distribuição de Frequência para variáveis QUANTITATIVAS --------

# Obter o mínimo e máximo da variável Renda
min(dados_ibge$Renda)
max(dados_ibge$Renda)

# Personalizando
classes <- c(0, 1576, 3152, 7880, 15760, 200000)
labels <-  c('E', 'D', 'C', 'B', 'A')

# Criar tabela de frequência
 cut( x = dados_ibge$Renda,
      breaks = classes,
      labels = labels,
      include.lowest = TRUE)
 
 cut <- table(cut( x = dados_ibge$Renda,
                   breaks = classes,
                   labels = labels,
                   include.lowest = TRUE))
cut

# Realizar o percentual 
perc_quantitativo <- prop.table(cut) * 100
perc_quantitativo

quantitativa_personalizada <- cbind("Frequência" = cut, 'Porcentagem %' = perc_quantitativo)
quantitativa_personalizada

# Ordenar (não esquecer da vírgula no final)
quantitativa_personalizada [order(row.names(quantitativa_personalizada)),]

#---------  Histograma da variável Altura
n <- nrow(dados_ibge) # número de linhas

hist(dados_ibge$Altura)
hist(
  x = dados_ibge$Altura,
  breaks = 'Sturges',
  col = 'lightblue',
  main = 'Histograma das Alturas',
  xlab = 'Altura',
  ylab = 'Frequência')

# Histograma com o ggplot da variável Altura
ggplot(dados_ibge, aes(x = Altura)) +
         geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) +
         ylab("Frequência") +
  xlab("Altura") +
  ggtitle ("Histograma das Alturas")

         
# Filtrar e criar tabelas de comando específicos 
filter(dados_ibge, Renda > 50000)

filter(dados_ibge, Altura > 1.90)

# Filtro da Renda acima de 50.000
filter_renda <- filter(dados_ibge, Renda > 50000)

 # ---------------------- Gráficos de variáveis Qualitativas

bar_chart <-  data.frame(perc_quantitativo)

# Gráfico da variável qualitativa Classes de Renda
ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Freq)) +
  geom_bar(stat = 'identity') +
  ylab("Frequência") +
  xlab("Classes de Renda") +
  ggtitle("Gráfico Classes de Renda")
  

#######################
###DEFININDO OBJETOS###
#######################
rm(list=ls()) #zerar as váriaveis

# Digite o seguinte comando em seu script:
x <- 2 #Variável numérica
y <- 3 #Variável numérica
z <- 5 #Variável numérico

letra <- "a" #Variável caractér
frase1  <- "Semana Acadêmica de Biotecnologia" #Variável string (conjunto de caracteres)
frase2 <- "Semana Acadêmica de Biologia"

# Posicione o cursor em cada linha digitada e pressione Control + Enter (comando de executar)
# Observe na aba Console se o comando foi executado
# Observe na aba "Environment" se os valores foram criados
# Sim? Próximo passo.

vetor1 <- c(x, y, z, x) #utilizando a função c() concatenar pra formar vetores
vetor1

vetor2 <- c(z, x, y, x) #utilizando a função c() concatenar pra formar vetores
vetor2

################
###OPERADORES###
################

#Existem diferentes tipos de operadores, pressione Control + Enter em cada linha para executar cada comando e observe o resultado

# (Alguns) Aritméticos
x+y

z/y

vetor1*2

# (Alguns) Relacionais

x>y

vetor1 <= x

vetor1 == vetor2

# (Alguns) Lógicos

w <- c(TRUE,TRUE,FALSE,TRUE)
k <- c(FALSE,TRUE,FALSE,FALSE)

#AND
w&k

#OR
k|w

###############
### FUNÇÕES ###
###############

#Seleciona o maior valor
max(vetor1)

#Seleciona o menor valor
min(vetor2)

#Calcula a média
mean(vetor1)

#Calcula a mediana
median(vetor1)

#Calcula a média do vetor e multiplica por 2
mean(vetor1)*2

#Gera repetições
rep(vetor1, 2)

#Gerar uma amostra randômica a partir de um conjunto de valores
sample(vetor1)


#########################
### DATAFRAME E DPLYR ###
#########################

# Na janela do console, digite: `install.packages("dplyr")`

#Carregando biblioteca
library(dplyr)

#Piping
x %>% sum(14) %>% sqrt()

#Configura alguns parâmetros para reprodução em diferentes computadores
set.seed(001)

#Criando uma tabela
tabela <- data.frame(num_palestra = rep(vetor1, 4),
                     num_pessoas = rep(vetor2*10, 4),
                     evento = sample(rep(c(frase1, frase2), 8)))

tabela

tabela %>%
  dplyr::select(num_palestra)

?data.frame
?sample

###########################
### ANÁLISE DADOS COVID ###
###########################

#Não se esqueça do install.packages()

library(ggplot2)
library(ggthemes)

#Importanto dados
datacovid <- read.csv('2020_covid_datasus.csv', sep = ';')


#Coleta e seleção das variáveis de interesse
covidselect <- datacovid %>%
  dplyr::select(DT_NOTIFIC, SG_UF_NOT, CS_SEXO, NU_IDADE_N, TP_IDADE,
                CS_RACA, CS_ESCOL_N, ID_MN_INTE , HOSPITAL, UTI, DT_ENTUTI,
                DT_SAIDUTI, SUPORT_VEN, DT_INTERNA, DT_EVOLUCA, AMOSTRA, EVOLUCAO, CLASSI_FIN)

str(covidselect)

#Data wrangling
datacovid1 <-  covidselect %>%
  #Filtragem para casos COVID-19 de Foz do Iguaçu
  dplyr::filter(ID_MN_INTE == 'FOZ DO IGUACU' & CLASSI_FIN == 5) %>%
  #Filtragem de pacientes com idade igual ou superior a 18 anos
  dplyr::filter(NU_IDADE_N >= 18 & TP_IDADE == 3) %>%
  #calculando período na UTI e Renomeando valores
  dplyr::mutate(PERIOD_UTI = as.Date(DT_SAIDUTI, "%d/%m/%Y") - as.Date(DT_ENTUTI, "%d/%m/%Y"),
                PERIOD_HOSP = as.Date(DT_EVOLUCA, "%d/%m/%Y") - as.Date(DT_INTERNA, "%d/%m/%Y"),
                UTI = case_when(UTI == 1 ~ "Sim",
                                UTI == 2 ~ "Não"))

# PLOT 1
ggplot2::ggplot(datacovid1, aes(x = CS_SEXO, y = NU_IDADE_N)) +
  geom_boxplot()

#Sumário dos dados
datacovid1 %>%
  dplyr::group_by(CS_SEXO, UTI) %>%
  dplyr::summarise(n = n())


#Retiro Variáveis CS_SEXO = I
datacovid2 <- datacovid1 %>%
  dplyr::filter(CS_SEXO != "I") %>%
  dplyr::mutate(CS_SEXO = case_when(CS_SEXO == "F" ~ "Mulheres",
                                    CS_SEXO == "M" ~ "Homens"))


ggplot2::ggplot(datacovid2, aes(x = CS_SEXO, y = NU_IDADE_N)) +
  geom_boxplot()


ggplot2::ggplot(datacovid2, aes(x = CS_SEXO, y = NU_IDADE_N)) +
  #geometry1
  geom_violin(aes(fill = UTI), trim = FALSE, position = position_dodge(0.9) ) +
  #geometry2
  geom_boxplot(aes(color = UTI), width = 0.15, position = position_dodge(0.9)) +
  #Coordenadas
  labs(x = "", y = "Idade (anos)") +
  #Theme & Colors
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("black", "black")) +
  theme_clean()


#Outras possibilidades
#https://schagas.shinyapps.io/MiVectorViz/
#https://shiny.rstudio.com/gallery/


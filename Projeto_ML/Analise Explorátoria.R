library(tidyverse)


# Carregando base de dados & visualização de variavéis

df <- read.csv('adult.csv', stringsAsFactors = T)
View(df)
str(df)

# Checando valores missing

df <- replace(df, df == '?', NA)
sapply(df, function(x) sum(is.na(x)))
sum(is.na(df))
dim(df)

# Removendo valores missing

df <- df[complete.cases(df),]  
sum(is.na(df))
dim(df)
df_Copy <- df

# Renomeando colunas para o português 

colnames(df) <- c('Idade', 'Setor', 'Renda_Familiar', 
                  'Educação', 'Anos_Estudando', 'Estado_Civil', 
                  'Ocupação', 'Relacionamento', 'Raça', 'Sexo', 
                  'Capital_Ganho', 'Capital_Perdido', 
                  'Horas_por_Semana', 'País', 'Renda')

# Explorando variáveis qualitativas

is.fact <- sapply(df, is.factor)
factors.df <- df[, is.fact]
lapply(factors.df, levels)

# Remodelando variáveis qualitativas 

# Setor:  5 níveis 

levels(df$Setor) <- list(Privado = 'Private', 
                      Publico = c('State-gov', 'Federal-gov', 'Local-gov'), 
                      Autonomo = c('Self-emp-not-inc', 'Self-emp-inc'),
                      Voluntario = 'Without-pay',
                      Nunca_trabalhou = "Never-worked")
str(df$Setor)

# Educação: 6 níveis

levels(df$Educação) <- list(Ensino_Medio_Incompleto = c("11th", "9th", "7th-8th", 
                                                        "5th-6th", "10th", 
                                                        "1st-4th", "12th", 
                                                        "preschool"),
                            Ensino_Medio_Completo = "HS-grad", 
                            Superior = c("Bachelors", "Some-college", "Prof-school"), 
                            Mestrado = "Masters",
                            Doutorado = "Doctorate",
                            Tecnologo = c("Assoc-acdm", "Assoc-voc"))
str(df$Educação)

# Estado Cívil: 2 Níveis

levels(df$Estado_Civil) <- list(Casado = c("Married-AF-spouse", "Married-civ-spouse", 
                                           "Married-spouse-absent"),
                                Solteiro = c("Widowed", "Never-married" , "Divorced", 
                                             "Separated"))
str(df$Estado_Civil)

# Ocupação: 10 Níveis

levels(df$Ocupação) <- list(Militar = "Armed-Forces", 
                            Outros = "Other-service",
                            Rural = "Farming-fishing",
                            Vendas  = "Sales",
                            Transporte = "Transport-moving",
                            Limpesa_Reparos = c("Craft-repair", "Handlers-cleaners"),
                            Segurança = c("Protective-serv", "Priv-house-serv"),
                            Adminstração = c("Adm-clerical", "Exec-managerial"),
                            Tecnologia = c("Machine-op-inspct", "Tech-support"),
                            Educação = "Prof-specialty")
str(df$Ocupação)

# Relacionamento: A Variavel relacionamento não é util para nosso objetivo, logo será descartada

df <- df[, -8]

# Raça: 5 Níveis

levels(df$Raça) <- list(Branco = "White",
                        Negro = "Black",
                        Asiaticos = "Asian-Pac-Islander",
                        Indiginas = "Amer-Indian-Eskimo",
                        Outros = "Other")
str(df$Raça)

# Sexo: 2 Níveis

levels(df$Sexo) <- list(Masculino = "Male",
                        Feminino = "Female")
str(df$Sexo)

# Analíse gráfica das variavêis
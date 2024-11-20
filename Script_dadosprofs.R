#Script superprofs
#Correlação de variáveis 

#1. Importando o banco de dados
#Rodar esse código para descobrir em qual pasta do pc colocar o arquivo
getwd()

#1.1 Maneira mais simples de importar
dadosprofs <- read.csv("Indicadores dados profs .xlsx - Sheet1.csv")
View(dadosprofs)

#1.2 Caso aquela não funcione, precisa direcionar o R para o diretorio
#Instalar e carregar os pacotes necessarios
install.packages("data.table")
library(data.table)
install.packages("here")
library(here)
#Criar o objeto e importar o banco de dados
dadosprofs <- read.csv(here("Indicadores dados profs .xlsx - Sheet1.csv"))

# 2. Fazendo ajustes no banco
# 2.1 selecionando as variaveis de trabalho
install.packages("tidyverse")
library(tidyverse)
dadosprofs <- dadosprofs %>%
  select(`ID`, `RKG`, `PAG`, `MATÉRIA`, `NOTA`, `PREÇO`, `TAREFAS`, `FORM1`, 
         `FORM1d`, `Código.IES_Form1`, `IGC_Form1`, `Código.Curso_Form1`, 
         `Enade_Form1`, `FORM2`, `FORM2d`, `Código.IES_Form2`, `IGC_Form2`, 
         `Código.curso_Form2`, `Enade_Form2`)

#2.2 Renomeando algumas das variaveis selecionadas para facilitar o trabalho
dadosprofs <- dadosprofs %>% 
  rename(`CIESF1` = Código.IES_Form1,
         `IGCF1` = IGC_Form1, 
         `CCF1` = Código.Curso_Form1, 
         `ENADEF1` = Enade_Form1, 
         `CIESF2` = Código.IES_Form2, 
         `IGCF2` = IGC_Form2, 
         `CCF2` = Código.curso_Form2, 
         `ENADEF2` = Enade_Form2)

#3. Análises de correlação (variáveis quanti)
# Variável dependente = Posição do professor no ranking
# Variáveis independentes = preço da aula, número de tarefas na plataforma, 
# nota da universidade, e nota do curso

# 3.1 Posição no Ranking VS Preço
    # Calcular a correlação
RKG_PRECO <- cor(dadosprofs$RKG, dadosprofs$PREÇO, method = "pearson", 
                  use = "complete.obs")
cat("Correlação entre RKG e PREÇO:", RKG_PRECO, "\n") #imprimir o valor 

    #Visualizar com um gráfico
# Instalar o pacote ggplot2 se ainda não estiver instalado
if (!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)

ggplot(dadosprofs, aes(x = RKG, y = PREÇO)) +
  geom_point(color = "pink", alpha = 0.7) +   
  geom_smooth(method = "lm", color = "purple", se = FALSE) +  
  labs(title = "Correlação entre Posição no ranking e preço da aula",
       x = "Posição no Ranking (RKG)",
       y = "Preço da Aula (PREÇO)",
       caption = paste("Correlação: ", round(RKG_PRECO, 2))) +
  theme_minimal()

# 3.2 Posição no Ranking VS Tarefas 


# 3.3 Posição no Ranking VS IGCF1 (Universidade)


# 3.4 Posição no Ranking VS ENADEF1 (Curso)


#4. Teste de qui-quadrado (variáveis quali)
# Variável dependente = Posição do professor no ranking
# Variáveis independentes = Grau de formação (FORM1), Matéria 

# 4.1 Posição no Ranking VS Matéria 
    # Categorizar o ranking em faixas
dadosprofs$FAIXA_RANKING <- cut(dadosprofs$RKG, 
                                breaks = c(-Inf, 10, 50, 100), # intervalos
                                labels = c("Top 10", "11-50", "51-100"))

    # Criar tabela de contingência
tabela_contingencia <- table(dadosprofs$MATÉRIA, dadosprofs$FAIXA_RANKING)

    # Visualizar a tabela
print(tabela_contingencia)

    # Realizar o teste qui-quadrado
resultado_chisq <- chisq.test(tabela_contingencia)

    # Exibir os resultados do teste
print(resultado_chisq)

    # Extrair o p-valor para o gráfico
p_valor <- resultado_chisq$p.value

    # Visualizar com um gráfico
ggplot(dadosprofs, aes(x = MATÉRIA, fill = FAIXA_RANKING)) +
  geom_bar(position = "fill") +  
  labs(title = "Distribuição do Ranking por Matéria",
       x = "Matéria",
       y = "Proporção",
       fill = "Faixa de Ranking") +
  theme_minimal() +
  annotate("text", x = 1.5, y = 1, 
           label = paste("p-valor:", format(p_valor, digits = 3)), 
           hjust = 0, size = 5, color = "black")

# 4.1 Posição no Ranking VS FORM1 (Grau de formação)

# Analises dos dados

#pacotes
library(ggplot2)
library(scales)
library(dplyr)

#Variável resposta, tipo de tumor

cont_tum <- dados |> count(diagnosis)
cont_tum |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping=aes(x=diagnosis, y = n))+
  geom_bar(stat = "identity",fill="lightblue")+
  geom_text(aes(label=n), vjust=-0.5)+
  labs(x="Diagnóstico", y= "Frequência",
       title="Distribuição de frequências segundo tipo de diagnóstico")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text=element_text(size=15))


#médias

#raio dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=radius_mean))+
  geom_boxplot()+
  labs(y="Raio",x="Diagnóstico",
       title="Média dos raios dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))

#----------------------------------------------------------------------------------------------------

#textura (valor do desvio padrão na gray-scale) dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=texture_mean))+
  geom_boxplot()+
  labs(y="Desvio-padrão da textura na gray-scale",x="Diagnóstico",
       title="Média dos desviões padrões das texturas na grar-scale dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#perímetro dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=perimeter_mean))+
  geom_boxplot()+
  labs(y="Perímetro",x="Diagnóstico",
       title="Média dos perímetros dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#área dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=area_mean))+
  geom_boxplot()+
  labs(y="Área",x="Diagnóstico",
       title="Média das áreas dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#suavidade (variação local nos comprimentos dos raios do tumores)

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=smoothness_mean))+
  geom_boxplot()+
  labs(y="Suavidade",x="Diagnóstico",
       title="Média das suavidades dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#Compactação (perímetro/área - 1) dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=compactness_mean))+
  geom_boxplot()+
  labs(y="Compactção",x="Diagnóstico",
       title="Média das compactações dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#concavidade (severidade da concavidade no contorno do tumor)

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=concavity_mean))+
  geom_boxplot()+
  labs(y="Concavidade",x="Diagnóstico",
       title="Média das severidades das concavidades no contorno do tumor segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#número de porções côncavas no contorno dos tumores

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=concavepoints_mean))+
  geom_boxplot()+
  labs(y="Pontos côncavos",x="Diagnóstico",
       title="Média das quantidades de pontos cônvacos no contorno do tumor segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#simetria

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=symmetry_mean))+
  geom_boxplot()+
  labs(y="Simetria",x="Diagnóstico",
       title="Média das simetrias dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------

#Dimensão fractal (coastline aproximation - 1)

dados |> 
  mutate(diagnosis = factor(diagnosis, levels = c("M","B"), labels = c("Maligno","Benigno"))) |> 
  ggplot(mapping = aes(x=diagnosis,y=fractal_dimension_mean))+
  geom_boxplot()+
  labs(y="Dimensão fractal",x="Diagnóstico",
       title="Média das dimensões fractais dos tumores segundo o diagnóstico")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
        text = element_text(size=15))


#----------------------------------------------------------------------------------------------------
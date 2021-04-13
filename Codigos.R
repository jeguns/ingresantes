

ESTADISTICA = read.csv('ESTADISTICA_F.csv')

ESTADISTICA %>% 
  filter(VEZCURSO==2) %>% 
  select(CODIGO) %>% 
  as.matrix() %>% 
  as.vector() -> repitentes

ESTADISTICA %>% 
  mutate(OP2   = ifelse(OPCIÓN.2=="NINGUNA",0,1),
         OP3   = ifelse(OPCIÓN.3=="NINGUNA",0,1),
         NUMOP = 1+OP2+OP3,
         PRIM  = ifelse(OPCIÓN.1=="Estadística",1,0) %>% as.factor(),
         REPITENTE = ifelse(CODIGO %in% repitentes,"SI","NO")%>% as.factor(),
         VEZCURSO  = ifelse(VEZCURSO==1,"Primera","Segunda")%>% as.factor()) -> ESTADISTICA_OK


# Estadística descriptiva -------------------------------------------------

# Estadísticas univariadas
library(skimr)
ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  skimr::skim() 

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  mutate(CICLO = factor(CICLO, labels = c("2019-1","2019-2","2020-1"))) %>% 
  count(SEXO,CICLO) %>% 
  ggplot(aes(x=CICLO,y=n,fill=SEXO,label=n))+
  geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust=0.5))+
  scale_fill_manual(values=c("gray90","gray65"))+
  labs(x="Ciclo",
       y="Número de ingresantes")+
  theme_minimal()-> graf0a

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  mutate(CICLO = factor(CICLO, labels = c("2019-1","2019-2","2020-1"))) %>% 
  mutate(GESTION = factor(GESTIÓN, labels=c("Privada","Pública","Pública"))) %>% 
  count(GESTION,CICLO) %>% 
  ggplot(aes(x=CICLO,y=n,fill=GESTION,label=n))+
  geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust=0.5))+
  scale_fill_manual(values=c("gray90","gray65"))+
  labs(x="Ciclo",
       y="Número de ingresantes")+
  theme_minimal() -> graf0b

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  mutate(CICLO = factor(CICLO, labels = c("2019-1","2019-2","2020-1"))) %>% 
  count(MODALIDAD,CICLO) %>% 
  ggplot(aes(x=CICLO,y=n,fill=MODALIDAD,label=n))+
  geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust=0.5))+
  scale_fill_manual(values=c("gray60","gray75","gray85","gray95"))+
  labs(x="Ciclo",
       y="Número de ingresantes")+
  theme_minimal() -> graf0c

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  mutate(CICLO = factor(CICLO, labels = c("2019-1","2019-2","2020-1"))) %>% 
  mutate(PRIM  = factor(PRIM, labels = c("No","Sí"))) %>% 
  rename('PRIMERA OPCION'=37) %>% 
  count(`PRIMERA OPCION`,CICLO) %>% 
  ggplot(aes(x=CICLO,y=n,fill=`PRIMERA OPCION`,label=n))+
  geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust=0.5))+
  scale_fill_manual(values=c("gray90","gray65"))+
  labs(x="Ciclo",
       y="Número de ingresantes")+
  theme_minimal()-> graf0d


gridExtra::grid.arrange(graf0a,graf0b,graf0c,graf0d,ncol=2) -> graf0 
ggsave("Imagenes/grafico0.jpg",graf0,width=12.5*1.8,height=9*1.8,units="cm")

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  count(PUNTAJE.FÍSICA) 

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  select(PUNTAJE.QUÍMICA) %>% 
  ggplot(aes(PUNTAJE.QUÍMICA)) + 
  geom_histogram()

ESTADISTICA_OK %>% 
  filter(VEZCURSO == "Primera") %>% 
  select(PUNTAJE.RM) %>% 
  ggplot(aes(PUNTAJE.RM)) + 
  geom_histogram()

# Correlaciones bivariadas
library(GGally)
ESTADISTICA_OK %>%
  select_if(is.numeric) %>% 
  ggpairs() +
  theme_bw() # De aquí seleccionamos solo aquellas con asociación significativa

ESTADISTICA_OK %>%
  select(PUNTAJE.RV,PUNTAJE.QUÍMICA,VECESADM,PARCIAL,FINAL,NOTA) %>% 
  rename('Puntaje Raz. Verbal' = 1,
         'Puntaje Química'     = 2,
         'Veces que postula'   = 3,
         'Nota de Ex. parcial' = 4,
         'Nota de Ex. final'   = 5,
         'Nota del curso'      = 6) %>% 
  ggpairs() +
  theme_bw(base_size = 12) -> grafico1

ggsave("Imagenes/grafico1.jpg",grafico1,width=12.5*1.8,height=12.5*1.8,units="cm")

ESTADISTICA_OK %>%
  select(PC,TRABAJO,PARCIAL,FINAL,NOTA) %>% 
  ggpairs() +
  theme_bw() 

# Correlaciones bivariadas
library(corrplot)
library(RColorBrewer)
ESTADISTICA_OK %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot(type = "lower",
           col  = brewer.pal(n=8, name="RdYlBu"))

ESTADISTICA_OK %>% 
  select(PUNTAJE.RV,PUNTAJE.QUÍMICA,VECESADM,FINAL,NOTA) %>% 
  cor() %>% 
  corrplot(type = "lower",
           col  = brewer.pal(n=8, name="RdYlBu"))


# Relación entre MODALIDAD DE INGRESO y NOTA DEL CURSO (NUMÉRICO)
library(ggplot2)
ESTADISTICA_OK %>% 
  mutate(MODALIDAD = factor(MODALIDAD, levels=c("Concurso Ordinario",
                                                "CEPRE-UNALM",
                                                "Primeros puestos",
                                                "Otro"))) %>% 
  select(NOTA,MODALIDAD) %>% 
  rename('Nota del curso'=1,
         'Modalidad de ingreso'=2) %>% 
  ggplot(aes(x=`Modalidad de ingreso`,y=`Nota del curso`)) + 
  geom_jitter(alpha=0.95,size=1.5,color="black",width = 0, height = 0)+
  geom_boxplot(fill="gray50",alpha=0.5,
               outlier.shape = 1,outlier.colour = "gray10",outlier.size = 5) + 
  geom_hline(yintercept=10.5,color="gray50",size=1.5)+
  scale_y_continuous(limits=c(5,20),breaks=c(0,5,10,15,20))+
  theme_minimal() -> grafico2
grafico2
ggsave("Imagenes/grafico2.jpg",grafico2,width=12.5*1.5,height=9*1.5,units="cm")


# CEPRE y Primeros puestos aprueban, pero Primeros puestos con la menor mediana
# Concurso ordinario hay de todo
# Otros y Primeros puestos pocos

# Relación entre MODALIDAD DE INGRESO Y NOTA DEL CURSO (POR GRUPO)
ESTADISTICA_OK %>% 
  select(MODALIDAD,GRUPO) %>% 
  table() %>% 
  prop.table(1)
# CEPRE con el menor % en grupo de NOTA BAJA # ....

# Relación entre NÚMERO DE OPCIONES Y NOTA DEL CURSO (POR GRUPO)
ESTADISTICA_OK %>% 
  select(NUMOP,GRUPO) %>% 
  table() %>% 
  prop.table(1)

# Relación entre MARCARON O NO OPCION 1 y NOTA DEL CURSO (POR GRUPO)
# No se encontraron diferencias
ESTADISTICA_OK %>% 
  select(OPCIÓN.1,GRUPO) %>% 
  table() %>% 
  prop.table(1)

ESTADISTICA_OK %>% 
  select(OPCIÓN.1,GRUPO) %>% 
  table() %>% 
  chisq.test()

# Relación entre numero de veces que postularon y la nota obtenida
ESTADISTICA_OK %>% 
  mutate(VECESADM2 = cut(VECESADM,breaks=c(0,2,7),labels=c("1-2","3+"))) %>% 
  select(VECESADM2,GRUPO) %>% 
  table() %>% 
  chisq.test() # pvalor = 0.04104 # significativo !!! 

ESTADISTICA_OK %>% 
  mutate(VECESADM2 = cut(VECESADM,breaks=c(0,2,7),labels=c("1-2","3+"))) %>% 
  ggplot(aes(x=VECESADM2,y=NOTA)) + 
  geom_boxplot()  

ESTADISTICA_OK %>% 
  mutate(VECESADM2 = cut(VECESADM,breaks=c(0,2,7),labels=c("1-2","3+"))) %>% 
  group_by(VECESADM2) %>% 
  summarise(mean(NOTA),sd(NOTA),min(NOTA),median(NOTA),max(NOTA))

## CLUSTERING

ESTADISTICA_OK_CUANTI = ESTADISTICA_OK %>% 
  select_if(is.numeric) %>% 
  select(-OP2,-OP3)

library(factoextra)

get_clust_tendency(ESTADISTICA_OK_CUANTI, 
                   n = nrow(ESTADISTICA_OK_CUANTI)-1) # 0.650992 clusterizable

library(factoextra)
library(cluster)
ESTADISTICA_OK_CUANTI %>% 
  scale() %>% 
  fviz_nbclust(FUNcluster = kmeans, 
               method     = "silhouette", 
               k.max      = 15,
               barfill    = "black",
               barcolor   = "black",
               linecolor  = "black") +
  labs(title = "Número adecuado de clusters",
       x = "Número de clusters",
       y = "Ancho medio de la silueta") -> grafico3

ggsave("Imagenes/grafico3.jpg",grafico3,width=15,height=10,units="cm")


ESTADISTICA_OK_CUANTI %>% 
  scale() %>% 
  kmeans(centers = 2) -> resu.clust

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  count(CLUSTER)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  summarise_if(is.numeric,mean) %>% View()

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(PRIM)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(SEXO)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(MODALIDAD)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(GESTIÓN)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(FEXAM)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(GESTIÓN)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(GRUPO)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(REPITENTE,VEZCURSO)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  group_by(CLUSTER) %>% 
  count(VEZCURSO)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  select(PC,TRABAJO,PARCIAL,FINAL,NOTA,CLUSTER) %>% 
  rename('Prácticas Calificadas'= 1,
         'Trabajo grupal'  = 2,
         'Examen parcial'  = 3,
         'Examen final'    = 4,
         'Nota final'      = 5,
         Cluster           = 6) %>% 
  mutate(Cluster = as.factor(Cluster)) %>% 
  mutate(CLUSTER = ifelse(Cluster=="1",4,16)) -> DATOS_GRAFICA4

(ggpairs(DATOS_GRAFICA4[,1:5], 
         aes(colour = DATOS_GRAFICA4$Cluster, alpha = 0.4),
         lower = list(continuous=wrap("points", 
                                      colour = 'black',
                                      size   = 1.5,
                                      shape  = DATOS_GRAFICA4$CLUSTER)),
         diag = list(continuous = wrap("densityDiag", alpha=0.75))) -> grafico4)

for(i in 1:grafico4$nrow) {
  for(j in 1:grafico4$ncol){
    grafico4[i,j] <- grafico4[i,j] + 
      scale_fill_manual(values=c("gray60", "black", "green", "yellow", "black")) +
      scale_color_manual(values=c("gray60", "black", "green", "yellow", "black"))  
  }
}


grafico4
ggsave("Imagenes/grafico4.jpg",grafico4,width=12.5*2,height=10*2,units="cm")

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  select(PUNTAJE.RM,PUNTAJE.RV,PUNTAJE.MATEMÁTICAS,PUNTAJE.FÍSICA,PUNTAJE.QUÍMICA,PUNTAJE.BIOLOGÍA,CLUSTER) %>% 
  rename(Cluster = 7) %>%  
  mutate(Cluster = as.factor(Cluster)) %>%
  mutate(CLUSTER = ifelse(Cluster=="1",4,16)) -> DATOS_GRAFICA5

(ggpairs(DATOS_GRAFICA5[,1:6], 
         aes(colour = DATOS_GRAFICA5$Cluster, alpha = 0.4),
         lower = list(continuous=wrap("points", 
                                      colour = 'black',
                                      size   = 1.5,
                                      shape  = DATOS_GRAFICA5$CLUSTER)),
         diag = list(continuous = wrap("densityDiag", alpha=0.75))) -> grafico5)

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = as.factor(resu.clust$cluster)) %>% 
  select(CLUSTER,PC) %>% 
  ggplot(aes(x=PC,group=CLUSTER,fill=CLUSTER))+
  geom_density(alpha=0.5) 

ESTADISTICA_OK %>% 
  dplyr::mutate(CLUSTER = resu.clust$cluster) %>% 
  filter(CLUSTER==1) %>% View()

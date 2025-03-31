
### Diplomado ANALISIS SENSORIAL DE ALIMENTOS
### Aplicación: datos sensoriales de café
### 2024

library(Factoshiny) ## Interfaz dinámica con los resultados multivariados.
library(FactoMineR) ## ACP y Cluster.
library(corrplot)   ## Visualiza la matriz de correlación.
library(factoextra) ## Mejoras en la visualización multivariada.

## cargar el conjunto de datos
read.table("coffeData.csv",header=TRUE,sep=";",
dec=".",row.names=1,stringsAsFactors = TRUE) -> dat.act

### estadisticas descriptivas
summary(dat.act)

### histogramas de las variables cuantitativas

hist(dat.act$Fragrance...Aroma, col="blue")
hist(dat.act$Flavor, col="blue")
hist(dat.act$Aftertaste, col="blue")
hist(dat.act$Salt...Acid, col="blue")
hist(dat.act$Mouthfeel, col="blue")
hist(dat.act$Balance, col="blue")
hist(dat.act$Bitter...Sweet, col="blue")
hist(dat.act$Uniform.Cup, col="blue")
hist(dat.act$Clean.Cup, col="blue")
hist(dat.act$quality_score, col="blue")

### matriz de correlación
corX = cor(dat.act[,-c(1,2)])
corrplot(corX, method="number")


### ACP 
res.PCA.cafe <- PCA(dat.act,quali.sup = c(1,2))

PCAshiny(res.PCA.cafe) ## interfaz dinámica con los resultados del ACP.

### Analisis de cluster jeararquico.
Cluster <- HCPC(res.PCA.cafe,nb.clust=-1)

Cluster$desc.var  ### Importante para interpretar.

fviz_cluster(Cluster) ### Gráfico de los clusters.
HCPCshiny(res.PCA.cafe) ### Interfaz dinámica con los resultados.



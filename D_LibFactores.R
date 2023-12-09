library("FactoMineR")
library("factoextra")
library("corrplot")
library("RColorBrewer")
library("PerformanceAnalytics")
library("gplots")
library("stats")
library("readxl")
library("Hmisc")
library("corrplot")
library("plotly")
library("ggcorrplot")
library("ggthemes")
library("data.table")
TabContingencia_MaxMinXFila <- function(dataMM, NombreCMax, NombreCMin) {
  nInt <- nrow(dataMM)
  IndiceMax <- numeric(nInt)
  IndiceMin <- numeric(nInt)
  ClaseMax <- character(nInt)
  ClaseMin <- character(nInt)
  
  for (i in 1:nInt) {
    row <- dataMM[i, ]
    col_index_max <- which.max(row)
    col_index_min <- which.min(row)
    ClaseMax[i] <- names(dataMM)[col_index_max]
    ClaseMin[i] <- names(dataMM)[col_index_min]
    IndiceMax[i] <- col_index_max
    IndiceMin[i] <- col_index_min
  }
  
  dataOb <- data.frame(IndiceMax, IndiceMin, ClaseMax, ClaseMin)
  names(dataOb)[3:4] <- c(NombreCMax, NombreCMin)
  return(list(tabla = dataOb, longitud = nInt))
}

# Usar desde el modelo de correspondencia respecto a las relaciones con corrd, para
# establecer como factor y dimension las relaciones de grupo de palabras.
# >source("D_LibFactores.R")
# > tcos<-rmPCA$modelo$ind$coord
# > tg<-GenerarDataframeT(tcos,"DimMax","DimMin")
# > tg[1],tg[2]...
GenerarDataframeT <- function(dataFrame, NombreCMax, NombreCMin) {
  # Asegurarse de que el data frame se convierta a data.frame
  dataFrame <- as.data.frame(dataFrame)
  
  # Aplicar la función TabContingencia_MaxMinXFila
  resultado <- TabContingencia_MaxMinXFila(dataFrame, NombreCMax, NombreCMin)
  
  # Agregar la columna IndiceMax al data frame original
  dataFrame <- data.frame(dataFrame, resultado$tabla$IndiceMax)
  
  # Crear el nuevo data frame t
  t <- data.frame(Palabra = rownames(dataFrame), Dim = factor(dataFrame[, 5]))
  colnames(t) <- c("Palabra", "Dim")
  
  # Dividir el data frame en una lista de data frames por los valores de "Dim"
  lista_data_frames <- split(t, t$Dim)
  
  return(lista_data_frames)
}

vector_a_cadena <- function(vector) {
  # Utiliza la función paste para unir los elementos del vector con comas
  cadena_resultante <- paste(vector, collapse = ", ")
  return(cadena_resultante)
}

DividirPorPercentiles <- function(data_table, num_percentiles) {
  # Verificar si el data table tiene una columna llamada "valoracion"
  if (!"valoracion" %in% colnames(data_table)) {
    stop("El data table debe contener una columna llamada 'valoracion'.")
  }
  
  # Obtener los valores numéricos de la columna "valoracion"
  valores_numericos <- data_table$valoracion
  
  # Calcular los percentiles
  percentiles <- quantile(valores_numericos, probs = seq(0, 1, length.out = num_percentiles))
  
  # Crear una lista de datatables divididos por los percentiles
  grupos <- list()
  for (i in 1:(num_percentiles - 1)) {
    min_percentil <- percentiles[i]
    max_percentil <- percentiles[i + 1]
    
    # Filtrar filas dentro del rango del percentil
    grupo <- data_table[valores_numericos >= min_percentil & valores_numericos <= max_percentil, , drop = FALSE]
    
    grupos[[i]] <- as.data.frame(grupo)
  }
  
  # Agregar el último grupo
  # ultimo_grupo <- data_table[valores_numericos > max(percentiles), , drop = FALSE]
  # grupos[[num_percentiles]] <- ultimo_grupo
  
  return(list(Percentiles=percentiles,Grupos=grupos))
}

#Paletas:: “npg”, “aaas”, “lancet”, “jco”, “ucscgb”, “uchicago”, “simpsons” and “rickandmorty”
#gradientes Color:..En prompt escribir:RColorBrewer::display.brewer.all()
##Análisis CA Componentes Principales
##Se necesita una tabla de contingencia
resModeloCA <- function(dataCA, titulo, col_paleta = NULL) {
  # Realizar el análisis de componentes principales
  res.ca <- CA(dataCA, graph = FALSE)
  
  # Calcular el chi-cuadrado y p-valor
  chq <- chisq.test(dataCA)
  
  # Obtener la tabla de contingencia en porcentajes
  tabla_porcentaje <- round((dataCA / sum(dataCA[1, ]) * 100), 2)
  
  # Obtener la matriz de cosenos para filas y columnas
  cos2_row <- res.ca$col$cos2
  cos2_col <- res.ca$row$cos2
  
  # Calcular la varianza explicada
  eig.val <- get_eigenvalue(res.ca)
  
  # Definir paleta de colores si se proporciona
  if (!is.null(col_paleta)) {
    biplot_simetrico <- fviz_ca_biplot(res.ca, repel = TRUE, geom = c("point", "text"), alpha.row = 0.5, palette = col_paleta)
    biplot_asimetrico <- fviz_ca_biplot(res.ca, map = "rowprincipal", arrow = c(TRUE, TRUE), repel = TRUE, palette = col_paleta)
  } else {
    biplot_simetrico <- fviz_ca_biplot(res.ca, repel = TRUE, geom = c("point", "text"), alpha.row = 0.5)
    biplot_asimetrico <- fviz_ca_biplot(res.ca, map = "rowprincipal", arrow = c(TRUE, TRUE), repel = TRUE)
  }
  
  return(list(
    Tabla = dataCA,
    TablaPorcentaje = tabla_porcentaje,
    ChiCuadrado = chq$statistic,
    P_Valor = chq$p.value,
    modelo = res.ca,
    VarianzaExplicada = eig.val,
    BiplotSimetrico = biplot_simetrico,
    BiplotAsimetrico = biplot_asimetrico,
    PlotxFila = corrplot(cos2_row, is.corr = FALSE),
    PlotxColum = corrplot(cos2_col, is.corr = FALSE)
  ))
}

# dx<-dataR$MatrizTranspuestaSinDup
# rmca<-resModeloPCA(dx,1) 
# el 1 es el indice que no entra a evaluación por si fuera una contingencia con factor
# rmca<-resModeloPCA(resultados$VectorTextoSentimientosNRC,4) 4 es indice por factor en la contingencia
resModeloPCA <- function(dataPCA, indCluster) {
  # Realizar el análisis de componentes principales
  res.pca <- PCA(dataPCA[, -indCluster], ncp=4, graph = FALSE)
  # Obtener valores importantes
  eig.val <- get_eigenvalue(res.pca)
  var <- get_pca_var(res.pca)
  ind <- get_pca_ind(res.pca)
  # Clustering de variables
  set.seed(123)
  res.km <- kmeans(var$coord, centers = 2, nstart = 15)
  grp <- as.factor(res.km$cluster)
  return(list(
    modelo = res.pca,
    propVarianza = eig.val,
    variablesActivas = var,
    graficaDim = fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)),
    corrDimCos2 = corrplot(var$cos2, is.corr = FALSE),
    grafMapFactorCos2 = fviz_pca_var(res.pca, col.var = "cos2", 
                                     gradient.cols = c("#111111", "#111FFF", "#FF1111","#00AFAF"), 
                                     repel = TRUE),
    corrIndCos2 = corrplot(ind$cos2, is.corr = FALSE),
    grafMapaIndCos2 = fviz_pca_ind(res.pca, col.ind = "cos2", 
                                   gradient.cols = c("#111111", "#111FFF", "#FF1111","#00AFAF"),
                                   repel = TRUE # Avoid text overlapping (slow if many points)
                                   ),
    graficaClusterGrupos = fviz_pca_var(res.pca, col.var = grp, 
                                        palette = c("#FC4477", "#00755F", "#111111", "#111FFE"), 
                                        legend.title = "Cluster"),
    graficaClusterXGrupos = fviz_pca_ind(res.pca, geom.ind = "point", col.ind = dataPCA[, indCluster], 
                                         palette = "lancet", ggtheme = theme_bw(), 
                                         addEllipses = FALSE, legend.title = "Groups"),
    graficaXGruposXInd = fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969"),
    graficaClusterXGruposXInd = fviz_pca_biplot(res.pca, geom.ind = "point", 
                                                fill.ind = dataPCA[, indCluster], col.ind = "black", 
                                                pointshape = 21, pointsize = 2, palette = "lancet", 
                                                addEllipses = TRUE, alpha.var = "contrib", 
                                                col.var = "contrib", gradient.cols = "RdYlBu", 
                                                legend.title = list(fill = "AcepEns", 
                                                                    color = "Contrib", alpha = "Contrib"))
  ))
}

resModeloCluster <- function(dataClus, indiceClase, vectorClase) {
  # Realizar el análisis de componentes principales
  res.pca <- PCA(dataClus[, -indiceClase], graph = FALSE)
  res.hcpc <- HCPC(res.pca, graph = FALSE)
  # Obtener valores importantes
  eig.val <- get_eigenvalue(res.pca)
  var <- get_pca_var(res.pca)
  # Graficar individuos
  graficaInd <- fviz_pca_ind(res.pca, repel = TRUE, habillage = vectorClase, addEllipses = FALSE, palette = "lancet")
  return(list(
    modelo = res.pca,
    propVarianza = eig.val,
    variablesActivas = var,
    graficaDend=fviz_dend(res.hcpc, 
                          cex = 0.7,                     # Label size
                          palette = ("lancet"),               # Color palette see ?ggpubr::ggpar
                          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                          rect_border = "jco",           # Rectangle color
                          labels_track_height = 0.8      # Augment the room for labels
    ),
    graficaCluster=fviz_cluster(res.hcpc,
                                repel = TRUE,            # Avoid label overlapping
                                show.clust.cent = TRUE, # Show cluster centers
                                palette = ("lancet"),         # Color palette see ?ggpubr::ggpar
                                ggtheme = theme_minimal(),
                                main = "Factor map"
    ),
    graficaDim = fviz_pca_ind(res.pca, geom.ind = "point", col.ind = vectorClase, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, legend.title = "Grupos"),
    graficaCof = fviz_pca_ind(res.pca, geom.ind = "point", col.ind = vectorClase, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, ellipse.type = "confidence", legend.title = "Grupos"),
    graficaInd = graficaInd,
    graficaCompleta = ggpubr::ggpar(graficaInd, title = "Analisis de Componentes Principales", 
                                    subtitle = "Estudio -", caption = "Source: factoextra", 
                                    xlab = "PC1", ylab = "PC2", legend.title = "Estudio", 
                                    legend.position = "top", ggtheme = theme_fivethirtyeight(), 
                                    palette = "lancet"),
    graficaBiplot = fviz_pca_biplot(res.pca, col.ind = vectorClase, palette = "lancet", addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Estudio Clases"),
    graficaBiplot2 = fviz_pca_biplot(res.pca, geom.ind = "point", fill.ind = vectorClase, col.ind = "black", pointshape = 21, pointsize = 2, palette = "lancet", addEllipses = TRUE, alpha.var = "contrib", col.var = "contrib", gradient.cols = "Set1", legend.title = list(fill = "Estudio", color = "Contrib", alpha = "Contrib"))
  ))
}
# > g<-c(1,1,3,4,4,3)
# > t<-c("n","n","n","n","s","s")
# > nom<-c("Sexo","Tipo.Ing","Pen.Comp","Uso.Evi.Prog","Pen.Comp","Prog.Comp")
# > rdim<-c(1,6)
# >ind<-2
# > r.mfa<-resModeloMFA(dataUV,g,t,nom,rdim,ind)
# > indice<-2 (indice de tabla inicial - cualitativa o factor - dimensión de clasificación)
resModeloMFA <- function(dataMFA, grupos = NULL, tipos = NULL, nombres = NULL, rangoDim = NULL, indice) {
  res.mfa <- MFA(dataMFA, group = grupos, type = tipos, name.group = nombres, num.group.sup = rangoDim, graph = FALSE)
  eig.val <- get_eigenvalue(res.mfa)
  group <- get_mfa_var(res.mfa, "group")
  quanti.var <- get_mfa_var(res.mfa, "quanti.var")
  quali.var <- get_mfa_var(res.mfa, "quali.var")
  
  return(list(
    modelo = res.mfa,
    propVarianza = eig.val,
    graficaDim = fviz_screeplot(res.mfa),
    grupos = group,
    graficaGrupos = fviz_mfa_var(res.mfa, "group"),
    varCuanti = quanti.var,
    graficaCorrCuanti = fviz_mfa_var(res.mfa, "quanti.var", palette = "lancet", col.var.sup = "violet", repel = TRUE, geom = c("point", "text"), legend = "bottom"),
    graficaCorrCuali = fviz_mfa_var(res.mfa, "quali.var", palette = "lancet", col.var.sup = "violet", repel = TRUE, geom = c("point", "text"), legend = "bottom"),
    graficaCorrCuantiDimen = fviz_mfa_var(res.mfa, "quanti.var", palette = "lancet", col.var.sup = "violet", repel = TRUE),
    graficaCorrCualiDimen = fviz_mfa_var(res.mfa, "quali.var", palette = "lancet", col.var.sup = "violet", repel = TRUE),
    graficaEjesParciales = fviz_mfa_axes(res.mfa),
    graficaInd = fviz_mfa_ind(res.mfa, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE),
    graficaIndCat = fviz_mfa_ind(res.mfa, habillage = names(dataMFA[indice]), palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, ellipse.type = "confidence", repel = TRUE)
  ))
}

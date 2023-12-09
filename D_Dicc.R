library(googleLanguageR)
library(cld2)
library(tidyverse)
library(datasets)
library(corpus)
library(tm) # Framework for text mining.
library(readxl)
library(syuzhet)
library(NLP)
library(scales)
library(ggplot2)
nombresDataFrameEstudio<-c("ira","anticipación","asco","miedo","alegría",
                          "tristeza","sorpresa","confianza","negativo",
                          "positivo","syuzhet","bing","afinn","nrc")
nombresSentPNDataFrameEstudio<-c("ira","anticipación","asco","miedo","alegría",
                           "tristeza","sorpresa","confianza","negativo",
                           "positivo")
# UnirExcelITFF: Función para extraer comentarios IFFT desde archivos Excel 
# y realizar análisis de sentimiento.
# Parámetros:
#   nombreBase: Nombre base de los archivos Excel (sin la numeración ni la extensión)
#   IndIni: Índice inicial de archivos a leer
#   IndFin: Índice final de archivos a leer
#   IdiomaIni: Idioma inicial del texto
#   IdiomaFin: Idioma al que se desea traducir el texto (si SiTrad es TRUE)
#   IdiomaLimpieza: Idioma para la limpieza del texto
#   valorPor: Número de porciones para el análisis de sentimiento
#   SiTrad: Bandera que indica si se requiere traducción (TRUE) o no (FALSE)
# Retorna:
#   Una lista con diversos resultados del análisis de los comentarios IFFT.

# Ejemplo de llamada a la función:
# resultados <- UnirExcelITFF(nombreBase = "Data/Aux",
#                             IndIni = 1,
#                             IndFin = 1,
#                             IdiomaIni = "es",
#                             IdiomaFin = "en",
#                             IdiomaLimpieza = "english",
#                             valorPor = 10,
#                             SiTrad = FALSE)

# Los resultados almacenados en la lista "resultados" pueden ser accedidos
# utilizando los nombres de los elementos de la lista, por ejemplo:
# - resultados$UnionXLSX: DataFrame con los datos extraídos de los archivos Excel.
# - resultados$TextoSentencias: Vector con las sentencias de los comentarios.
# - resultados$PorNPSyuzhetVector: Vector con los porcentajes de sentimiento según el método "syuzhet".
# - ...

# Además, los resultados pueden ser exportados en diferentes formatos según las necesidades del usuario.
# Por ejemplo, para exportar el DataFrame "UnionXLSX" a un archivo CSV:
# write.csv(resultados$UnionXLSX, file = "comentarios_union.csv", row.names = FALSE)

# Para exportar los porcentajes de sentimiento a un archivo CSV:
# write.csv(resultados$PorNPSyuzhetVector, file = "porcentajes_syuzhet.csv", row.names = FALSE)
UnirExcelITFF <- function(nombreBase, IndIni, IndFin, IdiomaIni, IdiomaFin, IdiomaLimpieza, valorPor, SiTrad) {
  
  # Crear un dataframe vacío para almacenar los datos
  dataF <- data.frame(usuario = character(),
                      tuit = character(),
                      linkEstado = character(),
                      fecha = character())
  
  # Leer y unir los archivos Excel
  for (i in IndIni:IndFin) {
    dataT <- read_excel(paste0(nombreBase, i, ".xlsx"), 
                        col_names = c("usuario", "tuit", "linkEstado", "fecha"))
    dataF <- rbind(dataF, dataT)
  }
  # Realizar traducción si es necesario
  if (SiTrad) {
    textoFinal <- TraduceIdioma(dataF$tuit, IdiomaIni, IdiomaFin)
  } else {
    textoFinal <- dataF$tuit
  }
  # Dividir el texto en sentencias y realizar limpieza
  textoSentencias <- get_sentences(LimpiaTexto(textoFinal, IdiomaLimpieza))
  poa_Palabras_v <- get_tokens(textoSentencias, pattern = "\\W")
  poa_Palabras_v<- eliminarduplicados(poa_Palabras_v)
  # Realizar análisis de sentimiento con diferentes métodos
  metodos <- c("syuzhet", "bing", "afinn", "nrc")
  matricesSentimiento <- lapply(metodos, function(metodo) {
    get_sentiment(poa_Palabras_v, method = metodo, lang = IdiomaLimpieza)
  })
  # Combinar las matrices de análisis de sentimiento
  MCMV <- do.call(rbind, matricesSentimiento)
  colnames(MCMV) <- poa_Palabras_v
  rownames(MCMV) <- metodos
  # Crear una matriz simplificada valorada por método
  MatSimpMCMC <- MatrizValoradaXMetodo(MCMV, SiTrad)
  # Obtener porcentajes de valores de sentimiento
  porcentajes <- lapply(matricesSentimiento, function(vectorSentimiento) {
    get_percentage_values(vectorSentimiento, bins = valorPor)
  })
  # Aplicar la transformación discreta del coseno (DCT) a los valores de sentimiento
  dct_valores <- lapply(matricesSentimiento, function(vectorSentimiento) {
    get_dct_transform(vectorSentimiento, low_pass_size = 5, x_reverse_len = 100, 
                      scale_vals = FALSE, scale_range = TRUE)
  })
  # Obtener sentimientos por token (Sin Ceros para NRC) y por sentencia
  tSentimientos_data <- get_sentiment(poa_Palabras_v)
  tSentimientos_nrc_data <- get_nrc_sentiment(poa_Palabras_v)
  rownames(tSentimientos_nrc_data)<-poa_Palabras_v
  poa_palabrasNRC<-poa_Palabras_v
  sSentimientos_data <- get_sentiment(textoSentencias)
  sSentimientos_nrc_data <- get_nrc_sentiment(textoSentencias)

  # Retornar una lista con los resultados
  return(list(UnionXLSX = dataF,
              SoloTuit = dataF$tuit,
              TextoIdiomaFinal = textoFinal,
              TextoSentencias = textoSentencias,
              TextoTokens = poa_Palabras_v,
              PorNPSyuzhetVector = porcentajes[[1]],
              PorNPBingVector = porcentajes[[2]],
              PorNPAfinnVector = porcentajes[[3]],
              PorNPNRCVector = porcentajes[[4]],
              TDCSyuzhet = dct_valores[[1]],
              TDCBing = dct_valores[[2]],
              TDCAfinn = dct_valores[[3]],
              TDCNRC = dct_valores[[4]],
              MatrizComparaMetodosVectores = MCMV,
              MatrizSimplificadaTokensValorados = MatSimpMCMC,
              VectorTextoSentimientos = tSentimientos_data,
              VectorSTextoSentimientos = sSentimientos_data,
              VectorTextoSentimientosNRC = tSentimientos_nrc_data,
              VectorSTextoSentimientosNRC = sSentimientos_nrc_data))
}

### MatrizValoradaXMetodo: Limpia y reorganiza una matriz de sentimientos valorados por método.
### Parámetros:
###   mdata: Matriz de sentimientos (4 x N) con 4 métodos y N tokens.
###   tradToken: Indica si se requiere traducción de los tokens (TRUE/FALSE).
### Retorna:
###   Una lista con varios resultados y matrices relacionados.

MatrizValoradaXMetodo <- function(mdata = matdat[][], tradToken) {
  # Filtrar las columnas de la matriz donde al menos un método tenga un valor distinto de cero
  mcol0 <- mdata[, mdata[1,] != 0 | mdata[2,] != 0 | mdata[3,] != 0 | mdata[4,] != 0]
  
  # Transponer la matriz y eliminar filas duplicadas
  mtran <- t(mcol0)
  mtranSinDup <- mtran[!duplicated(rownames(mtran)), ]
  
  if (tradToken == TRUE) {
    # Si se requiere traducción, obtener traducciones de los tokens
    vtokEsp <- vTraduceEn(rownames(mtran))
    mtranEsp <- mtran
    rownames(mtranEsp) <- vtokEsp[, 3]
  } else {
    # Si no se requiere traducción, mantener los tokens originales
    vtokEsp <- rownames(mtran)
    mtranEsp <- mtran
  }
  
  # Obtener el número de filas con y sin duplicados
  nfilasConDup <- nrow(mtran)
  nfilasSinDup <- nrow(mtranSinDup)
  
  # Retornar una lista con los resultados
  return(list(MatrizCol0 = mcol0,
              MatrizTranspuesta = mtran,
              MatrizTranspuestaSinDup = mtranSinDup,
              TraduccTokens = vtokEsp,
              MatrizTranspuestaEspa = mtranEsp,
              NumeroFilasConDup = nfilasConDup,
              NumeroFilasSinDup = nfilasSinDup))
}
### vTraduceEn: Traduce un vector de textos utilizando Google Translate.
### Parámetros:
###   texto: Vector de textos a traducir.
### Retorna:
###   Un dataframe con las columnas Orig (original), IdioOrig (idioma original) y Traduccion (traducción).
vTraduceEn <- function(texto = c()) {
  gl_auth("tesis-d-8a1c40b0af73.json")
  Trad <- IdioOrig <- Traduccion <- character(0)
  
  for (i in seq_along(texto)) {
    Trad <- gl_translate(texto[i], target = "es", format = "text", source = "")
    Orig[i] <- Trad[[3]]
    IdioOrig[i] <- Trad[[2]]
    Traduccion[i] <- Trad[[1]]
  }
  
  df <- data.frame(Orig, IdioOrig, Traduccion)
  return(df)
}

### first_letter_upper: Pone en mayúscula la primera letra de cada frase.
first_letter_upper <- function(x) {
  ifelse(is.na(x) == TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
}

### TraduceIdioma: Traduce el texto de un idioma a otro utilizando Google Translate.
### Parámetros:
###   x: Texto a traducir.
###   abrevidiomaIni: Abreviatura del idioma original.
###   abrevidiomaFin: Abreviatura del idioma objetivo de la traducción.
### Retorna:
###   Un dataframe con el texto traducido.
TraduceIdioma <- function(x, abrevidiomaIni, abrevidiomaFin) {
  gl_auth("tesis-d-8a1c40b0af73.json")
  s <- LenguajeDetectado(x)
  sn <- s$NuevaSentencia
  ld <- s$LenguajeDetectado
  
  for (i in 1:ncol(sn)) {
    sn[, i][ld[, i] == abrevidiomaIni & !is.na(ld[, i])] <- 
      data.frame(gl_translate(sn[, i][ld[, i] == abrevidiomaIni & !is.na(ld[, i])], target = abrevidiomaFin))[, 1]
    sn[, i][sn[, i] %in% c("NA", "N/a", "N/A", "Na", "na", "n/a", "not applicable")] <- NA
  }
  
  return(sn)
}

### LimpiaTexto: Realiza una serie de operaciones para limpiar el texto.
### Parámetros:
###   nov_text: Texto a limpiar.
###   idioma: Idioma del texto.
### Retorna:
###   Texto limpio.
LimpiaTexto <- function(nov_text, idioma) {
  nov_text <- nov_text %>% as.matrix
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  
  nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
  nov_text <- tolower(nov_text)
  nov_text <- removeWords(nov_text, words = stopwords(idioma))
  nov_text <- removePunctuation(nov_text)
  nov_text <- removeNumbers(nov_text)
  nov_text <- removeNumPunct(nov_text)
  nov_text <- removeURL(nov_text)  
  nov_text <- stripWhitespace(nov_text)
  nov_text <- trimws(nov_text) 
  return(nov_text)
}


# Función para seleccionar las filas donde todos los valores de 
# columna sean cero
seleccionarfilas <- function(datatable) {
  filas_seleccionadas <- datatable %>%
    filter_all(any_vars(. != 0))
  return(filas_seleccionadas)
}
# Función que elimina de un vector los duplicados
eliminarduplicados <- function(vector) {
  vector_sin_duplicados <- unique(vector)
  return(vector_sin_duplicados)
}

# Función para obtener la intersección de dos data frames por los nombres de fila
interseccionDataFrames <- function(df1, df2) {
  # Obtener los nombres de fila comunes
  nombres_comunes <- intersect(rownames(df1), rownames(df2))
  
  # Filtrar los data frames originales usando los nombres comunes
  df1_interseccion <- df1[nombres_comunes, , drop = FALSE]
  df2_interseccion <- df2[nombres_comunes, , drop = FALSE]
  
  # Unir los data frames resultantes en uno solo por columnas
  interseccion <- cbind(df1_interseccion, df2_interseccion)
  
  # Obtener los nombres de fila del data frame
  nombres <- rownames(interseccion)
  
  # Convertir los nombres en una cadena separada por comas
  nombresCadena <- paste(nombres, collapse = ", ")
  nombresCadena <- gsub("\\s*,\\s*", ",", nombresCadena)
  # Paso 1: Eliminar caracteres vacíos
  palabras <- unlist(strsplit(nombresCadena, ","))
  palabras <- gsub(" ", "", palabras)
  
  
  
  # Paso 2: Eliminar tildes de cada palabra
  palabras <- lapply(palabras, function(palabra) {
    palabra <- gsub("[áäâà]", "a", palabra)
    palabra <- gsub("[éëêè]", "e", palabra)
    palabra <- gsub("[íïîì]", "i", palabra)
    palabra <- gsub("[óöôò]", "o", palabra)
    palabra <- gsub("[úüûù]", "u", palabra)
    return(palabra)
  })
  
  # Unir las palabras limpias en una cadena nuevamente
  cadenaLimpia <- paste(palabras, collapse = ", ")
  
  # Devolver el data frame resultante
  return(list(TablaCompletaNRC=interseccion, 
              NombreFilas=cadenaLimpia))
}

# Función para modificar los nombres de fila de un data frame
modificarNombresFilas <- function(dataFrame, nombresCadena) {
  # Dividir la cadena de nombres por comas para obtener un vector de nombres
  nuevosNombres <- unlist(strsplit(nombresCadena, ","))
  
  # Eliminar espacios en blanco alrededor de los nombres
  nuevosNombres <- trimws(nuevosNombres)
  
  # Verificar y agregar sufijos a los nombres duplicados
  nombresUnicos <- make.unique(nuevosNombres, sep = "_")
  
  # Asignar los nuevos nombres a las filas del data frame
  rownames(dataFrame) <- nombresUnicos
  colnames(dataFrame)<- nombresDataFrameEstudio
  return(dataFrame)
}

# Función para guardar un data frame en formato CSV
guardarDataFrameCSV <- function(dataFrame, nombreArchivo) {
  # Guardar el data frame en un archivo CSV
  write.csv(dataFrame, file = nombreArchivo, row.names = TRUE)
}

# Función para leer un archivo CSV con nombres de fila y columna y convertirlo en un data frame
leerCSVConNombres <- function(archivoCSV) {
  # Leer el archivo CSV
  datos <- read.csv(archivoCSV, header = TRUE, row.names = 1)
  return(datos)
}

eliminar_filas_repetidas <- function(dataframe) {
  # Agrega una columna "FilaTexto" con la representación de cadena de cada fila
  dataframe$FilaTexto <- do.call(paste, c(dataframe, sep = "_"))
  
  # Encuentra las filas duplicadas y cuenta su frecuencia
  duplicados <- data.frame(table(dataframe$FilaTexto))
  
  # Agrega una columna "Repeticiones" al dataframe original con ceros por defecto
  dataframe$Repeticiones <- 0
  
  # Actualiza el valor de "Repeticiones" para las filas duplicadas
  for (i in 1:nrow(duplicados)) {
    fila_duplicada <- duplicados[i, ]
    fila_indices <- which(dataframe$FilaTexto == fila_duplicada$Var1)
    dataframe[fila_indices, "Repeticiones"] <- fila_duplicada$Freq - 1
  }
  
  # Elimina la columna "FilaTexto" y las filas duplicadas dejando solo una instancia de cada una
  dataframe <- dataframe[!duplicated(dataframe$FilaTexto), ]
  dataframe$FilaTexto <- NULL
  
  return(dataframe)
}

ReemplazarValores_0_1 <- function(dataframe, indice_columna, valor_cero, valor_uno) {
  # Validar que el índice de columna sea válido
  if (indice_columna < 1 || indice_columna > ncol(dataframe)) {
    stop("El índice de columna especificado no es válido.")
  }
  
  # Obtener el nombre de la columna
  nombre_columna <- colnames(dataframe)[indice_columna]
  
  # Reemplazar los valores 0 por valor_cero y los valores 1 por valor_uno
  dataframe_modificado <- dataframe %>%
    mutate(!!nombre_columna := ifelse(!!sym(nombre_columna) == 0, valor_cero, valor_uno))
  
  return(dataframe_modificado)
}

CrearGraficoSentimiento <- function(vectorSentimiento, titulo = "Análisis de Sentimiento") {
  # Crear un data frame con las sentencias y el sentimiento
  df <- data.frame(
    sentencias = 1:length(vectorSentimiento),
    sentimentos = rescale(vectorSentimiento)
  )
  
  # Calcular la línea de ajuste Loess y Rolling Mean
  df$loess <- loess(sentimentos ~ sentencias, data = df)$fitted
  df$rolling_mean <- zoo::rollmean(df$sentimentos, k = 3, fill = NA)
  
  # Crear el gráfico
  plot <- ggplot(df, aes(sentencias, sentimentos)) +
    geom_line(aes(color = "Sentimiento"), size = 0.8) +
    geom_line(aes(y = loess, color = "Loess (suavizado)"), size = 1) +
    geom_line(aes(y = rolling_mean, color = "Rolling Mean (media móvil)"), size = 1) +
    labs(title = titulo,
         x = "Sentencias",
         y = "Sentimiento") +
    scale_color_manual(values = c("Sentimiento" = "black", "Loess (suavizado)" = "blue", "Rolling Mean (media móvil)" = "red")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    theme_minimal() +
    theme(legend.position = "top") +
    guides(color = guide_legend(title = "Leyenda"))
  
  return(plot)
}

CrearDataFrameSentimientos <- function(vector_syuzhet, vector_bing, vector_afinn, vector_nrc) {
  # Crear un DataFrame con los vectores como filas
  df <- data.frame(
    syuzhet = vector_syuzhet,
    bing = vector_bing,
    afinn = vector_afinn,
    nrc = vector_nrc
  )
  
  # Transponer el DataFrame para obtener 4 filas y 10 columnas
  df <- t(df)
  
  # Crear un nuevo DataFrame con nombres de columna
  df <- data.frame(df)
  colnames(df) <- c("ira", "anticipación", "asco", "miedo", "alegría", "tristeza", "sorpresa", "confianza", "negativo", "positivo")
  
  # Definir los nombres de las filas
  rownames(df) <- c("syuzhet", "bing", "afinn", "nrc")
  
  return(df)
}
###Notas pueden generarse para guardar en archivo fisico como csv
# > dxM<-resultados$VectorTextoSentimientosNRC
# > dataR<-resultados$MatrizSimplificadaTokensValorados
# > dx<-dataR$MatrizTranspuestaSinDup
##> dxM<-as.matrix(dxM)
##> dxM<-as.data.frame(dxM)
##> dx<-as.matrix(dx)
##> dx<-as.data.frame(dx)
##> write_csv(dxM,"r4Met.csv")
##> > write_csv(dx,"rNRC.csv")
## Esta indicando que NRC solo toma por ejemplo 59 y los 4 Diccionarios 90 un 30% más de información 

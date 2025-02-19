# �ndice------------------------------------------------------------------------------------------------------------------------1
# Cargar librer�as necesarias--------------------------------------------------------------------------------------------------43
# Leer archivos .sav-----------------------------------------------------------------------------------------------------------55
# Tabla 1: Tarea Stroop--------------------------------------------------------------------------------------------------------61
#   Descriptivos de la muestra: edad-------------------------------------------------------------------------------------------66
#   Transformaci�n de la matriz a un formato largo para TR---------------------------------------------------------------------75
#     ANOVA 2 x 2 x 2 para TR--------------------------------------------------------------------------------------------------93
#   transformaci�n de la matriz a un formato largo para Precisi�n-------------------------------------------------------------102
#     Descriptivos para Precisi�n---------------------------------------------------------------------------------------------120
#     ANOVA 2 x 2 x 2 para Precisi�n------------------------------------------------------------------------------------------130
# Tabla 2: Tarea de Difusi�n de im�genes--------------------------------------------------------------------------------------144
# La VD se considera continua: difusi�n poca o mucha--------------------------------------------------------------------------156
#    A�adir columna con difusi�n total y difusi�n en funci�n de valencia afectiva---------------------------------------------158
#     Obtener descriptivos de Difusi�n para cada imagen de la tarea y graficarlos---------------------------------------------176
#    Pruebas de diferencia de medias entre Grupos para todas las im�genes-----------------------------------------------------220
#   Empieza el an�lisis de la Difusi�n de im�genes en general-----------------------------------------------------------------245
#     Correlaciones Grupo con difusi�n en general y en funci�n de la Valencia Afectiva----------------------------------------247
#     Transformaci�n Matriz en Formato Largo para VD seg�n Valencia Afectiva--------------------------------------------------274
#     Descriptivos y gr�fica de barras difusi�n seg�n Grupo y ValAFect--------------------------------------------------------289
#   An�lisis ANOVA para la Difusi�n-------------------------------------------------------------------------------------------359
#   ANOVA 2 (grupo) x 3 (uso habitual de redes sociales) x 3 (valencia afectiva) para la VD Difusi�n--------------------------365
#    Pruebas post hoc---------------------------------------------------------------------------------------------------------382
# H1. La prominencia de mortalidad aumenta la difusi�n de contenido en SNSs---------------------------------------------------388
# H2. La prominencia de mortalidad aumenta la difusi�n de contenido cuando el tiempo de uso diario de SNSs es bajo.-----------392
# H3. La MS facilita en mayor medida la difusi�n de contenido positivo, seguido del contenido negativo.-----------------------413
# La VD se considera discontinua: difusi�n privada vs p�blica-----------------------------------------------------------------419
#   ANOVA 2 (grupo) x 3 (valencia afectiva) x 3(v�a de difusi�n)--------------------------------------------------------------481
# Duda 1. �Por qu� el ANOVA no calcula la F y significatividad de los tres primeros factores?---------------------------------490
# H4. La MS aumenta la difusi�n de contenido negativo en la red privada m�s que en la p�blica.--------------------------------491
# Tabla 3: cuestionario Stroop------------------------------------------------------------------------------------------------529
# Calcular Alfa de Cronbach --------------------------------------------------------------------------------------------------535
# An�lisis factorial----------------------------------------------------------------------------------------------------------549
#   Correlaciones con la Autoestima-------------------------------------------------------------------------------------------622
# Regresi�n Lineal M�ltiple con mediaci�n-moderaci�n--------------------------------------------------------------------------697
#   modelo--------------------------------------------------------------------------------------------------------------------702
# Repetici�n de los an�lisis principales pero controlando nueva V "Horario"---------------------------------------------------724
#   ANOVA Tarea Stroop--------------------------------------------------------------------------------------------------------726
#     Post hocs---------------------------------------------------------------------------------------------------------------734
#   ANOVA tarea Difusi�n------------------------------------------------------------------------------------------------------747
#   ANOVA cuestionario Autoestima---------------------------------------------------------------------------------------------756


# Cargar librer�as necesarias
library(haven) # Para leer archivos .sav
library(tidyverse) # Para manipulaci�n de datos
library(afex) # Para an�lisis ANOVA de medidas repetidas
library(lavaan) # Para rlm con mediaci�n-moderaci�n
library(lm.beta) # Para regresi�n lineal
library(ggplot2) # Para visualizaci�n
library(dplyr) # Para manejar tablas mejor
library(psych) #Para el an�lisis factorial y prueba de fiabilidad  
library(emmeans) #Para pruebas post-hoc
library(Hmisc) #Para obtener la significatividad de los an�lisis correlacionales

# Leer archivos .sav
tabla1 <- read_sav("DatosTareaStroop.sav")
tabla2 <- read_sav("DatosTareaImagenes.sav")
tabla3 <- read_sav("DatosCuestionarioAutoestima.sav")


# ---------------- Tabla 1: Tarea Stroop ---------------- #

# Descriptivos de la muestra: edad
descriptivos_edad_muestra <- tabla1 %>% 
  summarise(
    Frecuencia = n(), 
    Media = mean(Edad, na.rm = TRUE), 
    Desviaci�n_T�pica = sd(Edad, na.rm = TRUE)
  )
print(descriptivos_edad_muestra)

#En R para hacer ANOVA hay que transformar la matriz a un formato largo
tabla1_larga_TR <- tabla1 %>%
  pivot_longer(
    cols = c(StnBluTR, StnRedTR, ObjBluTR, ObjRedTR),
    names_to = "Condicion",
    values_to = "TR"
  ) %>%
  mutate(
    TipoPalabra = case_when(
      str_detect(Condicion, "Stn") ~ "Est�ndar",
      str_detect(Condicion, "Obj") ~ "Objetivo"
    ),
    Color = case_when(
      str_detect(Condicion, "Blu") ~ "Blue",
      str_detect(Condicion, "Red") ~ "Red"
    )
  )

# ANOVA 2 (grupo) x 2 (tipo de palabra) x 2 (color) para el Tiempo de Respuesta
anova_stroopTR <- aov_car(
  TR ~ Grupo * Color * TipoPalabra + Error(Subject / (Color * TipoPalabra)),
  data = tabla1_larga_TR
)

# Mostrar los resultados
summary(anova_stroopTR)

# ANOVA 2 (grupo) x 2 (tipo de palabra) x 2 (color) para la Precisi�n de Respuesta
tabla1_larga_Precisi�n <- tabla1 %>%
  pivot_longer(
    cols = c(StnBluAE, StnRedAE, ObjBluAE, ObjRedAE),
    names_to = "Condicion",
    values_to = "Precisi�n"
  ) %>%
  mutate(
    TipoPalabra = case_when(
      str_detect(Condicion, "Stn") ~ "Est�ndar",
      str_detect(Condicion, "Obj") ~ "Objetivo"
    ),
    Color = case_when(
      str_detect(Condicion, "Blu") ~ "Blue",
      str_detect(Condicion, "Red") ~ "Red"
    )
  )

# Descriptivos para Precisi�n
descriptivos_Stroop_Precisi�n <- tabla1_larga_Precisi�n%>%
  group_by(Grupo, TipoPalabra) %>% # Agrupar por niveles de Grupo y Tipo_de_palabra
  summarise(
    Frecuencia = n(), 
    Media = mean(Precisi�n, na.rm = TRUE), 
    Desviaci�n_T�pica = sd(Precisi�n, na.rm = TRUE)
  )
print(descriptivos_Stroop_Precisi�n)

# ANOVA para Precisi�n
anova_stroopAE <- aov_car(
  Precisi�n ~ Grupo * Color * TipoPalabra + Error(Subject / (Color * TipoPalabra)),
  data = tabla1_larga_Precisi�n
)

# Mostrar los resultados
summary(anova_stroopAE)

#Prueba posthoc para las interacciones que resultasen significativas
posthoc<- emmeans(anova_stroopAE, pairwise ~ Grupo:Color)
print(posthoc)


# ---------------- Tabla 2: Tarea de Difusi�n de im�genes ---------------- #

# Crear columna PercentilUsoSNS
tabla2 <- tabla2 %>%
  mutate(
    PercentilUsoSNS = case_when(
      UsoSNSs <= quantile(UsoSNSs, 0.33, na.rm = TRUE) ~ 1,
      UsoSNSs <= quantile(UsoSNSs, 0.66, na.rm = TRUE) ~ 2,
      TRUE ~ 3
    )
  )

#La VD se considera continua: difusi�n poca o mucha

#A�adir columna con difusi�n total general
columnas_imagenes <- paste0("@", c("1710", "1920", "2057", "2550", "5470", "5621", "5760", 
                                   "5830", "7502", "8080", "2095", "2683", "2688", "6243", 
                                   "7380", "9340", "9432", "9520", "9621", "9630", "2397", 
                                   "2570", "2702", "2850", "2880", "5740", "5920", "7037", 
                                   "7590", "8232"), ".jpg") 

# Discriminar las columnas de imagenes seg�n su Val.Afect.
columnas_imagen_positiva <- paste0("@", c("1710", "1920", "2057", "2550", "5470", "5621", "5760", "5830", "7502", "8080"),".jpg")
columnas_imagen_negativa <- paste0("@", c("2095","2683","2688","6243","7380","9340","9432", "9520", "9621", "9630"),".jpg")
columnas_imagen_neutral <- paste0("@", c("2397", "2570", "2702", "2850", "2880", "5740", "5920", "7037", "7590", "8232"),".jpg")

#A�ade columnas con la difusi�n total general para cada categor�a de im�genes seg�n valencia afectiva
tabla2$Difusi�nTotal <- rowSums(tabla2[, columnas_imagenes], na.rm = TRUE)
tabla2$Difusi�ntotalImagenesNegativas <- rowSums(tabla2[, columnas_imagen_negativa], na.rm = TRUE)
tabla2$Difusi�ntotalImagenesNeutrales <- rowSums(tabla2[, columnas_imagen_neutral], na.rm = TRUE)
tabla2$Difusi�ntotalImagenesPositivas <- rowSums(tabla2[, columnas_imagen_positiva], na.rm = TRUE)

# Obtener descriptivos de Difusi�n para cada imagen de la tarea.
resumen <- tabla2 %>%
  select(all_of(columnas_imagenes)) %>% # Seleccionar solo las columnas de inter�s
  summarise(across(everything(), list(
    media = ~mean(.x, na.rm = TRUE),
    desviacion = ~sd(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Imagen", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    ValenciaAfectiva = case_when(
      Imagen %in% columnas_imagen_negativa ~ "Negativa",
      Imagen %in% columnas_imagen_neutral ~ "Neutra",
      Imagen %in% columnas_imagen_positiva ~ "Positiva",
      TRUE ~ NA_character_ # Por seguridad
    )
  )

# Verificar el resultado del resumen
print(resumen)

# Ordenar las im�genes por ValenciaAfectiva
resumen <- resumen %>%
  mutate(
    ValenciaAfectiva = factor(ValenciaAfectiva, levels = c("Negativa", "Neutra", "Positiva")),
    Imagen = reorder(Imagen, as.numeric(ValenciaAfectiva)) # Reordenar las im�genes por valencia afectiva
  )

# Gr�fica de Difusi�n de las im�genes
ggplot(resumen, aes(x = Imagen, y = media, fill = ValenciaAfectiva)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Medias de Difusi�n por Imagen",
    x = "Im�genes agrupadas por Valencia Afectiva",
    y = "Media de Difusi�n",
    fill = "Valencia Afectiva"
  ) +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45)) + # Rotar etiquetas del eje x
  theme(legend.position = "top")

# Pruebas de diferencia de medias entre Grupos para cada imagen
resultados <- lapply(columnas_imagenes, function(col) {
  # Realizar prueba t independiente para cada columna entre Grupo 1 y Grupo 2
  t_test <- t.test(tabla2[[col]] ~ tabla2$Grupo, data = tabla2)
  
# Retornar los resultados como una lista
  data.frame(
    Imagen = col,
    Media_Grupo1 = mean(tabla2[[col]][tabla2$Grupo == 1], na.rm = TRUE),
    Media_Grupo2 = mean(tabla2[[col]][tabla2$Grupo == 2], na.rm = TRUE),
    Diferencia = t_test$estimate[1] - t_test$estimate[2],
    p_value = t_test$p.value
  )
})

# Combinar los resultados en un data frame
resultados_df <- do.call(rbind, resultados)

# Filtrar solo las diferencias significativas
resultados_significativos <- resultados_df %>%
  filter(p_value < 0.05)

# Mostrar los resultados significativos
print(resultados_significativos)

# Empieza el an�lisis de la Difusi�n de im�genes en general

#Correlaciones para ver si el Grupo se asocia a mayor difusi�n general y/o en funci�n de la Valencia Afectiva
 
variables_correlaci�n_tabla2 <- tabla2 %>%
  select(Grupo, Difusi�nTotal, Difusi�ntotalImagenesNegativas, Difusi�ntotalImagenesNeutrales, Difusi�ntotalImagenesPositivas)

cor_spearman_tabla2_VDcomoContinua <- rcorr(as.matrix(variables_correlaci�n_tabla2), type = "spearman")

# Crea funci�n para etiquetar p-valores con asteriscos
asterisks <- function(p) {
  ifelse(p <= 0.001, "***",
         ifelse(p <= 0.01, "**",
                ifelse(p <= 0.05, "*", "")))
}

# Aplicar la funci�n a la matriz actual
p_values_with_asterisks <- apply(cor_spearman_tabla2_VDcomoContinua$P, c(1, 2), asterisks)

# Combinar los r-valores con los asteriscos de la significatividad correspondiente
cor_with_asterisks <- matrix(
  paste0(round(cor_spearman_tabla2_VDcomoContinua$r, 3), " ", p_values_with_asterisks),
  nrow = nrow(cor_spearman_tabla2_VDcomoContinua$r),
  ncol = ncol(cor_spearman_tabla2_VDcomoContinua$r),
  dimnames = dimnames(cor_spearman_tabla2_VDcomoContinua$r)
)
# Mostrar la matriz combinada
print(cor_with_asterisks) #El Grupo no correlaciona con ning�n tipo de difusi�n

#Transformaci�n Matriz en Formato Largo para VD seg�n Valencia Afectiva
tabla2_larga_VDcomoContinua <- tabla2%>%
  pivot_longer(
    cols = c(Difusi�ntotalImagenesNegativas, Difusi�ntotalImagenesNeutrales, Difusi�ntotalImagenesPositivas),
    names_to = "ValAfect",
    values_to = "Difusi�nSeg�nValenciaAfectivaVDcomoContinua"
  ) %>%
  mutate(
    ValAfect = case_when(
      str_detect(ValAfect, "ImagenesNegativas") ~ "Negativa",
      str_detect(ValAfect, "ImagenesNeutrales") ~ "Neutral",
      str_detect(ValAfect, "ImagenesPositivas") ~ "Positiva"
    )
  )

#Descriptivos segun ValAFect
descriptivos_Im�genesVDcomoContinua <- tabla2_larga_VDcomoContinua %>%
  group_by(ValAfect) %>% # Agrupar por niveles de Grupo y Tipo_de_palabra
  summarise(
    Frecuencia = n(),   
    Media = mean(Difusi�nSeg�nValenciaAfectivaVDcomoContinua, na.rm = TRUE), 
    Desviaci�n_T�pica = sd(Difusi�nSeg�nValenciaAfectivaVDcomoContinua, na.rm = TRUE)
  )
# Mostrar el resultado
print(descriptivos_Im�genesVDcomoContinua)

#Gr�fica comparando Difusi�n de los dos grupos seg�n ValAfect

# Ordenar las variables ValAfect y Grupo
tabla2_larga_VDcomoContinua <- tabla2_larga_VDcomoContinua %>%
  mutate(
    Ordenado = factor(
      paste0("Grupo ", Grupo, " Valencia Afectiva ", ValAfect),
      levels = c(
        "Grupo 1 Valencia Afectiva 1",
        "Grupo 2 Valencia Afectiva 1",
        "Grupo 1 Valencia Afectiva 2",
        "Grupo 2 Valencia Afectiva 2",
        "Grupo 1 Valencia Afectiva 3",
        "Grupo 2 Valencia Afectiva 3"
                )
          )

# Crear la gr�fica de barras

# Crear la columna de interacci�n y calcular la media para cada combinaci�n
resumen <- tabla2_larga_VDcomoContinua %>%
     group_by(Grupo, ValAfect) %>%
     summarise(Media_Difusi�n = mean(Difusi�nSeg�nValenciaAfectivaVDcomoContinua, na.rm = TRUE)) %>%
     ungroup() %>%
     mutate(
        # Ajustar la combinaci�n de texto para que coincida con los niveles
        Interacci�n = factor(
          case_when(
            ValAfect == "Negativa" & Grupo == 1 ~ "Negativas Grupo 1",
            ValAfect == "Negativa" & Grupo == 2 ~ "Negativas Grupo 2",
            ValAfect == "Neutral" & Grupo == 1 ~ "Neutras Grupo 1",
            ValAfect == "Neutral" & Grupo == 2 ~ "Neutras Grupo 2",
            ValAfect == "Positiva" & Grupo == 1 ~ "Positivas Grupo 1",
            ValAfect == "Positiva" & Grupo == 2 ~ "Positivas Grupo 2",
            TRUE ~ NA_character_
          ),
          levels = c(
            "Negativas Grupo 1",
            "Negativas Grupo 2",
            "Neutras Grupo 1",
            "Neutras Grupo 2",
            "Positivas Grupo 1",
            "Positivas Grupo 2"
          )
        )
      )
    
    # Crear la gr�fica de barras
    ggplot(resumen, aes(x = Interacci�n, y = Media_Difusi�n, fill = factor(Grupo))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Gr�fica de Barras: Difusi�n seg�n Grupo y Valencia Afectiva",
        x = "Grupo y Valencia Afectiva",
        y = "Media de Difusi�n",
        fill = "Grupo"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas en eje x

# An�lisis ANOVA
  
#Se requiere transformar las variables categ�ricas a factores para realizar el ANOVA
tabla2_larga_VDcomoContinua$Grupo <- as.factor(tabla2_larga_VDcomoContinua$Grupo)
tabla2_larga_VDcomoContinua$PercentilUsoSNS <- as.factor(tabla2_larga_VDcomoContinua$PercentilUsoSNS)

# ANOVA 2 (grupo) x 3 (uso habitual de redes sociales) x 3 (valencia afectiva) para la VD Difusi�n en general
anova_imagenes_VDcomoContinua <- aov_car(
  Difusi�nSeg�nValenciaAfectivaVDcomoContinua ~ Grupo * PercentilUsoSNS * ValAfect + Error(Subject / ValAfect),
  data = tabla2_larga_VDcomoContinua
)
summary(anova_imagenes_VDcomoContinua) 
# Se observa en el Mauchly Tests la significatividad y rechazo del supuesto de no esfericidad, se recurre a Greenhouse-Geisser

anova_imagenes_VDcomoContinua <- aov_car(
  Difusi�nSeg�nValenciaAfectivaVDcomoContinua ~ Grupo * PercentilUsoSNS * ValAfect + Error(Subject / ValAfect),
  data = tabla2_larga_VDcomoContinua,
  anova_table = list(correction = "GG", es = "ges", full = TRUE) 
)
resultados_anova_imagenes_VDcomoContinua_conCorrecci�nGG<- anova_imagenes_VDcomoContinua$anova_table
print(resultados_anova_imagenes_VDcomoContinua_conCorrecci�nGG)


# Pruebas post hoc para los nivel de interacci�n de variables significativos y aquellos demandados por las hip�tesis. 

# Aunque si ya no sali� una interacci�n significativa en el ANOVA sobrar�a mirar los post hoc
posthoc<- emmeans(anova_imagenes_VDcomoContinua, pairwise ~ ValAfect)
print(posthoc) #Se comprueba en general cu�les im�genes se difunde m�s

# H1. La prominencia de mortalidad aumenta la difusi�n de contenido en SNSs,
posthoc<- emmeans(anova_imagenes_VDcomoContinua, pairwise ~ Grupo)
print(posthoc)  #Rechazo H1
  
# H2. La prominencia de mortalidad aumenta la difusi�n de contenido cuando el tiempo de uso diario de SNSs es bajo.
em <- emmeans(anova_imagenes_VDcomoContinua, ~ Grupo | PercentilUsoSNS)
comparison_val3 <- contrast(em, method = "pairwise", by = "PercentilUsoSNS")
print(comparison_val3) # Rechazo H2
# M�s en detalle (tambi�n seg�n Valencia Afectiva)
em <- emmeans(anova_imagenes_VDcomoContinua, ~  PercentilUsoSNS | Grupo | ValAfect)
em_subset <- emmeans(em, ~  Grupo | PercentilUsoSNS| ValAfect)
contrast_result <- contrast(em_subset, method = "pairwise", adjust = "bonferroni")
print(contrast_result)
# El uso m�s habitual de redes sociales no influy� en la difusi�n, tampoco en interacci�n con el Grupo (seg�n se indica en el ANOVA), sin embargo
# en el post hoc s� que se encuentra una diferencia significativa entre grupos, �nicamente para los usuarios con un tiempo de Uso de SNS promedio en la difusi�n
# de im�genes negativas y positivas (difundiendo m�s el grupo 1 que 2)

# Quer�a aqu� ver el tama�o muestral para tal internivel de variables (sujetos grupo experimental y UsoSNS == 2)
descriptivos_n_seg�n_usoSNS <- tabla2_larga_VDcomoContinua %>%
  group_by(PercentilUsoSNS, Grupo, ValAfect) %>% # Ver n de UsoSNS
  summarise(
    Frecuencia = n(), 
  )
print(descriptivos_n_seg�n_usoSNS) 

# H3. La MS facilita en mayor medida la difusi�n de contenido positivo, seguido del contenido negativo.
em <- emmeans(anova_imagenes_VDcomoContinua, ~ Grupo | ValAfect)
comparison_val3 <- contrast(em, method = "pairwise", by = "ValAfect")
print(comparison_val3) # Rechazo H3


# Mismos an�lisis pero ahora
# La VD se considera discontinua: difusi�n privada vs p�blica

# Se crean nuevas columnas con el recuento de im�genes difundidas seg�n cada v�a de difusi�n
tabla2 <- tabla2 %>%
  rowwise() %>%
  mutate(
    Difusi�nTotalV�aNinguna = sum(c_across(all_of(columnas_imagenes)) == 0, na.rm = TRUE),
    Difusi�nTotalV�aPrivada = sum(c_across(all_of(columnas_imagenes)) == 1, na.rm = TRUE),
    Difusi�nTotalV�aPublica = sum(c_across(all_of(columnas_imagenes)) == 2, na.rm = TRUE)
  ) %>%
  ungroup()

# Nuevas columnas con el registro de respuesta seg�n Val.Afect. y v�a de difusi�n
tabla2 <- tabla2 %>%
  rowwise() %>%
  mutate(
    Difusi�nImagenesNegativasV�aNinguna = sum(c_across(all_of(columnas_imagen_negativa)) == 0, na.rm = TRUE),
    Difusi�nImagenesNegativasV�aPrivada = sum(c_across(all_of(columnas_imagen_negativa)) == 1, na.rm = TRUE),
    Difusi�nImagenesNegativasV�aPublica = sum(c_across(all_of(columnas_imagen_negativa)) == 2, na.rm = TRUE),
    Difusi�nImagenesNeutralesV�aNinguna = sum(c_across(all_of(columnas_imagen_neutral)) == 0, na.rm = TRUE),
    Difusi�nImagenesNeutralesV�aPrivada = sum(c_across(all_of(columnas_imagen_neutral)) == 1, na.rm = TRUE),
    Difusi�nImagenesNeutralesV�aPublica = sum(c_across(all_of(columnas_imagen_neutral)) == 2, na.rm = TRUE),
    Difusi�nImagenesPositivasV�aNinguna = sum(c_across(all_of(columnas_imagen_positiva)) == 0, na.rm = TRUE),
    Difusi�nImagenesPositivasV�aPrivada = sum(c_across(all_of(columnas_imagen_positiva)) == 1, na.rm = TRUE),
    Difusi�nImagenesPositivasV�aPublica = sum(c_across(all_of(columnas_imagen_positiva)) == 2, na.rm = TRUE),
  ) %>%
  ungroup()

#Transformaci�n Matriz en Formato Largo para VD seg�n Val.Afectiva y V�adeDifusi�n

tabla2_larga_VDcomoDiscontinua <- tabla2%>%
  pivot_longer(
    cols = c(Difusi�nImagenesNegativasV�aNinguna, Difusi�nImagenesNegativasV�aPrivada, Difusi�nImagenesNegativasV�aPublica, 
             Difusi�nImagenesNeutralesV�aNinguna, Difusi�nImagenesNeutralesV�aPrivada, Difusi�nImagenesNeutralesV�aPublica, 
             Difusi�nImagenesPositivasV�aNinguna, Difusi�nImagenesPositivasV�aPrivada, Difusi�nImagenesPositivasV�aPublica),
    names_to = "Condicion",
    values_to = "Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n"
  ) %>%
  mutate(
    ValAfect = case_when(
      str_detect(Condicion, "ImagenesNegativas") ~ "ImagenesNegativas",
      str_detect(Condicion, "ImagenesNeutrales") ~ "ImagenesNeutras",
      str_detect(Condicion, "ImagenesPositivas") ~ "ImagenesPositivas",
    ),
      ViaDif = case_when(
        str_detect(Condicion, "V�aNinguna") ~ "DifundidasV�aNinguna",
        str_detect(Condicion, "V�aPrivada") ~ "DifundidasV�aPrivada",
        str_detect(Condicion, "V�aPublica") ~ "DifundidasV�aPublica"
    )
  )

# Frecuencia de respuesta de difusi�n seg�n V�a de difusi�n
descriptivos_Im�genesVDcomoDiscontinua <- tabla2_larga_VDcomoDiscontinua %>%
  group_by(ViaDif, ValAfect) %>% 
  summarise(Frecuencia = n(),   
      Media = mean(Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n, na.rm = TRUE), 
      Desviaci�n_T�pica = sd(Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n, na.rm = TRUE)
            )
# Mostrar el resultado
print(descriptivos_Im�genesVDcomoDiscontinua)

# ANOVA 2 (grupo) x 3 (valencia afectiva) x 3(v�a de difusi�n) considerando la no esfericidad (correcci�n Greenhouse-Geisser)
anova_imagenes_VDcomoDiscontinua <- aov_car(
  Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n ~ Grupo * ValAfect * ViaDif + Error(Subject / (ValAfect * ViaDif)),
  data = tabla2_larga_VDcomoDiscontinua,
  anova_table = list(correction = "GG", es = "ges", full = TRUE) 
)
resultados_anova_imagenes_Discontinua_conCorrecci�nGG<- anova_imagenes_VDcomoDiscontinua$anova_table
print(resultados_anova_imagenes_Discontinua_conCorrecci�nGG) 

#Pruebas post hoc para los niveles mencionados en la hip�tesis
# H4. La MS aumenta la difusi�n de contenido negativo en la red privada m�s que en la p�blica.
em <- emmeans(anova_imagenes_VDcomoDiscontinua, ~ Grupo | ValAfect * ViaDif)
comparacion <- contrast(em, method = "pairwise", by = c("ValAfect", "ViaDif"), at = list(ValAfect = 1, ViaDif = 2))
print(comparacion)  # Rechazo H4

# Calcular percentiles de la frecuencia
tabla2_larga_VDcomoDiscontinua <- tabla2_larga_VDcomoDiscontinua %>%
  group_by(ValAfect) %>%
  mutate(
    PercentilFrecuencia = ntile(Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n, 4)  # Divide en 4 percentiles
  )

# Calcular media de frecuencia de respuesta
tabla2_medias <- tabla2_larga_VDcomoDiscontinua %>%
  group_by(ViaDif, ValAfect) %>%
  summarise(
    MediaDifusion = mean(Difusi�nSeg�nValenciaAfectivaYV�aDifusi�n, na.rm = TRUE),
    PercentilFrecuencia = ntile(MediaDifusion, 4)  # Calcular percentiles de la media
  )

#Gr�fico distribuci�n de respuestas
ggplot(tabla2_medias, 
       aes(x = ViaDif, 
           y = MediaDifusion, 
           color = ValAfect, 
           group = ValAfect)) +  # Agrupa por Categor�a Afectiva
  geom_point(alpha = 1) +       # A�ade puntos
  geom_line(size = 1) +         # A�ade l�neas que conectan los puntos
  labs(
    title = "Media de Difusi�n seg�n Categor�a Afectiva y Tipo de Difusi�n",
    x = "Tipo de Difusi�n",
    y = "Frecuencia de cada Respuesta de Difusi�n",
    color = "Categor�a Afectiva"
  ) +
  theme_minimal() +             # Tema limpio
  scale_x_discrete()            # Eje x discreto


# ---------------- Tabla 3: cuestionario Stroop ---------------- #

# Matriz de correlaciones �tems cuestionario
tabla_items_cuestionario <-tabla3 %>%
  select(6:19)

# Calcular Alfa de Cronbach para el cuestionario
alphaCuestionarioAutoestima <- psych::alpha(tabla_items_cuestionario)
print(alphaCuestionarioAutoestima)  #El alfa de Cronbach del cuestionario da igual que en SPSS
# Tan s�lo se puede mejorar la fiabilidad en un 0.01 tras eliminar el �tem Ident1, por lo que opto por conservar la estructura original

# Se comprueba si los datos son adecuados para en an�lisis factorial
# Prueba de KMO
kmo_result <- KMO(tabla_items_cuestionario)
print(kmo_result)

# Prueba de esfericidad de Bartlett
bartlett_result <- cortest.bartlett(cor(tabla_items_cuestionario), n = nrow(tabla_items_cuestionario))
print(bartlett_result)

# An�lisis factorial confirmatorio de la estructura del cuestionario original: con m�xima verosimilitud y Rotaci�n Oblimin
matriz_corr <- cor(scale(tabla_items_cuestionario))

resultado_afc <- fa(
  r = matriz_corr,
  nfactors = 4,
  rotate = "oblimin",
  fm = "ml"
)

# Comprobar ajuste del modelo y viabilidad de la reducci�n factorial
# Modelo CFA basado en tus factores e �tems
modelo <- '
Factor1 =~ Priv1 + Priv2 + Priv3
Factor2 =~ Public1 + Public2 + Public3 + Public4
Factor3 =~ Ident1 + Ident2 + Ident3 + Ident4
Factor4 =~ Memb1 + Memb2 + Memb3
'

# Ajustar el modelo CFA a los datos
fit <- cfa(modelo, data = tabla_items_cuestionario)
# Resumen con �ndices de ajuste
summary(fit, fit.measures = TRUE, standardized = TRUE) # valores de los �ndices CFI, TLI, RMSEA, SRMR inadecuados

# Se eliminan los �tems problem�ticos (fueron err�neamente transcritos) y se realiza un EFA con tantos factores como se estimen a partir de un autovalor > 1
tabla_items_cuestionario <- tabla_items_cuestionario %>% select(-c(10, 12))
matriz_corr <- cor(scale(tabla_items_cuestionario))

# PCA inicial sin rotaci�n para obtener los autovalores iniciales
pca_sin_rotacion <- principal(matriz_corr, nfactors = ncol(tabla_items_cuestionario), rotate = "none")

# Obtener los autovalores iniciales y varianza explicada
autovalores <- pca_sin_rotacion$values
varianza_explicada <- autovalores / sum(autovalores) * 100
varianza_acumulada <- cumsum(varianza_explicada)

# Crear tabla de varianza total explicada
tabla_varianza <- data.frame(
  Componente = paste("Componente", 1:length(autovalores)),
  Autovalor = autovalores,
  Varianza_Explicada = varianza_explicada,
  Varianza_Acumulada = varianza_acumulada
)

# Mostrar la tabla
print(tabla_varianza)

# PCA con rotaci�n Oblimin (si es necesaria)
pca_con_rotacion <- principal(matriz_corr, nfactors = 4, rotate = "oblimin")

# Mostrar resultados tras la rotaci�n
cat("\nCargas factoriales tras la rotaci�n Oblimin:\n")
print(pca_con_rotacion$loadings, cutoff = 0.32)

# Crear nuevas columnas con las puntuaciones factoriales y punt. total
puntuaciones_factoriales <- factor.scores(tabla_items_cuestionario, pca_con_rotacion, method = "regression")$scores

tabla3 <- cbind(tabla3, puntuaciones_factoriales)

tabla3$AutoestimaTotal <- rowSums(puntuaciones_factoriales) 

#A�adir columnas con VDs de tareas anteriores
tabla3 <- tabla3 %>%
  left_join(select(tabla2, Subject, PercentilUsoSNS), by = "Subject")
tabla3 <- tabla3 %>%
  left_join(select(tabla2, Subject, Difusi�nTotal), by = "Subject")
tabla3 <- tabla3 %>%
  left_join(select(tabla2, Subject, Difusi�ntotalImagenesNegativas), by = "Subject")
tabla3 <- tabla3 %>%
  left_join(select(tabla2, Subject, Difusi�ntotalImagenesNeutrales), by = "Subject")
tabla3 <- tabla3 %>%
  left_join(select(tabla2, Subject, Difusi�ntotalImagenesPositivas), by = "Subject")

#Correlaciones con la Autoestima
variables_correlaci�n_tabla3 <- tabla3 %>%
  select(Grupo, Difusi�nTotal,  AutoestimaTotal, Difusi�ntotalImagenesNeutrales, Difusi�ntotalImagenesNegativas ,Difusi�ntotalImagenesPositivas, PercentilUsoSNS)

cor_spearman_tabla3 <- rcorr(as.matrix(variables_correlaci�n_tabla3), type = "spearman")

# Aplicar la funci�n a la matriz actual
p_values_with_asterisks <- apply(cor_spearman_tabla3$P, c(1, 2), asterisks)

# Combinar los r-valores con los asteriscos de la significatividad correspondiente
cor_with_asterisks_tabla3 <- matrix(
  paste0(round(cor_spearman_tabla3$r, 3), " ", p_values_with_asterisks),
  nrow = nrow(cor_spearman_tabla3$r),
  ncol = ncol(cor_spearman_tabla3$r),
  dimnames = dimnames(cor_spearman_tabla3$r)
)
# Mostrar la matriz combinada
print(cor_with_asterisks_tabla3) # No se observa correlaci�n significativa de las VIs con la Autoestima

media_total <- mean(tabla3$AutoestimaTotal, na.rm = TRUE)
sd_total <- sd(tabla3$AutoestimaTotal, na.rm = TRUE)

# Estandarizar la puntuaci�n total
tabla3$AutoestimaTotal_estandarizada <- 
  (tabla3$AutoestimaTotal - media_total) / sd_total

# Verificar que la puntuaci�n estandarizada tiene media 0 y desviaci�n t�pica 1
media_estandarizada <- mean(tabla3$AutoestimaTotal_estandarizada, na.rm = TRUE)
sd_estandarizada <- sd(tabla3$AutoestimaTotal_estandarizada, na.rm = TRUE)
cat("Media de AutoestimaTotal estandarizada:", media_estandarizada, "\n")  
cat("Desviaci�n t�pica de AutoestimaTotal estandarizada:", sd_estandarizada, "\n")

# Mostrar descriptivos Autoestima por Grupo
descriptivos_autoestima <- tabla3 %>%
  group_by(Grupo) %>%
  summarise(
    Media = mean(AutoestimaTotal, na.rm = TRUE),
    Desviacion_Tipica = sd(AutoestimaTotal, na.rm = TRUE)
  )
# Mostrar la tabla resumen
print(descriptivos_autoestima)

# Realizar prueba t seg�n Grupo Exp para cada factor del cuestionario, as� como para la AutoestimaTotal
resultados_t <- lapply(c("TC1", "TC2", "TC3", "TC4", "AutoestimaTotal"), function(col) {
 t_test_autoestima <- t.test(tabla3[[col]] ~ tabla3$Grupo, data= tabla3)
})

print(resultados_t) # Resulta casi significativo el cuarto factor: Membres�a

# ANOVA Autoestima ~ 2 x 3 x 3  

# Crear columna PercentilDifusi�n
tabla3 <- tabla3 %>%
  mutate(
    PercentilDifusion = case_when(
      Difusi�nTotal <= quantile(Difusi�nTotal, 0.33, na.rm = TRUE) ~ 1,
      Difusi�nTotal <= quantile(Difusi�nTotal, 0.66, na.rm = TRUE) ~ 2,
      TRUE ~ 3
    )
  )

anova_Autoestima <- aov_car(
  AutoestimaTotal ~ Grupo * PercentilUsoSNS  * PercentilDifusion  + Error(Subject),
  data = tabla3,
  anova_table = list(correction = "GG", es = "ges", full = TRUE) 
)
resultados_anova_autoestima<- anova_Autoestima$anova_table
print(resultados_anova_autoestima) 

# Significativa la diferencia de la autoestima entre grupos en un grado de difusi�n bajo
em <- emmeans(anova_Autoestima, ~ Grupo | PercentilDifusion)
comparacion <- contrast(em, method = "pairwise", by = c("PercentilDifusion"), adjust = "bonferroni")
print(comparacion) 


#--- Regresi�n lineal m�ltiple de la Autoestima con mediaci�n de la Difusi�n y moderaci�n del Grupo --#

tabla3$Difusion_Grupo <- tabla3$Grupo * tabla3$Difusi�nTotal
tabla3$Difusion <- tabla3$Difusi�nTotal # El modelo de RLM no acepta tildes

# Definir el modelo de mediaci�n moderada
modelo_74 <- '
  # Ecuaci�n para la mediadora
  Difusion ~ a * Grupo

  # Ecuaci�n para la VD
  AutoestimaTotal ~ b * Grupo + c * Difusion + d * Difusion_Grupo

  # Efecto indirecto
  indirecto := a * c
'

# Ajustar el modelo
fit_74 <- sem(modelo_74, data = tabla3)

# Resumen de resultados
summary(fit_74, standardized = TRUE, rsquare = TRUE)

fitMeasures(fit_74, c("cfi", "tli", "rmsea", "srmr")) 
# Estos �ndices se�alan el ajuste inadecuado de los datos para la RLM


#----- Repetici�n de los an�lisis principales pero controlando la influencia de la variable Horario -----#

#ANOVA Tarea Stroop Precisi�n
control_anova_stroopAE <- aov_car(
  Precisi�n ~ Grupo * Horario * Color * TipoPalabra + Error(Subject / (Color * TipoPalabra)),
  data = tabla1_larga_Precisi�n
)
# Mostrar los resultados
summary(control_anova_stroopAE) 

em <- emmeans(control_anova_stroopAE, ~ Grupo | Horario)
comparacion <- contrast(em, method = "pairwise", by = c("Horario"),)
print(comparacion) # Entre los participantes de la ma�ana el Grupo 2 mostr� mayor precisi�n

# ANOVA tarea Stroop para el Tiempo de Respuesta
control_anova_stroopTR <- aov_car(
  TR ~ Grupo * Horario * Color * TipoPalabra + Error(Subject / (Color * TipoPalabra)),
  data = tabla1_larga_TR
)
# Mostrar los resultados
summary(control_anova_stroopTR) 
# Nada nuevo o interesante, los posthoc interesar�an si fuera significativo Grupo:Horario:TipoPalabra

#ANOVA tarea Difusi�n im�genes
control_anova_imagenes_VDcomoContinua <- aov_car(
  Difusi�nSeg�nValenciaAfectivaVDcomoContinua ~ Grupo * Horario  * PercentilUsoSNS * ValAfect + Error(Subject / ValAfect),
  data = tabla2_larga_VDcomoContinua,
  anova_table = list(correction = "GG", es = "ges", full = TRUE) 
)
control_resultados_anova_imagenes_VDcomoContinua_conCorrecci�nGG<- control_anova_imagenes_VDcomoContinua$anova_table
print(control_resultados_anova_imagenes_VDcomoContinua_conCorrecci�nGG) # Nada interesante

# ANOVA cuestionario Autoestima
control_anova_Autoestima <- aov_car(
  AutoestimaTotal ~ Grupo  * Horario * PercentilDifusion  + Error(Subject),
  data = tabla3,
  anova_table = list(correction = "GG", es = "ges", full = TRUE) 
)
control_resultados_anova_autoestima<- control_anova_Autoestima$anova_table
print(control_resultados_anova_autoestima)

# Post hoc Autoestima seg�n Grupo
em <- emmeans(control_anova_Autoestima, ~ Grupo)
comparacion <- contrast(em, method = "pairwise",)
print(comparacion) 
# Post hoc Autoestima seg�n Grupo:Horario
em <- emmeans(control_anova_Autoestima, ~ Grupo | Horario)
comparacion <- contrast(em, method = "pairwise", by = c("Horario"),)
print(comparacion) 

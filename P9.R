library(readxl)
library(visdat)
library(dplyr)
library(tidyverse)
library(lubridate)
library(forecast)
# imporation 

library(readxl)
calcul_DJU_14_11_2021 <- read_excel("calcul_DJU_14_11_2021.xlsx")

meteo <- calcul_DJU_14_11_2021[11:24,]
colnames(meteo) <- meteo[1,]
meteo <- meteo[2:14,]
colnames(meteo)[1] <- "year"
colnames(meteo) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12","total")

# On veut des variables numériques 
# df[cols] <- lapply(df[cols], function(x) as.numeric(as.character(x)))

meteo[2:14] <- lapply(meteo[2:14],function(x) as.numeric(as.character(x)))

# On supprime 2009,2010,2011 Notre df commence en 2012
meteo <- meteo[1:10,]

# Pivot_longer

M1 <- select(meteo,-total)

M1 <- pivot_longer(M1,-year)

# df %<>%
# unite(ColAandB, colA, colB, remove = FALSE)

M1 <- M1 %>% unite(mois,year,name,sep = "-")

# as.Date(M1$mois, format="%Y-%m")

# mutate(M1, mois=as.Date.character(mois, format = "%Y/%m"))

# meteo[1,8:13] <- NA
# meteo <- na.omit(meteo)

# lubridate 

M1 <- mutate(M1, mois=ym(mois)) # Format date 
M1 <- filter(M1, value !="0") # on suprrime les valeurs nulles qui sont  des NA 

ym(M1$mois)
df <- mutate(df, mois= ym(Mois) )

#MISSING DATA 

# Visualisation 
vis_dat(eCO2mix_RTE_energie_M)

df <- eCO2mix_RTE_energie_M[,1:15] # on exclut les données NA == commerce internationale 

vis_miss(df) # 34.86 % de donnée manquante pour le Nucléaire == principale source d'électricité

df <- df[,c(1,2,3,14)] %>% filter(Territoire=="France") 


df <- mutate(df, mois= ym(Mois) )



# Visualisation 
ggplot(M1) +
  aes(x=mois,y=value) +
  geom_line(color="peru") +
  ggtitle("Météo : Besoin en Chauffage ") +
  xlab("") +
  ylab("chauffage : écart de température ")


# Consommation Electrique en France 2012-2022
ggplot(df) +
  aes(x=mois,y=Consommation.totale) +
  geom_line(color="steelblue3") +
  ggtitle("Consommation Électrique en France") +
  ylab("Consommation totale (GWh)") +
  xlab('') 
  





# Jointure des deux data frames 

df2 <- merge(M1,df) %>% select(mois,value,Consommation.totale)
head(df2)

# df2 <- group_by(df2,month = month(mois)) 

# Regression linéaire ------

mod1 <- lm(log(Consommation.totale) ~ value , data = df2)
summary(mod1)
mod1$coefficients


par(mfrow=c(2,2))
plot(mod1)





# mod2 <- lm(formula = BoxCox(Consommation.totale) ~ BoxCox(value), data = df2)

mod3 <-  lm(Consommation.totale ~ value  , data = df2)
  
summary(mod3)  
  
# Regression Linéaire sur la consommation totale 
ggplot(df2) +
  aes(x=mois,y=Consommation.totale) +
  geom_line(color="steelblue3") +
  ggtitle("Consommation Électrique en France") +
  ylab("Consommation totale (GWh)") +
  xlab('') + 
  geom_smooth(method = "loess")




# autoplot(df2, facets = TRUE) +
#  scale_x_continuous(breaks = 2012:20

mutate(df2, total_corrigé = Consommation.totale[1:114] - (value[1:114] * mod1$coefficients))

# qqplot(mod1,id.n=3)
library(forecast)


Ccf(df2$Consommation.totale,df2$value)

acf(df2)   
pacf(df2)


autoplot(df2[,c("Consommation.totale","value")])

?stl()


plot(resid(mod1), type="l")

?tslm

tslm( Consommation.totale ~ mois , data = df2)


# tester 


a <- log(df2$Consommation.totale) - df2$value * mod1[["coefficients"]][["value"]]
exp(a)
plot(exp(a))



autoplot(ts, series=" Consommation corrigée",size=1) +
  autolayer(hw.pred, series=" Prévision HW",size=1) +
  ggtitle("Modèle HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','#E69F00'))



ggplot(df2, aes(x=mois)) + 
  geom_line(aes(y = Consommation.totale), size=0.8) + 
  geom_line(aes(y = Consommation_corrigée), size=0.8) +
  xlab("") +
  ylab("Consommation totale (GWh)") +
  ggtitle("Consommation Électrique en France") +
  scale_color_manual(values=c('steelblue','#E69F00'))




# BOX COX 


library(MASS)
bc <- boxcox(df2$Consommation.totale ~ df2$value) # modèle linéaire 
(lambda <- bc$x[which.max(bc$y)]) # Lambda optimal : -0.1818182 : proche de zéro 

#fit new linear regression model using the Box-Cox transformation
BC_model <- lm(((df2$Consommation.totale^lambda-1)/lambda) ~ df2$value)
summary(BC_model)

library(Ecfun)
b <- ((df2$Consommation.totale^lambda-1)/lambda) - df2$value * BC_model[["coefficients"]][["df2$value"]]
b <- invBoxCox(b,lambda = lambda)

# df2 <- mutate(df2, Consommation_corrigée_bc = exp(a))


mutate(df2, Consommation_corrigée_bc = b) %>% 
  ggplot() +
  geom_line(aes(y=Consommation_corrigée,x=mois,color="Log"),size=1,linetype="twodash") +
  geom_line(aes(y=Consommation_corrigée_bc,x=mois,color="BoxCox"),size=1) +
  scale_color_manual(values =c('Log' = 'blue','BoxCox' = 'deeppink')) +
  labs(x="",y="Consommation Corrigée",title = "Consommation Corrigée Log VS BoxCox")+
  theme(legend.position = "top") 
  #legend.title = theme_blank()


# chunk HW premier
fit <- HoltWinters(ts,seasonal = "multiplicative",gamma = T)

# predictive accuracy
# library(forecast)
#accuracyt(fit)

# predict next three future values
library(forecast)
# forecast(fit, 12)
plot(forecast(fit, 12))


# GARégation différentes régions
library(readxl)

calcul_DJU_14_11_2021 <- read_excel("calcul_DJU_14_11_2021.xlsx")
meteo <- calcul_DJU_14_11_2021[12:21,]
colnames(meteo) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12","total")
meteo[2:14] <- lapply(meteo[2:14],function(x) as.numeric(as.character(x)))


# 

# temp = list.files(pattern="*.xlsx")
# myfiles = lapply(temp, read.delim)

# mylist <- list(m1,m2,m3,m4,m5,m6,m7,m8)

# 8 Régions représentative de la météo en France métropolitaine 
m1 <- read_excel("m1.xlsx") 
m2 <- read_excel("m2.xlsx")
m3 <- read_excel("m3.xlsx")
m4 <- read_excel("m4.xlsx")
m5 <- read_excel("m5.xlsx")
m6 <- read_excel("m6.xlsx")
m7 <- read_excel("m7.xlsx")
m8 <-  read_excel("calcul_DJU_14_11_2021.xlsx")




m1 <- m1[12:21,1:13]
colnames(m1) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m1[2:13] <- lapply(m1[2:13],function(x) as.numeric(as.character(x)))
m1 <- pivot_longer(m1,-year)
m1 <- m1 %>% unite(mois,year,name,sep = "-")

m2 <- m2[12:21,1:13]
colnames(m2) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m2[2:13] <- lapply(m2[2:13],function(x) as.numeric(as.character(x)))
m2 <- pivot_longer(m2,-year)
m2 <- m2 %>% unite(mois,year,name,sep = "-")

m3 <- m3[12:21,1:13]
colnames(m3) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m3[2:13] <- lapply(m3[2:13],function(x) as.numeric(as.character(x)))
m3 <- pivot_longer(m3,-year)
m3 <- m3 %>% unite(mois,year,name,sep = "-")

m4 <- m4[12:21,1:13]
colnames(m4) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m4[2:13] <- lapply(m4[2:13],function(x) as.numeric(as.character(x)))
m4 <- pivot_longer(m4,-year)
m4 <- m4 %>% unite(mois,year,name,sep = "-")


m5 <- m5[12:21,1:13]
colnames(m5) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m5[2:13] <- lapply(m5[2:13],function(x) as.numeric(as.character(x)))
m5 <- pivot_longer(m5,-year)
m5 <- m5 %>% unite(mois,year,name,sep = "-")

m6 <- m6[12:21,1:13]
colnames(m6) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m6[2:13] <- lapply(m6[2:13],function(x) as.numeric(as.character(x)))
m6 <- pivot_longer(m6,-year)
m6 <- m6 %>% unite(mois,year,name,sep = "-")

m7 <- m7[12:21,1:13]
colnames(m7) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m7[2:13] <- lapply(m7[2:13],function(x) as.numeric(as.character(x)))
m7 <- pivot_longer(m7,-year)
m7 <- m7 %>% unite(mois,year,name,sep = "-")

m8 <- m8[12:21,1:13]
colnames(m8) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12")
m8[2:13] <- lapply(m8[2:13],function(x) as.numeric(as.character(x)))
m8 <- pivot_longer(m8,-year)
m8 <- m8 %>% unite(mois,year,name,sep = "-")


# On joint les fichiers et on agrège par la moyenne par date
# On supprimer les valeurs égales à zéro qui sont des valeurs manquantes NA
# On applique le format Date à la colonne "mois"

M2 <-left_join(m1,m2,by="mois") %>% left_join(m3, by="mois") %>% 
  left_join(m4,by="mois") %>% left_join(m5,by="mois") %>%
  left_join(m6,by="mois") %>% left_join(m7,by="mois") %>% left_join(m8,by="mois") %>%
  mutate(value = (value.x+value.y+value.x.x+value.y.y+value.x.x.x+value.y.y.y+value.x.x.x.x+value.y.y.y.y)/8 ) %>%
  select(mois,value) %>% filter(value !="0") %>%
  mutate(mois=ym(mois))


M3 <- left_join(M1,M2,by="mois")


ggplot(M3, aes(x=mois),color = variable) + 
  geom_line(aes(y = value.x), color = "peru",size=1) + 
  geom_line(aes(y = value.y), color="steelblue",size=1) +
  xlab("") +
  ylab("Consommation totale (GWh)") +
  ggtitle("Différence Tours et moyenne FR") +
  labs(color="Legend")


ggplot(M3)+
  geom_line(mapping=aes(y=value.x,x= mois,color="value.x"),size=1 ) +
  geom_line(mapping=aes(y=value.y,x= mois,color="value.y"),size=1) +
  scale_color_manual(values = c(
    'value.x' = 'darkblue',
    'value.y' = 'red')) +
  labs(color = 'Y series') +
  theme(legend.position = "top")

# Plot mission 1
ggplot(df2)+
  geom_line(mapping=aes(y=Consommation.totale,x = mois,color="Consommation Totale"),size=1,linetype="twodash") +
  geom_line(mapping=aes(y=Consommation_corrigée,x= mois,color="Consommation Corrigée (hors chauffage électrique)"),size=1) +
  scale_color_manual(values = c(
    'Consommation Totale' = 'peru',
    'Consommation Corrigée (hors chauffage électrique)' = 'steelblue')) +
  labs(color = '',y="Consommation totale (GWh)",x="",title = c("Consommation Électrique en France")) +
  theme(legend.position = "top")



ggplot(df2 %>% mutate(Consommation_cor=Consommation_corrigée)) + 
  geom_line(mapping=aes(y = Consommation.totale,x=mois,color="Consommation.totale"),size=0.8) + 
  geom_line(mapping=aes(y = Consommation_cor,x=mois),size=0.8,color='Consommation_cor') +
  xlab("") +
  ylab("Consommation totale (GWh)") +
  ggtitle("Consommation Électrique en France") +
  scale_color_manual(values = c(
    'Consommation.totale' = 'yellow',
    'Consommation_cor' = 'red')) +
  labs(color = 'Y series') +
  theme(legend.position = "top")

# MISSION 2 PLOT 


autoplot(ts, series="Consommation Corrigée",size=0.8) +
  autolayer(MA_ts, series="Tendance : Moyenne Mobile",size=1.5,na.rm = T) +
  autolayer(decomp_ts_adj, series="Effet Saisonnier",size=0.8,linetype="solid") +
  ggtitle("Consommation Électrique en France",subtitle = "Hors chauffage électrique") +
  labs(x="",y="Consommation Corrigée (GWh)") +
  theme(legend.position="bottom",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','yellowgreen','red'))

autoplot(ts, series=" Consommation corrigée",size=0.8) +
  autolayer(hw.pred, series=" Prévision HW",size=0.8) +
  ggtitle("Modèle HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','#E69F00'))

# season plot 
cowplot::plot_grid(ggseasonplot(window(ts))+ggtitle("Season plot")+labs(x="",y=""),ggsubseriesplot(window(ts),polar=T)+labs(x="",y=""),nrow = 2, labels = c("A", "B"))
ggseasonplot(window(ts))
ggseasonplot(window(ts),polar = T) + ggtitle("Season plot Polar")
ggsubseriesplot(window(ts),polar=T)


plotly::ggplotly(ggseasonplot(window(ts)))






# Visualisation Bruit Blanc : Autocorrélation
ggAcf(ts, ci = 0.95) + ggtitle("TS: Autocorrélation")

# Test
Box.test(ts, lag = 24, type = "Ljung-Box")

autoplot(ts)

checkresiduals(fit
               )
shapiro.test(residuals(fit)) # non-normal si p-value < seuil (0.05)


# Modèle HW

ets(ts) # prévision paramètre 

autoplot(ets(ts))


# Modèle TBATS----------

ts %>% tbats() %>% forecast() %>% autoplot()
summary(ts %>% tbats() %>% forecast())

tb_mod <- ts_train %>% tbats() 
tb_fc <- tb_mod %>% forecast(h=33)

tb_mod


autoplot(ts_test, series=" Data Train",size=0.8) +
  autolayer(ts_test, series="Data Test",size=0.8) +
  autolayer(tb_fc,serie="TB : Prévision sur Data Test") +
  ggtitle("TBATS : Évaluation du modèle sur Data Test") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','green','#E69F00'))



accuracy(tb_fc,ts)

# Forecasting

tb_pred <- ts %>% tbats(model = tb_mod) %>% predict(12)
autoplot(tb_pred)

accuracy(tb_pred)




# TRAIN and TEST 

# exemple 

# flow_ts <- hydrometeo_monthly_ts[, 1]



ts_train <- window(ts, start = 2012, end = 2018.6666667)
ts_test <- window(ts, start = 2018.75)

ts_ets <- ts_train %>% ets() 
ts_fc <- ts_ets %>% forecast(h=33) 

ts_pred <- predict(ts_fc,12)

plot(ts_pred)
accuracy(ts_ets)
accuracy(ts_fc)

accuracy(hw.pred)


checkresiduals(ts_ets)
checkresiduals(ts_pred)
shapiro.test(residuals(ts_pred)) # Résidus peuvent suivre une loi Normale.


autoplot(ts, series=" Consommation corrigée",size=0.8) +
  autolayer(hw.pred, series=" Prévision HW",size=0.8) +
  autolayer(ts_fc,serie="prevision train") +
  ggtitle("Modèle HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','#E69F00','green'))

autoplot(ts_test, series=" Data test",size=0.8) +
  autolayer(ts_fc,serie="HW : prevision") +
  ggtitle("Modèle HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('green','#E69F00'))

# Prévision HW sur le data Test nous donne une bonne tendance mais néglige les effets saisonniers extrêmes 

autoplot(ts, series=" Consommation corrigée",size=0.8) +
  autolayer(ts_pred, series=" Prévision HW",size=0.8) +
  ggtitle("Modèle HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','#E69F00','green'))

# Prévision sur les 12 prochaine mois avec ce modèle HW 


### SARIMA 

train.arima <- auto.arima(ts_train)
train.forecast <- forecast(train.arima, level = c(95), h = 33)
autoplot(train.forecast,serie="conso")


t2 <- autoplot(ts_test, series=" Consommation corrigée",size=0.8) +
  autolayer(train.forecast, series=" Prévision ARIMA") +
  ggtitle(" ARIMA : Évaluation du modèle sur Data Test") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','magenta'))

t2

# Test Hw et sarima
gridExtra::grid.arrange(t1,t2)


# d.arima <- auto.arima(ts)

d.forecast <- forecast(d.arima, level = c(95), h = 12)
autoplot(d.forecast,serie="conso")

new.arima <- Arima()

autoplot(ts, series=" Consommation corrigée",size=0.8) +
  autolayer(d.forecast, series=" Prévision ARIMA",size=0.8) +
  ggtitle(" Prévision de consommation électrique modèle ARIMA") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','magenta'))


# new.arima <- Arima(y=ts,order = c(0,0,0),seasonal = c(2,1,0))


# Notre Modèle Sarima
new.arima <- Arima(y=ts,model = train.arima)
new.f.arima <- new.arima %>% forecast(level=c(90),h=12) 


# Comparaison des prédictions HW et SARIMA

k1 <- autoplot(ts_test, series=" Consommation corrigée",size=0.8) +
  autolayer(new.f.arima, series="Prévision SARIMA ~ 12 mois ",size=0.1) +
  #autolayer(d.forecast,series = "y",size=0.1)+
  #autolayer(hw.pred, series=" Prévision HW ~ 12 mois",size=0.8)+
  ggtitle("SARIMA  : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','magenta','red'))


k2 <- autoplot(ts_test, series=" Consommation corrigée",size=0.8) +
  autolayer(hw.pred, series=" Prévision HW ~ 12 mois",size=0.8) +
  ggtitle("HOLT WINTERS : Prévision de la consommation électrique en France") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','#E69F00'))


cowplot::plot_grid(k2,k1,nrow = 2)
# Test Hw et sarima
gridExtra::grid.arrange(t1,t2)


# Comparaison accuracy deux modèles 

accuracy(new.f.arima)
accuracy(hw)
#accuracy(d.forecast)

# Test SARIMA 
checkresiduals(new.f.arima)
shapiro.test(residuals(new.f.arima))

# On rejette bruite blanc et normalité des residus 


# Premier ARIMA 




d.arima <- auto.arima(ts)
d.forecast <- forecast(d.arima, level = c(95), h = 12)
# autoplot(d.forecast,serie="conso")


autoplot(ts, series=" Consommation corrigée",size=0.8) +
  autolayer(d.forecast, series=" Prévision ARIMA",size=0.8) +
  ggtitle(" Prévision de consommation électrique modèle ARIMA") +
  theme(plot.title = element_text(size=10)) +
  labs(x="",y="Consommation corrigée (GWh)") +
  theme(legend.position="top",plot.title = element_text(size=10)) +
  scale_color_manual(values=c('steelblue','magenta'))
  
  
  
  
# HTS package


library(hts)


Donnée Météo 1 
```{r warning=FALSE}
library(readxl)
calcul_DJU_14_11_2021 <- read_excel("calcul_DJU_14_11_2021.xlsx")

meteo <- calcul_DJU_14_11_2021[11:24,]
meteo <- meteo[2:14,]
colnames(meteo) <- c("year","01","02","03","04","05","06","07","08","09","10","11","12","total")

meteo[2:14] <- lapply(meteo[2:14],function(x) as.numeric(as.character(x)))
meteo <- meteo[1:10,]

# Pivot_longer

M1 <- select(meteo,-total)

M1 <- pivot_longer(M1,-year)

# df %<>%
# unite(ColAandB, colA, colB, remove = FALSE)

M1 <- M1 %>% unite(mois,year,name,sep = "-")
M1 <- mutate(M1, mois=ym(mois)) # Format date 
M1 <- filter(M1, value !="0") # on suprrime les valeurs nulles qui sont  des NA

```

## DYGRAPH ---------

library(dygraphs)
library(xts)

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)


# (x = data$value, order.by = data$time)

x1 <- xts(x=df2$Consommation_corrigée,order.by=df2$mois)

x2 <- xts(v)

lungdygraph(ts)


# exemple 

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)


# application 

dygraph(v, main= "Consommation Électrique en France") %>%
  dySeries("ts2", label = "Totale", color = "peru") %>%
  dySeries("ts", label = "Corrigée(hors chauffage électrique)", color= "steelblue") %>%
  dyOptions(stackedGraph = F) %>%
  dyRangeSelector(height = 20)




# v2

v2 <- cbind(MA_ts,ts)
v2

dygraph(v2, main= "Consommation Électrique en France",ylab = "Consommation Corrigée (GWh)") %>%
  dySeries("MA_ts", label = "Moyenne Mobile", color = "red") %>%
  dySeries("ts", label = "Consommation(hors chauffage électrique)", color= "steelblue") %>%
  dyOptions(stackedGraph = F) %>%
  dyRangeSelector(height = 20)


# test
tseries::adf.test(ts) # Non Stationnaire
tseries::adf.test(ts_train) # S
tseries::adf.test(ts_test) # NS 


# Moyenne par année 


temps_year <- split(ts, f = "year")

lapply(X = temps_year, FUN = mean) 

#find mean sales by week
#round dates down to week
df2$year <- floor_date(df2$month, "year")


df2 %>%
  group_by(year) %>%
  summarize(mean = mean(Consommation_corrigée))

aggregate.ts(ts,by=12,FUN=mean)
plot(aggregate.ts(ts,by=12,FUN=mean))


# SARIMA VS PROPHET 


ts_pr <-  ts(prophet_forecast$trend, start=c(2012, 1), end=c(2022, 6), frequency=12)

autoplot(prophet_forecast
         )

#ts_sa <- 

# GOAL: DETECT THE PROMOTIONAL WEEKS **OF THE STORES** FOR EACH CUSTOMER-PRODUCT


library(tidyverse)
library(aweek)
library(gridExtra)
library(TSA)
library(tseries)
library(AST)
library(readxl)
library(plotly)
library(FactoMineR)
library(Factoshiny)
library(factoextra)
library(ggcorrplot)
library(tibbletime)
library(anomalize)
options(ggrepel.max.overlaps = Inf)  # This option allows to avoid that, by setting repel = TRUE, if there are too many labels to print because of the overlapping, only some of them will be printed.


#### Load data ----
GAME_3 <- read_excel("GAME 3.xlsx", col_types = c("text", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "numeric", "numeric"))
View(GAME_3)

#### Cleaning ----

# week2date("2022-W00", week_start = "Monday")
# week2date("2021-W52", week_start = "Monday")

GAME_3 <- GAME_3 %>%
  mutate(Date = week2date(YW, week_start = "Monday")) %>%
  select(-PriceUnits, -PriceVolume)

GAME_3 <- GAME_3 %>%
  mutate(PriceUnits = Value/Units, PriceVolume = Value/Volume) %>%
  select(CUSTOMER, PRODOTTO, YW, Date, Units, PriceUnits, Volume, PriceVolume, 
         Value, WD)

Customer_A <- GAME_3 %>%
  filter(CUSTOMER == "Customer A")

#### Date (The date of the Customer B for the COMPETITION 2 product is a mess, i.e. ----
# 25 dates are repeated, but the price units, units, etc, aren't) 
Customer_B <- GAME_3 %>%
  filter(CUSTOMER == "Customer B")

nrow(GAME_3 %>%
  group_by(CUSTOMER, PRODOTTO) %>%
  summarise(unique_Date = unique(Date))) # Here we sow that 25 date are repeated (31217 unique dates per CUSTOMER and PRODOTTO over 31242 in total)

nrow(Customer_A %>%
       group_by(PRODOTTO) %>%
       summarise(unique_n = unique(Date))) # No dates repeated for the Customer_A

nrow(Customer_B %>%
       group_by(PRODOTTO) %>%
       summarise(unique_n = unique(Date))) # 25 dates repeated (so, all the repeated data are in this Customer)

unique_date_CUSTOMER_B <- GAME_3 %>%
  filter(CUSTOMER == "Customer B") %>% 
  group_by(PRODOTTO) %>%
  summarise(unique_Date = unique(Date)) # Here there are only the unique dates (see unique__number_date_CUSTOMER_B to understand why we wrote this)


number_date_Customer_B <- Customer_B %>%
  group_by(PRODOTTO) %>%
  summarise(n_product = n()) # Here we sow that in competition 2 there are
# 25 repeated dates

unique_number_date_CUSTOMER_B <- unique_date_CUSTOMER_B %>%
  group_by(PRODOTTO) %>%
  summarise(n_product = n()) # Here we sow that in competition 2 there are
# 25 repeated dates

repeated_dates <- Customer_B %>%
  filter(PRODOTTO == "COMPETITION 2") %>%
  filter(YW == "2018-W18" | YW == "2018-W24" | YW == "2018-W25" | YW == "2018-W30"
         | YW == "2018-W32" | YW == "2018-W33" | YW == "2018-W34"
         | YW == "2018-W37" | YW == "2018-W39" | YW == "2018-W41" | YW == "2018-W42" 
         | YW == "2019-W05" | YW == "2019-W06" | YW == "2019-W07" 
         | YW == "2019-W08" | YW == "2019-W09" | YW == "2019-W10"
         | YW == "2019-W12" | YW == "2019-W13" | YW == "2019-W15"
         | YW == "2019-W17" | YW == "2019-W40" | YW == "2020-W16"
         | YW == "2020-W37" | YW == "2021-W02") %>%
  arrange(YW) # Here we have only the 25 dates which repeated

# With ggplot
Customer_B %>%
  filter(PRODOTTO == "COMPETITION 2") %>%
  ggplot(mapping = aes(Date, PriceUnits)) +
  geom_line()

# With plotly
options(warn = -1) 
# warn: sets the handling of warning messages. 
# If warn is negative all warnings are ignored.
# If warn is zero (the default) warnings are stored until the top–level function returns. 
# If fewer than 10 warnings were signalled they will be printed otherwise a message saying how many (max 50) were signaled.
Customer_B %>%
  filter(PRODOTTO == "COMPETITION 2") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  layout(showlegend = F) %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1"),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


Customer_B_first_values <- read_excel("daticustomerb_primo.xlsx", 
                                  col_types = c("text", "text", "date", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))

Customer_B_second_values <- read_excel("customerb_secondo.xlsx", 
                                  col_types = c("text", "text", "date", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))


a <- Customer_B_first_values %>%
  ggplot(mapping = aes(Date, PriceUnits)) +
  geom_line() +
  geom_point(color = "red") # Plot with the first values (first dates)

b <- Customer_B_second_values %>%
  ggplot(mapping = aes(Date, PriceUnits)) +
  geom_line() +
  geom_point(color = "red") # Plot with the second values (second dates)

c <- Customer_B %>%
  filter(PRODOTTO == "COMPETITION 2") %>%
  ggplot(mapping = aes(Date, PriceUnits)) +
  geom_line() # Plot with both values (both dates)

grid.arrange(a,b,c, nrow = 3)

# Maybe there is some repetition because different stores of the same customer
# sell in the same day but at different prices. But it should be more common
# whereas there are only 25 occurrences in the whole dataset...


#### Customer A: summary, MCA and clustering for the product BABY DRY DUO DWCT ----

Customer_A %>%
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY DUO DWCT (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY DUO DWCT (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

# Building the data-set
Customer_A_BABY_DRY_DUO_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  select(Date:WD)

Customer_A_BABY_DRY_DUO_DWCT <- as.data.frame(Customer_A_BABY_DRY_DUO_DWCT)

length(unique(Customer_A_BABY_DRY_DUO_DWCT$Date))

rownames(Customer_A_BABY_DRY_DUO_DWCT) <- Customer_A_BABY_DRY_DUO_DWCT$Date

Customer_A_BABY_DRY_DUO_DWCT <- Customer_A_BABY_DRY_DUO_DWCT %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_DUO_DWCT)
corr_Customer_A_BABY_DRY_DUO_DWCT <- cor(Customer_A_BABY_DRY_DUO_DWCT)
ggcorrplot(corr_Customer_A_BABY_DRY_DUO_DWCT, hc.order = TRUE, lab = TRUE, title = "BABY DRY DUO DWCT")
pairs(Customer_A_BABY_DRY_DUO_DWCT)

# Starting the MCA
Customer_A_BABY_DRY_DUO_DWCT %>%
  ggplot(mapping = aes(PriceUnits)) +
  geom_histogram() # To understand how to split PriceUnits in classes

Customer_A_BABY_DRY_DUO_DWCT %>%
  ggplot(mapping = aes(Units)) +
  geom_histogram() # To understand how to split Units in classes

Customer_A_BABY_DRY_DUO_DWCT %>%
  ggplot(mapping = aes(Value)) +
  geom_histogram() + 
  scale_x_continuous(labels = scales::comma) # To understand how to split Value in classes

Customer_A_BABY_DRY_DUO_DWCT %>%
  ggplot(mapping = aes(WD)) +
  geom_histogram()

Customer_A_BABY_DRY_DUO_DWCT <- Customer_A_BABY_DRY_DUO_DWCT %>%
  mutate(PriceUnits_class =
           if_else(PriceUnits<8, "p<8",
                                    if_else(PriceUnits>=8 & PriceUnits<8.5, "8<=p<8.5",
                                            if_else(PriceUnits>=8.5 & PriceUnits<9, "8.5<=p<9",
                                                    if_else(PriceUnits>=9 & PriceUnits<9.5, "9<=p<9.5",
                                                            if_else(PriceUnits>=9.5 & PriceUnits<10, "9.5<=p<10",
                                                                if_else(PriceUnits>=10 & PriceUnits<11, "10<=p<11","11<=p<=12"))))))) %>%      
  mutate(Units_class = 
           if_else(Units<2500, "units<2500",
                   if_else(Units>=2500&Units<5000, "2500<=units<5000",
                    if_else(Units>=5000&Units<7500, "5000<=units<7500",
                      if_else(Units>=7500&Units<10000, "7500<=units<10000","units>=10000"))))) %>%
  mutate(WD_class = 
           if_else(WD<80, "wd<80",
                   if_else(WD>=80&WD<85, "80<=wd<85",
                           if_else(WD>=85&WD<90, "85<=wd<90",
                                   if_else(WD>=90&WD<92.5, "90<=wd<92.5",
                                           if_else(WD>=92.5&WD<95, "92.5<=wd<95",
                                                   if_else(WD>=95&WD<97.5, "95<=wd<97.5",
                                           "wd>=97.5"))))))) %>%
  mutate(Value_class =
           if_else(Value<25000, "value<25000",
                   if_else(Value>=25000&Value<50000, "25000<=value<50000",
                           if_else(Value>=50000&Value<75000, "50000<=value<75000",
                                   if_else(Value>=75000&Value<100000, "75000<=value<100000", "value>=100000")))))
  

  
Customer_A_BABY_DRY_DUO_DWCT %>%
  group_by(PriceUnits_class) %>%
  summarise(n = n())

Customer_A_BABY_DRY_DUO_DWCT %>%
  group_by(Value_class) %>%
  summarise(n = n())

Customer_A_BABY_DRY_DUO_DWCT %>%
  group_by(Units_class) %>%
  summarise(n=n())

Customer_A_BABY_DRY_DUO_DWCT %>%
  group_by(WD_class) %>%
  summarise(n=n())


res.MCA<-MCA(Customer_A_BABY_DRY_DUO_DWCT,quanti.sup=c(1,2,3,4,5,6),graph=FALSE)
plot.MCA(res.MCA,habillage='contrib',cex=1.5,cex.main=1.5,cex.axis=1.5,label ='none') # Guttman effect (due to the transformation of quantitative variables in qualitative variables)

# Starting the HCPC
Factoshiny(Customer_A_BABY_DRY_DUO_DWCT %>%
             select(Units:WD))

res.PCA<-PCA(Customer_A_BABY_DRY_DUO_DWCT %>% select(Units:WD),ncp=Inf, scale.unit=FALSE,graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree',tree.barplot =FALSE)
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')

# Starting k-means
scale_data <- scale(Customer_A_BABY_DRY_DUO_DWCT) # The scale function with the default values (center = TRUE and scale = TRUE) subtracts the values of each variable for the variable mean and then divides the result for the variable standard deviation (it is the z-score: (x-mean)/sd). It must be applied when you have several variables to examine over multiple scales (e.g. one with magnitude 100, one with magnitude 1000)
data_distance_matrix <- dist(as.matrix(scale_data)) # too many observations to observe it (but this line of code is not important)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out <- kmeans(scale_data, centers = 4, nstart = 1000) # As you know, to find, for a specific k, the best way of clustering you need to iteratively repeat the k-means steps and every time check the WSS. You will assign to that specific k the clustering with the lowest WSS (this number of iterations is nstart)
km.out
km.clusters <- km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, palette = "Set1") + theme_minimal() # it computes principal components by default (axes = c(1,2). You can choose to display another factorial plane: axes = c(1,3) for example).
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, palette = "Set1", axes = c(1,3)) + theme_minimal()

Factoshiny(Customer_A_BABY_DRY_DUO_DWCT)
res.PCA<-PCA(Customer_A_BABY_DRY_DUO_DWCT,graph=FALSE)
summary(res.PCA)
dimdesc(res.PCA)
plot.PCA(res.PCA,cex=1.1,cex.main=1.1,cex.axis=1.1) # The second dimension is turned up and down with respect the one in fviz_cluster
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(2,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_DUO_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_DUO_DWCT %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
  
data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75)

data_anomalized %>%
  filter(anomaly == 'Yes')

#### Customer A: summary, MCA and clustering for the product BABY DRY QUADRI ---- 

Customer_A %>%
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY QUADRI (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY QUADRI (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

# Building the data-set
Customer_A_BABY_DRY_QUADRI <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  select(Date:WD)

Customer_A_BABY_DRY_QUADRI <- as.data.frame(Customer_A_BABY_DRY_QUADRI)

length(unique(Customer_A_BABY_DRY_QUADRI$Date))

rownames(Customer_A_BABY_DRY_QUADRI) <- Customer_A_BABY_DRY_QUADRI$Date

Customer_A_BABY_DRY_QUADRI <- Customer_A_BABY_DRY_QUADRI %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_QUADRI)
corr_Customer_A_BABY_DRY_QUADRI <- cor(Customer_A_BABY_DRY_QUADRI)
ggcorrplot(corr_Customer_A_BABY_DRY_QUADRI, hc.order = TRUE,  lab = TRUE, title = "BABY DRY QUADRI")
pairs(Customer_A_BABY_DRY_QUADRI)

# HCPC
Factoshiny(Customer_A_BABY_DRY_QUADRI)

res.PCA<-PCA(Customer_A_BABY_DRY_QUADRI,ncp=Inf, scale.unit=FALSE,graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=4,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')

# Starting K-means
scale_data <- scale(Customer_A_BABY_DRY_QUADRI)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 3, nstart = 10000) # With 3 clusters all the "maybe promotional" weeks in the time series are in the blue cluster
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()
  
res.PCA<-PCA(Customer_A_BABY_DRY_QUADRI,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 92.6% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_QUADRI <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_QUADRI %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75)

a <- data_anomalized %>%
  filter(anomaly == 'Yes')

data_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")

#### Customer A: summary, MCA and clustering for the product BABY DRY DWCT ----

Customer_A %>%
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY DWCT (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY DWCT (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

# Building the data-set
Customer_A_BABY_DRY_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  select(Date:WD)

Customer_A_BABY_DRY_DWCT <- as.data.frame(Customer_A_BABY_DRY_DWCT)

length(unique(Customer_A_BABY_DRY_DWCT$Date))

rownames(Customer_A_BABY_DRY_DWCT) <- Customer_A_BABY_DRY_DWCT$Date

Customer_A_BABY_DRY_DWCT <- Customer_A_BABY_DRY_DWCT %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_DWCT)
corr_Customer_A_BABY_DRY_DWCT <- cor(Customer_A_BABY_DRY_DWCT)
ggcorrplot(corr_Customer_A_BABY_DRY_DWCT, hc.order = TRUE,  lab = TRUE, title = "BABY DRY DWCT")
pairs(Customer_A_BABY_DRY_DWCT)


# Starting K-means
scale_data <- scale(Customer_A_BABY_DRY_DWCT)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 3, nstart = 10000) # With 3 clusters all the "maybe promotional" weeks in the time series are in the blue cluster
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()

res.PCA<-PCA(Customer_A_BABY_DRY_DWCT,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 97.135% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_DWCT %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder, alpha = 0.075) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75, time_recomposed = TRUE) +
  ggtitle("alpha = 0.075")

a <- data_anomalized %>%
  filter(anomaly == 'Yes')
#### Customer A: summary, MCA and clustering for the product BABY DRY TRIO DWCT ----
Customer_A %>%
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY TRIO DWCT (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY TRIO DWCT (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

# Building the data-set
Customer_A_BABY_DRY_TRIO_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  select(Date:WD)

Customer_A_BABY_DRY_TRIO_DWCT <- as.data.frame(Customer_A_BABY_DRY_TRIO_DWCT)

length(unique(Customer_A_BABY_DRY_TRIO_DWCT$Date))

rownames(Customer_A_BABY_DRY_TRIO_DWCT) <- Customer_A_BABY_DRY_TRIO_DWCT$Date

Customer_A_BABY_DRY_TRIO_DWCT <- Customer_A_BABY_DRY_TRIO_DWCT %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_TRIO_DWCT)
corr_Customer_A_BABY_DRY_TRIO_DWCT <- cor(Customer_A_BABY_DRY_TRIO_DWCT)
ggcorrplot(corr_Customer_A_BABY_DRY_TRIO_DWCT, hc.order = TRUE,  lab = TRUE, title = "BABY DRY TRIO DWCT")  # Here the price and the units are not negatively correlated (probably because the elasticity of demand is really near to zero)
pairs(Customer_A_BABY_DRY_TRIO_DWCT)


# Starting K-means
scale_data <- scale(Customer_A_BABY_DRY_TRIO_DWCT)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 3, nstart = 10000) 
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()

res.PCA<-PCA(Customer_A_BABY_DRY_TRIO_DWCT,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 92.485% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_TRIO_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_TRIO_DWCT %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder, alpha = 0.05) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75, time_recomposed = TRUE)

a <- data_anomalized %>%
  filter(anomaly == 'Yes') # For BABY DRY TRIO DWCT the anomaly doesn't work really good

#### Customer A: summary, MCA and clustering for the product BABY DRY IL MUTANDINO SP ----
Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY IL MUTANDINO SP (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY IL MUTANDINO SP (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

# Building the data-set
Customer_A_BABY_DRY_IL_MUTANDINO_SP <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  select(Date:WD)

Customer_A_BABY_DRY_IL_MUTANDINO_SP <- as.data.frame(Customer_A_BABY_DRY_IL_MUTANDINO_SP)

length(unique(Customer_A_BABY_DRY_IL_MUTANDINO_SP$Date))

rownames(Customer_A_BABY_DRY_IL_MUTANDINO_SP) <- Customer_A_BABY_DRY_IL_MUTANDINO_SP$Date

Customer_A_BABY_DRY_IL_MUTANDINO_SP <- Customer_A_BABY_DRY_IL_MUTANDINO_SP %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_IL_MUTANDINO_SP)
corr_Customer_A_BABY_DRY_IL_MUTANDINO_SP <- cor(Customer_A_BABY_DRY_IL_MUTANDINO_SP)
ggcorrplot(corr_Customer_A_BABY_DRY_IL_MUTANDINO_SP, hc.order = TRUE,  lab = TRUE, title = "BABY DRY IL MUTANDINO SP") # Positive correlation between prices and units  
pairs(Customer_A_BABY_DRY_IL_MUTANDINO_SP)


# Starting K-means
scale_data <- scale(Customer_A_BABY_DRY_IL_MUTANDINO_SP)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 3, nstart = 10000) 
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()

res.PCA<-PCA(Customer_A_BABY_DRY_IL_MUTANDINO_SP,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 99.089% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_IL_MUTANDINO_SP <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_IL_MUTANDINO_SP %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder, alpha = 0.05) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75, time_recomposed = TRUE)

a <- data_anomalized %>%
  filter(anomaly == 'Yes')

#### Customer A: summary, MCA and clustering for the product BABY DRY IL MUTANDINO 2 X DWCT ----
Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY IL MUTANDINO 2 X DWCT (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY IL MUTANDINO 2 X DWCT (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

# Building the data-set
Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  select(Date:WD)

Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT <- as.data.frame(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT)

length(unique(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT$Date))

rownames(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT) <- Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT$Date

Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT <- Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT)
corr_Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT <- cor(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT)
ggcorrplot(corr_Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT, hc.order = TRUE,  lab = TRUE, title = "BABY DRY IL MUTANDINO 2 X DWCT")  
pairs(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT)


# Starting K-means
scale_data <- scale(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 4, nstart = 10000) 
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()

res.PCA<-PCA(Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 94.867% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection
Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT <- Customer_A %>%
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  select(Date:WD)

data_anomalized <- Customer_A_BABY_DRY_IL_MUTANDINO_2_X_DWCT %>%
  time_decompose(PriceVolume, merge = TRUE) %>%
  anomalize(remainder, alpha = 0.2) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75, time_recomposed = TRUE)

a <- data_anomalized %>%
  filter(anomaly == 'Yes')

#### Customer A: summary, MCA and clustering for the product COMPETITION 21 ----
Customer_A %>%
  filter(PRODOTTO == "COMPETITION 21") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='COMPETITION 21 (Units)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

Customer_A %>%
  filter(PRODOTTO == "COMPETITION 21") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~PriceUnits)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='COMPETITION 21 (PriceUnits)') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')

# Building the data-set
Customer_A_COMPETITION_21 <- Customer_A %>%
  filter(PRODOTTO == "COMPETITION 21") %>%
  select(Date:WD)

Customer_A_COMPETITION_21 <- as.data.frame(Customer_A_COMPETITION_21)

length(unique(Customer_A_COMPETITION_21$Date))

rownames(Customer_A_COMPETITION_21) <- Customer_A_COMPETITION_21$Date

Customer_A_COMPETITION_21 <- Customer_A_COMPETITION_21 %>%
  select(Units:WD)

# Variables correlation
summary(Customer_A_COMPETITION_21)
corr_Customer_A_COMPETITION_21 <- cor(Customer_A_COMPETITION_21)
ggcorrplot(corr_Customer_A_COMPETITION_21, hc.order = TRUE,  lab = TRUE, title = "COMPETITION 21")  
pairs(Customer_A_COMPETITION_21)


# Starting K-means (USELESS HERE)
scale_data <- scale(Customer_A_COMPETITION_21)
fviz_nbclust(scale_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km.out = kmeans(scale_data, centers = 4, nstart = 10000) 
km.out
km.clusters = km.out$cluster
fviz_cluster(list(data = scale_data, cluster = km.clusters), repel = TRUE, labelsize = 10, palette = "Set1") +
  theme_minimal()

res.PCA<-PCA(Customer_A_COMPETITION_21,graph=FALSE)
summary(res.PCA) # The dim 1 and the dim 2 explain the 88.511% of the total variance
dimdesc(res.PCA)
# plot.PCA(res.PCA,axes=c(1,2),cex=1.1,cex.main=1.1,cex.axis=1.1)
plot.PCA(res.PCA,axes=c(1,2),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)
plot.PCA(res.PCA,axes=c(1,3),choix='var',habillage = 'contrib',cex=1.5,cex.main=1.5,cex.axis=1.5)

# Starting the Anomaly Detection (IT DOESN'T WORK)
Customer_A_COMPETITION_21 <- Customer_A %>%
  filter(PRODOTTO == "COMPETITION 21") %>%
  select(Date:WD)

data_anomalized <- Customer_A_COMPETITION_21 %>%
  time_decompose(Units, merge = TRUE) %>%
  anomalize(remainder, alpha = 0.05) %>%
  time_recompose()

data_anomalized %>% plot_anomalies(ncol = 2, alpha_dots = 0.75, time_recomposed = TRUE)

a <- data_anomalized %>%
  filter(anomaly == 'Yes')
#### General plots Customer_A ----

p1 <- Customer_A %>%
  group_by(PRODOTTO) %>%
  summarise(mean_PRODOTTO_Units = mean(Units)) %>%
  ggplot(mapping = aes(mean_PRODOTTO_Units, PRODOTTO)) +
  geom_bar(mapping = aes(fill = PRODOTTO), stat = "identity", width = 1,
           position = "dodge", show.legend = FALSE, color = "black")

p2 <- Customer_A %>%
  group_by(PRODOTTO) %>%
  summarise(mean_PRODOTTO_PriceUnits = mean(PriceUnits)) %>%
  ggplot(mapping = aes(mean_PRODOTTO_PriceUnits, PRODOTTO)) +
  geom_bar(mapping = aes(fill = PRODOTTO), stat = "identity", width = 1,
           position = "dodge", show.legend = FALSE, color = "black")

grid.arrange(p1, p2, nrow = 2, ncol = 1)


plot_ly(Customer_A, y = ~PriceUnits, color = ~PRODOTTO, type = "box") %>%
  layout(
    xaxis= list(showticklabels = FALSE),
    showlegend = FALSE
    )
  

plot_ly(Customer_A, y = ~Units, color = ~PRODOTTO, type = "box") %>%
  layout(
    xaxis= list(showticklabels = FALSE),
    showlegend = FALSE
  )


#### plots Customer_A ----

Customer_A %>%
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY DUO DWCT') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

Customer_A %>%
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  plot_ly(type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Units)%>%
  # add_trace(x = ~Date, y = ~Units, line = list(color = '#8c564b'))%>%
  layout(showlegend = F, title='BABY DRY QUADRI') %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick = "M1",
                 rangeslider = list(visible = T),
                 rangeselector = list(
                   buttons = list(
                     list(count=1, label="1m", step="month", stepmode="backward"),
                     list(count=6, label="6m", step="month", stepmode="backward"),
                     list(count=1, label="YTD", step="year", stepmode="todate"),
                     list(count=1, label="1y", step="year", stepmode="backward"),
                     list(step="all")
                   )
                 )),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

p1_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY DUO DWCT UNITS") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
    )

p1_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY DUO DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY DUO DWCT PRICES ") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p1_units, p1_prices, nrow = 2)

p2_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY DWCT") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p2_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY DWCT") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p2_units, p2_prices, nrow = 2)

p3_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY IL MUTANDINO 2 X DWCT") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p3_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY IL MUTANDINO 2 X DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY IL MUTANDINO 2 X DWCT") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p3_units, p3_prices, nrow = 2)

p4_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY IL MUTANDINO SP") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p4_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY IL MUTANDINO SP") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY IL MUTANDINO SP") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p4_units, p4_prices, nrow = 2)

p5_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY QUADRI") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p5_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY QUADRI") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY QUADRI") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p5_units, p5_prices, nrow=2)

p6_units <- Customer_A %>% 
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY TRIO DWCT") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p6_prices <-  Customer_A %>% 
  filter(PRODOTTO == "BABY DRY TRIO DWCT") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      BABY DRY TRIO DWCT") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p6_units, p6_prices, nrow = 2)

p7_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 11") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 11") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p7_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 11") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 11") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p7_units, p7_prices, nrow=2)

p8_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 12") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 12") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p8_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 12") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 12") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p8_units, p8_prices, nrow=2)

p9_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 15") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 15") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p9_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 15") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 15") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p9_units, p9_prices, nrow = 2)

p10_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 17") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 17") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p10_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 17") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 17") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p10_units, p10_prices, nrow=2)

p11_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 19") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 19") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p11_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 19") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 19") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p11_units, p11_prices, nrow=2)

p12_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 21") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 21") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p12_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 21") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 21") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p12_units, p12_prices, nrow=2)

p13_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 25") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 25") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p13_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 25") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 25") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p13_units, p13_prices, nrow=2)

p14_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 7") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 7") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p14_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 7") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 7") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p14_units, p14_prices, nrow=2)

p15_units <- Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 8") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 8") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p15_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COMPETITION 8") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COMPETITION 8") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p15_units, p15_prices, nrow=2)

p16_units <- Customer_A %>% 
  filter(PRODOTTO == "COSTUMINO") %>%
  ggplot() +
  geom_line(mapping = aes(Date, Units), color = "red") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COSTUMINO") +
  theme(
    title = element_text(color = "red", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "red"),
    axis.title.y=element_text(face = "bold", color = "red"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

p16_prices <-  Customer_A %>% 
  filter(PRODOTTO == "COSTUMINO") %>%
  ggplot() +
  geom_line(mapping = aes(Date, PriceUnits), color = "blue") +
  scale_x_date(date_breaks = "4 months") +
  ggtitle("                      COSTUMINO") +
  theme(
    title = element_text(color = "blue", face = "bold" ),
    axis.title.x=element_text(face = "bold", color = "blue"),
    axis.title.y=element_text(face = "bold", color = "blue"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="azure2")
  )

grid.arrange(p16_units, p16_prices, nrow=2)





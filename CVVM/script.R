
# first things first
install.packages("foreign")
install.packages("tidyverse")
install.packages("scales")
install.packages("formattable")

library(foreign)
library(tidyverse)
library(scales)
library(formattable)

setwd("")
getwd()

cvvm = read.spss("V1906_F1.sav", to.data.frame = T)
                                # .., to.data.frame = T - IMPORTANT, because of pipe

view(cvvm)
summary(cvvm)

sel = cvvm %>% select(PO.109A:PO.111I)

sel_list = as.list(sel)

str(sel_list)

rec = function(x) {
  
    recode(x, "naprosto žádná hrozba" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
           "7" = 7, "8" = 8, "9" = 9, "naprosto zásadní hrozba" = 10, "NEVÍ" = NULL)
}

sel_list_rec = lapply(sel_list, rec)

sel_list = lapply(sel_list_rec, as.numeric)

str(sel_list)

##### double-check of the values

PO_recode = cvvm$PO.109A %>% 
  recode("naprosto žádná hrozba" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
                 "7" = 7, "8" = 8, "9" = 9, "naprosto zásadní hrozba" = 10, "NEVÍ" = NULL) %>% as.numeric()

PO_recode


mean(PO_recode, na.rm = T) %>% round(1)
sd(PO_recode, na.rm = T) %>% round(3)
round((sum(is.na(PO_recode)) / length(PO_recode) * 100), 1)


mean(sel_list$PO.109A, na.rm = T) %>% round(1)
sd(sel_list$PO.109A, na.rm = T) %>% round(1)
round((sum(is.na(sel_list$PO.109A)) / length(sel_list$PO.109A) * 100), 1)


##### table itself

df_1 = as.data.frame(sel_list)

df_1 = df_1 %>% summarize_all(list(~mean(., na.rm = T), 
                                   ~sd(., na.rm = T), 
                                   ~round((sum(is.na(.)) / length(.) * 100),0)))

view(df_1)

mean = df_1 %>% select(contains("_mean")) %>% unname() %>% as.numeric() %>% round(., 1)
sd = df_1 %>% select(contains("_sd")) %>% unname() %>% as.numeric() %>% round(., 1)
na = df_1 %>% select(contains("_round")) %>% unname() %>% as.numeric() 

tab_1 = cbind(mean, sd, na)

tab_1 = tab_1 %>% as.data.frame()

row.names(tab_1) = c(#1 
  "Přírodní katastrofy, např. povodeň, větrná smršt, rozsáhlé požáry atd.", 
                     #2 
  "Epidemie",
                     #3
  "Dlouhodobé výkyvy počasí, např. dlouhodobé sucho, dlouhodobé extrémně vysoké nebo nízké teploty apod.",
                     #4
  "Únik nebezpečných chemických či radioaktivních látek do prostředí", 
                     #5
  "Dlouhodobý nedostatek potravin či pitné vody",
                     #6
  "Rozsáhlý dlouhodobý výpadek dodávek elektrické energie",
                     #7
  "Dlouhodobý nedostatek ropy či plynu",
                     #8
  "Dlouhodobý výpadek internetu, mobilních sítí nebo telefonu",
                     #9
  "Kybernetický, počítačový útok",
                     #10
  "Teroristický útok na místě s vysokým počtem osob",
                     #11
  "Válečný konflikt",
                     #12
  "Rabování a výtržnosti",
                     #13
  "Masová migrace",
                     #14
  "Stárnutí populace",
                     #15
  "Nárust chudoby",
                     #16
  "Krach bankovního sektoru",
                     #17
  "Prohlubování ekonomických rozdílů mezi skupinami obyvatel",
                     #18
  "Šíření konspiračních teorií a dezinformací po internetu",
                     #19
  "Manipulace s informacemi ve veřejnoprávních médiích, tedy v České televizi nebo Českém rozhlase",
                     #20
  "Manipulace s informacemi v soukromých médiích", 
                     #21
  "Prohlubování názorových rozdílů mezi skupinami obyvatel",
                     #22
  "Uchvácení státní moci ze strany úzké skupiny osob",
                     #23
  "Účast extrémistickÝch politických stran ve vládě",
                     #24
  "Účast politických stran prosazujících zájmy nepřátelského státu ve vládě",
                     #25
  "Energetická či jiná hospodářská závislost na nepřátelském státu",
                     #26
  "Technologická závislost státu na nadnárodních společnostech jako jsou Huawei, Facebook, Google apod.")


tab_order = tab_1[order(-mean), ] 

formattable(tab_order)







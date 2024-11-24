######################
#Package Install Function
######################
packages_install = function(library_string){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(library_string, character.only = TRUE)    
  print("Packages fully installed and loaded")
}

######################
#Packages
######################
library_string = c("here", # creating dynamic paths based on script location
                   "haven", # reading/writing of dta files
                   "tidyverse", # data manipulation/wrangeling
                   "doParallel", #parallel processing
                   "foreach"#parallel looping"
)
packages_install(library_string)
######################
#Paths
######################
path = paste0(here(),"/")
readpath = paste0(path,"Output/")
writepath = readpath

######################
#Load and Drop
######################
wm_data = read_dta(paste0(readpath,"repeated_offerings_2022-05-20.dta"))

mpb = wm_data %>% select(
  -denkmalobjekt,
  -rollstuhlgerecht,
  -kaufpreis,
  -heizkosten,
  -bauphase,
  -nutzflaeche,
  -betreut,
  -energieausweistyp,
  -ev_kennwert,
  -energieeffizienzklasse,
  -click_url,
  -ev_wwenthalten,
  -foerderung,
  -parkplatz,
  -gaestewc,
  -ferienhaus,
  -einliegerwohnung,
  -grundstuecksflaeche,
  -kaufvermietet,
  -wohngeld,
  -mieteinnahmenpromonat,
  -haustier_erlaubt,
  -kategorie_Haus,
  -nebenraeume
)%>% filter(
  ajahr >=2010,
  !gid2015 <0,
  !immobilientyp == -9
) 
######################
#Drop first,last percentile
######################
mpb_2 = mpb %>% group_by(ajahr) %>% mutate(
  upper_price_var = quantile(price_var, 0.99),
  lower_price_var = quantile(price_var, 0.01),
)%>% filter(
  price_var >= lower_price_var & price_var <= upper_price_var
)%>% mutate(
  upper_wohnflaeche = quantile(wohnflaeche, 0.99),
  lower_wohnflaeche = quantile(wohnflaeche, 0.01),
)%>% filter(
  wohnflaeche >= lower_wohnflaeche & wohnflaeche <= upper_wohnflaeche
)%>% ungroup()  %>% mutate(
  price_sqm = (price_var / wohnflaeche),
  upper_price_sqm = quantile(price_sqm, 0.99),
  lower_price_sqm = quantile(price_sqm, 0.01),
) %>% filter(
  price_sqm >= lower_price_sqm & price_sqm <= upper_price_sqm
) %>% select(
  !starts_with(c("upper","lower"))
) %>% mutate(
  ln_price_var = log(price_var),
  ln_wohnflaeche = log(wohnflaeche),
  ln_price_sqm = log(price_sqm),
  baujahr_cat = case_when(
    baujahr >=1500 & baujahr <1600 ~ 1,
    baujahr >=1600 & baujahr <1700 ~ 2,
    baujahr >=1700 & baujahr <1800 ~ 3,
    baujahr >=1800 & baujahr <1900 ~ 4,
    baujahr >=1900 & baujahr <2000 ~ 5,
    baujahr >=2000 & baujahr <2020 ~ 6,
    TRUE ~ 0
  ),
  
  age = case_when(
    baujahr == 0 ~ 0,
    TRUE ~ 2019 - baujahr
  ),
  age_squared = age^2
  
)

#missings recode
mpb_2[mpb_2 < 0] = NA
mpb_2$baujahr[mpb_2$baujahr < 1500] = NA
mpb_2$letzte_modernisierung[mpb_2$letzte_modernisierung > 2019] = NA
mpb_2$etage[mpb_2$etage >=15] = NA
mpb_2$anzahletagen[mpb_2$anzahletagen >=15] = NA
mpb_2$zimmeranzahl[mpb_2$anzahletagen >10.5] = NA
mpb_2$zimmeranzahl = round(mpb_2$zimmeranzahl,0)

#gen missing dummys
mpb_3 = mpb_2 %>% mutate(
  baujahrNA = is.na(baujahr),
  ageNA = is.na(age),
  age_squaredNA = is.na(age_squared),
  objektzustandNA = is.na(objektzustand),
  letzte_m_NA = is.na(letzte_modernisierung),
  #this cannot be true since we dropped all beforehand
  etageNA = is.na(etage),
  anzahletagenNA = is.na(anzahletagen),
  zimmeranzahlNA = is.na(zimmeranzahl),
  einbaukuecheNA = is.na(einbaukueche),
  balkonNA = is.na(balkon),
  gartenNA = is.na(garten),
  heizkosten_in_wm_enthaltenNA = is.na(heizkosten_in_wm_enthalten),
  kellerNA = is.na(keller),
  ausstattungNA = is.na(ausstattung),
  heizungsartNA = is.na(heizungsart),
  kategorie_WohnungNA = is.na(kategorie_Wohnung),
  aufzugNA = is.na(aufzug)
) %>% filter(
  letzte_modernisierung >= baujahr,
  !(etage > anzahletagen & etage != 0 & anzahletagen != 0),
  amonths >= 2015*12 + 6
)

mpb_3[is.na(mpb_3)] = 0

mpb_3 = mpb_3 %>% mutate(mpb_treat = case_when(
  objektzustand %in% c(2,3,5,6) & baujahr >= 2015 ~ FALSE,
  TRUE ~ TRUE
))
reg = lm(ln_price_sqm ~ 
           mpb_treat + as.factor(emonths)+ as.factor(obj_parent)
           + age + ageNA + age_squared + wohnflaeche 
           + etage + etageNA 
           + anzahletagen + anzahletagenNA 
           + zimmeranzahl + zimmeranzahlNA 
           + aufzug + aufzugNA 
           + balkon + balkonNA 
           + einbaukueche + einbaukuecheNA 
           + garten + gartenNA
           + keller + kellerNA
           + as.factor(heizungsart)+ heizungsartNA
           + as.factor(ausstattung) + as.factor(objektzustand),data = mpb_3)
summary(reg)  
  
modelsummary(reg, coef_omit = "emoths|obj_parent",output = paste0(writepath,"reg_output.txt"))
  
  
  
  
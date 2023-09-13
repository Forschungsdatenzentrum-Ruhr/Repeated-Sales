make_hedonic <- function(classification = NA) {
  
# Notes:
# Need to remerge to inital RED somewhere to reattach dropped variables 
# that are used during regression (garten etc), use counting_id

  
  
# Variables used ----------------------------------------------------------
  # * variables for apartment rents
  # global varWM "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.keller i.ausstattung zimmeranzahl_full"
  # global depWM "ln_rent_sqm"
  # 
  # * variables for house sales
  # global varHK "i.constr first_occupancy i.gaestewc einliegerwohnung i.ausstattung zimmeranzahl_full i.plotarea_size typ_*"
  # global depHK "ln_houseprice_sqm"
  # 
  # * variables for apartment sales
  # global varWK "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.aufzug i.keller i.betreut i.ausstattung declared_wohngeld zimmeranzahl_full nofloors catfloors"
  # global depWK "ln_flatprice_sqm"
  

# depVar ------------------------------------------------------------------
#   * House sales
#   if "`TYPE'" == "HK" {
#     * logarithmise house prices (0 = missing)
#     replace kaufpreis = 0 if kaufpreis < 0  
#     gen ln_houseprice = ln(kaufpreis) 
#     label var ln_houseprice "log houseprice"
#     
#     * generate houseprices per sqm
#     gen houseprice_sqmeter = kaufpreis / wohnflaeche
#     label var houseprice_sqmeter "price per sqm"
#     
#     * generate log houseprices per sqm
#     gen ${dep`TYPE'} = ln(houseprice_sqmeter)
# 		label var ${dep`TYPE'} "log price per sqm"
#   }
#   * Apartment rents
#   else if "`TYPE'" == "WM" {
#     *logarithmise house prices (0 = missing)
#     replace mietekalt = 0 if mietekalt < 0
#     gen ln_rent = ln(mietekalt) 
#     label var ln_rent "log rent"
#     
#     gen rent_sqmeter = mietekalt / wohnflaeche 
#     label var rent_sqmeter "rent per sqm"
#     
#     gen ${dep`TYPE'} = ln(rent_sqmeter)
# 		label var ${dep`TYPE'} "log rent per sqmeter"
#   }
#   * Apartment sales
#   else if "`TYPE'" == "WK" {
#     * flat price/rent in log (0 = missing)
#     replace kaufpreis = 0 if kaufpreis < 0  
#     gen ln_flatprice = ln(kaufpreis) 
#     label var ln_flatprice "log flatprice"
#     
#     * generate flat prices per sqm
#     gen flatprice_sqmeter = kaufpreis / wohnflaeche
#     label var flatprice_sqmeter "price per sqm"
#     
#     gen ${dep`TYPE'} = ln(flatprice_sqmeter)
# 		label var ${dep`TYPE'} "log price per sqmeter" 

#   ***************************************************************************
#     * create categories for year of construction 
#   gen constr_yr = 1 if baujahr == -5 | baujahr == -9
#   replace constr_yr = 2 if baujahr >0 & baujahr < 1900
#   replace constr_yr = 3 if baujahr >= 1900 & baujahr <= 1945
#   replace constr_yr = 4 if baujahr >= 1946	& baujahr <= 1959
#   replace constr_yr = 5 if baujahr >= 1960	& baujahr <= 1969
#   replace constr_yr = 6 if baujahr >= 1970	& baujahr <= 1979
#   replace constr_yr = 7 if baujahr >= 1980	& baujahr <= 1989
#   replace constr_yr = 8 if baujahr >= 1990	& baujahr <= 1999
#   replace constr_yr = 9 if baujahr >= 2000	& baujahr <= 2009
#   replace constr_yr = 10 if baujahr >= 2010 
#   
#   label define constr_yr 1 "missing" 2 "before 1900" 3 "1900-1945" 4 "1946-1959" 5 "1960-1969" 6 "1970-1979" 7 "1980-1989" 8 "1990-1999" 9 "2000-2009" 10 "after 2009"
#   label values constr_yr constr_yr
#   bys ejahr: tab constr_yr
#   
#   * create dummy variable for first occupancy 
#   gen first_occupancy = objektzustand == 1
#   label define first_occupancy 1 "Yes" 0 "No"
#   label values first_occupancy first_occupancy
#   bys ejahr: tab first_occupancy
#   
#   * missings as zero 
#   replace grundstuecksflaeche = 0 if grundstuecksflaeche < 0
#   
#   ***************************************************************************
#     * more type specific cleaning
#   * House sales
#   if "`TYPE'" == "HK" {
#     * create categories for plot area
#     gen plotarea_size = 0 if grundstuecksflaeche <= 0
#     replace plotarea_size = 1 if grundstuecksflaeche > 0 & grundstuecksflaeche <= 200
#     replace plotarea_size = 2 if grundstuecksflaeche > 200 & grundstuecksflaeche <= 400
#     replace plotarea_size = 3 if grundstuecksflaeche > 400 & grundstuecksflaeche <= 600
#     replace plotarea_size = 4 if grundstuecksflaeche > 600 & grundstuecksflaeche <= 800
#     replace plotarea_size = 5 if grundstuecksflaeche > 800 & grundstuecksflaeche <= 1200
#     replace plotarea_size = 6 if grundstuecksflaeche > 1200
#     label define plotarea_size 0 "missing" 1 "(0-200]" 2 "(200-400]" 3 "(400-600]" 4 "(600-800]" 5 "(800-1200]" 6 ">1200"
#     
#     * create categories for house types
#     * dummy for single family houses (SFH detached, Bungalow, Farmhouse)
#     gen typ_freistehend = 0
#     replace typ_freistehend = 1 if kategorie_Haus == 1 | kategorie_Haus == 7 | kategorie_Haus == 8
#     label var typ_freistehend "1 if house is detached"
#     
#     * dummy for semi-detached single family house (Single-family house, Semi-detached house)
#     gen typ_DHH = 0
#     replace typ_DHH = 1 if kategorie_Haus == 2 | kategorie_Haus == 3
#     label var typ_DHH "1 if house is semi-detached"
#     
#     * dummy for terraced single family house (all categories for terraced house)
#     gen typ_Reihenhaus = 0
#     replace typ_Reihenhaus = 1 if kategorie_Haus == 4 | kategorie_Haus == 5 | kategorie_Haus == 6
#     label var typ_Reihenhaus "1 if house is terraced house"
#     
#     * dummy for exclusive house (Castle, Masion)
#     gen typ_exclusive = 0
#     replace typ_exclusive = 1 if kategorie_Haus == 9 | kategorie_Haus == 10
#     label var typ_exclusive "1 if house is labelled as castle or mansion(Villa)"
#     
#     * dummy for block of flats, although already thrown out, so 0 changes here
#     * two-family house or block of flats
#     gen typ_MFH = 0
#     replace typ_MFH = 1 if kategorie_Haus == 11 | kategorie_Haus == 12
#     label var typ_MFH "1 if house has more than one flat"
#     
#     * dummy for other house type
#     gen typ_other = 0
#     replace typ_other = 1 if kategorie_Haus == 13 | kategorie_Haus == 14 | kategorie_Haus == 15
#     label var typ_other "1 if house is other object"
#   }
#   * Apartment sales
#   else if "`TYPE'" == "WK" {
#     * dummy for betreutes Wohnen
#     replace betreut = 0 if betreut < 0
#     bys ejahr: tab betreut
#     
#     * dummy for Wohngeld to evaluate the transparancy of the offer
#     gen declared_wohngeld = 0
#     replace declared_wohngeld = 1 if wohngeld > 0 & wohngeld < 2500
#     bys ejahr: tab declared_wohngeld
#     
#     * numbers of floors
#     gen nofloors = 1 if anzahletage <= 0
#     replace nofloors = 2 if anzahletage >= 1 & anzahletage <= 3
#     replace nofloors = 3 if anzahletage >= 4 & anzahletage <= 5
#     replace nofloors = 4 if anzahletage >= 6 & anzahletage <= 10
#     replace nofloors = 5 if anzahletage >= 10
#     label define nofloors 1 "missing" 2 "1-3 floors" 3 "4-5 floors" 4 "6-10 floors" 5 "more than 10 floors" 
#     label values nofloors nofloors
#     bys ejahr: tab nofloors
#     
#     * floor of the offered flat
#     gen catfloors = 0 if etage < 0
#     replace catfloors = 1 if etage == 0 
#     replace catfloors = 2 if etage == 1
#     replace catfloors = 3 if etage == 2 | etage == 3
#     replace catfloors = 4 if etage == 4 | etage == 5
#     replace catfloors = 5 if etage >= 6 & etage <= 10
#     replace catfloors = 6 if etage > 10
#     
#     label define catfloors 0 "missing" 1 "ground floor (UG)" 2 "first floor (EG)" 3 "2nd to 3rd floor" 4 "4th to 5th floor" 5 "6th to 10th floor" 6 "above 10th floor"
#     label values catfloors catfloors
#     bys ejahr: tab catfloors
#     
#     * exception of Penthouse living also in multistory building
#     gen penthouse = 0
#     replace penthouse = 1 if kategorie_Wohnung == 7
#     bys ejahr: tab penthouse
#   }
#   
#   ***************************************************************************
#     * missing information
#   
#   * balcony, missing as zero
#   replace balkon = 0 if balkon < 0
#   * garden, missing as zero
#   replace garten = 0 if garten < 0
#   *Kitchen included, missing as zero
#   replace einbaukueche = 0 if einbaukueche < 0
#   *Guest toilet included, missing as zero 
#   replace gaestewc = 0 if gaestewc < 0
#   *Cellar, missing as zero
#   replace keller = 0 if keller < 0 
#   *Elevator available
#   replace aufzug = 0 if aufzug < 0
#   *Interior rating, rename 
#   replace ausstattung = 0 if ausstattung < 0
#   label define ausstattung 0 "missing" 1 "Simple" 2 "Normal" 3 "Sophisticated" 4 "Deluxe"
#   
#   label value ausstattung ausstattung
#   bys ejahr: tab ausstattung
#   
#   bys ejahr: tab balkon 
#   bys ejahr: tab garten 
#   bys ejahr: tab einbaukueche 
#   bys ejahr: tab gaestewc 
#   bys ejahr: tab keller 
#   bys ejahr: tab aufzug
#   
#   replace einliegerwohnung = 0 if einliegerwohnung < 0
#   bys ejahr: tab einliegerwohnung
#   
#   * number of rooms, restrict to more than zero and less than 8, exclude missings
#   replace zimmeranzahl = 0 if zimmeranzahl < 0
#   * downward adjust number of rooms given with decimal points
#   by obid, sort: gen zimmeranzahl_full = floor(zimmeranzahl)
#   label var zimmeranzahl_full "number of rooms, downward adjusted"
#   bys ejahr: tab zimmeranzahl_full
#   
#   duplicates report obid
#   
#   ***************************************************************************
#     * drop extreme values of variables 
#   
#   * Apartment rents
#   if "`TYPE'" == "WM" {
#     drop if zimmeranzahl_full > 7
#     
#     * drop objects with rent price above 5000 and only 0 (or below) for plausability
#     drop if mietekalt > 5000
#     drop if mietekalt == 0
#     
#     * drop objects if living area is uncommonly small or large
#     drop if wohnflaeche < 15 //missings also excluded
#     drop if wohnflaeche > 400
#     drop kaufpreis mieteinnahmenpromonat nutzflaeche nebenraeume wohngeld betreut denkmalobjekt einlieger ferienhaus kaufvermietet bauphase kategorie_Haus
#   }
#   * House sales
#   else if "`TYPE'" == "HK" {
#     drop if zimmeranzahl_full > 15
#     
#     * drop objects with houseprices above 5 Mio
#     drop if kaufpreis > 5000000
#     drop if kaufpreis == 0
#     
#     * drop objects if living area is uncommonly small or large
#     drop if wohnflaeche < 50 //also missing excluded
#     drop if wohnflaeche > 600
#     
#     * drop plot area over 2500 sqm to objects with agrarian use
#     drop if grundstuecksflaeche > 2500
#     
#     * drop if number of floors is uncommonly large to exclude misplaced MFH
#     drop if anzahletage > 5
#     
#     drop mietekalt nebenkosten etage wohngeld betreut foerderung heizkosten_in_wm_enthalten garten haustier_erlaubt kategorie_Wohnung
#   }
#   * Apartment rents
#   else if "`TYPE'" == "WK" {
#     drop if zimmeranzahl_full > 8
#     
#     * drop objects above 2 Mio Euro
#     drop if kaufpreis <= 0
#     drop if kaufpreis > 2000000
#     
#     summarize wohnflaeche, detail
#     
#     * drop living area if smaller than 1% (27 sqm) and larger than 99% (240sqm) of the observations
#     drop if wohnflaeche < r(p1) | wohnflaeche > r(p99)
#     
#     * drop variables of other type (varlist == -8)
#     drop mietekalt mietewarm nebenkosten nebenraeume immobilientyp einliegerwohnung ev_wwenthalten foerderung heizkosten_in_wm_enthalten bauphase haustier_erlaubt kategorie_Haus
#   }
#   
#   sort obid spell
#   *offers with more than one spell
#   bysort obid: gen n = _n
#   bysort obid: gen N = _N
#   keep if n == N
#   
#   count
#   sort ags2019
#   count if ags2019 == . 
#   
#   * decode negative values, that indicate missings
#   mvdecode _all, mv(-9, -8, -7, -5, -3,-2,-1)
#   
#   ***************************************************************************
#     * addin geo information
#   
#   merge m:1 ags2019 using "${ORIG_PATH}locations/ags2gid.dta"
#   drop if _merge == 2
#   
#   
#   *Gebietsreformen NS 2016-18
#   forval year = 2016/2018 {
#     gen double ags__`year' = ags2019 if _merge == 1
# 		merge m:1 ags__`year' using "${REFORM_PATH}GR`year'.dta", gen(mergeGR`year') keepusing(ags__2019)
# 		drop if mergeGR`year' == 2
# 		                                                              replace ags2019 = ags__2019 if mergeGR`year' == 3
# 		drop ags__2019
# 	}
# 
# 	*Gebietsreform 2019
# 	merge m:1 ags__2018 using "${REFORM_PATH}GR2019.dta", gen(mergeGR2019) keepusing(ags__2019)
# 	drop if mergeGR2019 == 2
# 	replace ags2019 = ags__2019 if mergeGR2019 == 3
# 
# 	drop rs-ags_verband
# 	drop ags__2016 - mergeGR2019
# 
# 	merge m:1 ags2019 using "${ORIG_PATH}locations/ags2gid.dta", gen(mergeverband2)
# 	drop if mergeverband2 == 2
# 	display "numbers of the unmatched before and after correction --> badly georef.: _merge == 1 & mergeverband2 == 1 & lon==. "
# 	count if _merge == 1 & mergeverband2 == 1 & lon_gps == .
# 	drop if mergeverband2 <3
# 	drop mergeverband2
# 
# 	drop _merge
# 
# 	* rename VWG
# 	cap drop gid2019
# 	rename ags_verband gid2019
# 	destring gid2019, replace
# 	rename vwg gid2019_name
# 	rename gemeinde_name ags_name
# 
# 	*merge AMRs
# 	gen dg = kid2019
# 	merge m:1 dg using "${SHAPE_PATH}Arbeitsmarktregion/RWI2018/zuordnung2_AMR.dta", keepusing(AMR2)
# 
# 	drop if _merge==2
# 	drop _merge dg
# 
# 	***************************************************************************
# 	* sort out all observations with missings 
# 	reg ${dep`TYPE'} ${var`TYPE'} i.ejahr  i.kid2019 , robust
# 	keep if e(sample) 
# 	bysort ejahr: count
# 
# 	foreach v in e a{
# 		gen `v'quarter = 1 if `v'monat == 1 | `v'monat == 2|`v'monat == 3
# 		replace `v'quarter = 2 if `v'monat == 4 | `v'monat == 5 |`v'monat == 6
# 		replace `v'quarter = 3 if `v'monat == 7 | `v'monat == 8 |`v'monat == 9
# 		replace `v'quarter = 4 if `v'monat == 10 | `v'monat == 11|`v'monat == 12
# 	}
# 	
# 	***************************************************************************
# 	* adding additional variable
# 	
# 	* quarter variable qudate
# 	gen equa = yq(ejahr, equarter)
# 	gen aqua = yq(ajahr, aquarter)
# 	format aqua equa %tq
# 	gen qudate = equa
# 
# 	* month variable 
# 	gen emon = ym(ejahr, emonat)
# 	gen amon = ym(ajahr, amonat)
# 	format amon emon %tm
# 	gen modate = emon
# 
# 	* generate grid ID
# 	egen grid = group(ergg_1km)
# 
# 	***************************************************************************
# 	* export link between grid definitions
# 	preserve
# 	keep grid ergg_1km
# 	duplicates drop 
# 	save "${DATA_PATH}ergg2grid_`TYPE'.dta", replace 
# 	restore
# 
# 	***************************************************************************
# 	* adding variables for regression
# 	
# 	foreach region of varlist kid2019 gid2019 AMR2 grid{
# 		foreach time of varlist ejahr qudate modate{
# 			*generate interatction effects
# 			egen id_`region'_`time' = group(`region' `time')
# 			sum id_`region'_`time'
# 
# 			*generate mean houseprice per sqm, per year and per district
# 			bysort `region' `time': egen m${price`TYPE'}sqm_`region'_`time' = mean(${price`TYPE'}_sqmeter)
# 			label var m${price`TYPE'}sqm_`region'_`time' "mean of ${price`TYPE'} per `region' and`time'"
# 		}
# 	}
# 	
# 	***************************************************************************
# 	* export and finish
# 	
# 	*reduce memory used 
# 	compress
  
}
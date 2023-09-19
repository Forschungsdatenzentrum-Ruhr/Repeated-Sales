make_hedonic_HK = function(classification = NA){
  
  ## Setup
  # * variables for house sales
  # global varHK "i.constr first_occupancy i.gaestewc einliegerwohnung i.ausstattung zimmeranzahl_full i.plotarea_size typ_*"
  # global depHK "ln_houseprice_sqm"
  #
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
  # indepVar ----------------------------------------------------------------
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
  #     * drop extreme values of variables 
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
}

# ### house type ----
# # the combination of categories follows 'Klick & Schaffner' (2019)
# purchases[, house_type := fcase(
#   house_type == -7 | house_type == -9,           0L,
#   house_type == 1 | house_type == 2,             1L,
#   house_type == 11 | house_type == 12,           2L,
#   house_type == 3L,                              3L,
#   between(house_type, 4, 6),                     4L,
#   house_type == 13 | house_type == 15,           5L,
#   between(house_type, 7, 10) | house_type == 14, 6L,
#   house_type == special_code,                    special_code # 0-6 are for homes, `special_code` is for flats (WK_SUF)
#   # --a special type by construction
# )]
# purchases[, house_type := factor(
#   house_type, c(0L:6L, special_code),
#   c(
#     "na",            # -9 (Sonstiges Missing) + -7 (Keine Angabe)
#     "single-family", #  1 Single-family house (detached) + 2 Single-family house
#     "two-family",    # 11 two-family houses + 12 block of flats
#     "semi-detached", # itself, 3 semi-detached
#     "terraced",      # 4 terraced + 5 terraced (middle unit) + 6 terraced (end unit)
#     "other",         # 13 other property for living + 15 other
#     "special",       # 7 Bungalow + 8 Farmhouse + 9 Castle + 10 Mansion + 14 Special property
#     "apartments"     # the above are house types, this is for flats (WK_SUF)
#   )
# )]
# 
# 
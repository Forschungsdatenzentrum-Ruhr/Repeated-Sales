make_hedonic_WM = function(classification = NA){
  
  ## Setup
  # * variables for apartment rents
  # global varWM "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.keller i.ausstattung zimmeranzahl_full"
  # global depWM "ln_rent_sqm"
  # 
  # depVar ------------------------------------------------------------------
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
  #   

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
  
}
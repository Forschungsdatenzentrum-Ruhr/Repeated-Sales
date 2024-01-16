make_hybrid = function(RED_classified,self_merged_rs_pairs, data_type){
  #tar_load(RED_classified); tar_load_globals()
  list_var = make_var(data_type = data_type)
  indepVar = list_var$indepVar
  depVar = list_var$depVar
  # think of a solution for this, they are mutated in prepare_hedonic
  var_to_keep = c(indepVar,"rs_id","emonths","depVar")
# one consideration is that we have to decide between using or dropping updates from hedonic as well

# build by me based on Case and Quigley 1991
# get ids of all listings that are classified as repeat sales (pure or changed)
all_rs = self_merged_rs_pairs[["rs_id"]] |> unique()
# split into repeat and hedonic
RED_classified = prepare_hedonic(RED_classified, data_type)[,":="(
  hybrid_type = fifelse(rs_id %in% all_rs, "repeat", "hedonic"),
  depVar = exp(get(depVar))
)
]


# to split repeat into pure and changed, figure out which listings have changed within id
# this means however that between pairs quality changed, so for that listing pair

# reduce listings to only repeats and set missings to zero
pure_rs = RED_classified[
    hybrid_type == "repeat",
    ..var_to_keep
    ]

changed_boolean = pure_rs[,
  lapply(.SD, function(x){c(NA,diff(x))}),
  by = rs_id,
  .SDcols = setdiff(var_to_keep,c("rs_id","emonths","depVar"))
][,rs_id := NULL] |> rowSums() != 0

is.na(changed_boolean) = FALSE

tar_assert_true(length(changed_boolean) == nrow(pure_rs))

pure_rs[, changed_to := changed_boolean][, changed_from := lead(changed_to,1), by = rs_id]

binary_names = c("balkon","garten","einbaukueche","gaestewc","aufzug","keller","betreut","ausstattung","declared_wohngeld", "baujahr_cat", "first_occupancy", "num_floors", "floors_cat")
cont_names = c("zimmeranzahl")
#"ausstattung",


# this pretty much allows for duplicate indiviudal listings between pure/changed
# sample 1 pure rs
pure_pairs = pure_rs[changed_to == FALSE | changed_from == FALSE]

# sample 2 quality changed rs
changed_pairs = pure_rs[changed_to == TRUE | changed_from == TRUE]

# smaple 3 hedonic
hedonic_listings = RED_classified[hybrid_type == "hedonic", ..var_to_keep]





# type specific setups, mostly for readability
# this is incredibly ugly, refactor it later
# hedonic
hedonic_V = hedonic_listings[["depVar"]]
hedonic_t_month = hedonic_listings[["emonths"]]

# pure
pure_V_t = pure_pairs[["depVar"]]
pure_V_T = pure_pairs[,lag(depVar,1), by = "rs_id"][,rs_id := NULL][["V1"]]
pure_t_month = pure_pairs[["emonths"]]
pure_T_month = pure_pairs[,lag(emonths,1), by = "rs_id"][,rs_id := NULL][["V1"]]
# changed
changed_V_t = changed_pairs[["depVar"]]
changed_V_T = changed_pairs[,lag(depVar,1), by = "rs_id"][,rs_id := NULL][["V1"]]
changed_t_month = changed_pairs[["emonths"]]
changed_T_month = changed_pairs[,lag(emonths,1), by = "rs_id"][,rs_id := NULL][["V1"]]


Z = do.call(rbind, list(
# hedonic
X_1 = make_X_1(hedonic = hedonic_listings, x_conts = cont_names, x_binaries = binary_names, t_month = hedonic_t_month),
# pure
X_2 = make_X_2(pure = pure_pairs, x_conts = cont_names, x_binaries = binary_names, t_month = pure_t_month, T_month = pure_T_month),
# changed
X_3 = make_X_3(
    changed = changed_pairs,
    x_conts = cont_names, 
    x_binaries = binary_names, 
    t_month = changed_t_month, 
    T_month = changed_T_month
)
)) 


Y = log(
    c(
        hedonic_V,
        (pure_V_t/pure_V_T), 
        (changed_V_t/changed_V_T)
    )
)

#beta = crossprod(Z)
#beta = qr.coef(qr(Z), Y)
# or
test = cbind(Z,Y) |> na.omit()
summary(test)
beta = lm(Y ~ ., data = test)$coefficients
#reg = lm(Y ~ Z)
#summary(reg)
 return(beta)
}
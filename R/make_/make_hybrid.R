make_hybrid = function(RED_classified,self_merged_rs_pairs, data_type){

  list_var = make_var(data_type = data_type)
  depVar = list_var$depVar
  indepVar = list_var$indepVar
  # think of a solution for this, they are mutated in prepare_hedonic
  var_to_keep = c(setdiff(indepVar,c("declared_wohngeld", "baujahr_cat", "first_occupancy", "num_floors", "floors_cat")),"rs_id")

# one consideration is that we have to decide between using or dropping updates from hedonic as well

# build by me based on Case and Quigley 1991
# get ids of all listings that are classified as repeat sales (pure or changed)
all_rs = self_merged_rs_pairs[["rs_id"]] |> unique()
# split into repeat and hedonic
RED_classified[,hybrid_type := fifelse(rs_id %in% all_rs, "repeat", "hedonic")]

# to split repeat into pure and changed, figure out which listings have changed within id
# this means however that between pairs quality changed, so for that listing pair

# reduce listings to only repeats and set missings to zero
pure_rs = RED_classified[
    hybrid_type == "repeat",
    ..var_to_keep
    ][,
    (var_to_keep) := lapply(.SD, 
    function(x) {
      fifelse(x < 0, 0, x)
    }),
    .SDcols = var_to_keep
  ]

changed_boolean = pure_rs[,
  lapply(.SD, function(x){c(NA,diff(x))}),
  by = rs_id,
  .SDcols = setdiff(var_to_keep,"rs_id")
][,rs_id := NULL] |> rowSums() != 0

tar_assert_true(length(changed_boolean) == nrow(pure_rs))

pure_rs[,changed_to := changed_boolean][, changed_from := lead(changed_to,1), by = rs_id]

# sample 1 pure rs
pure_pairs = pure_rs[changed_to == FALSE | changed_from == FALSE]

# sample 2 quality changed rs
changed_pairs = pure_rs[changed_to == TRUE | changed_from == TRUE]

# smaple 3 hedonic
hedonic_listings = RED_classified[hybrid_type == "hedonic", ..var_to_keep]

binary_names = c("balkon","garten","einbaukueche","gaestewc","aufzug","keller","betreut")
cont_names = c("ausstattung","zimmeranzahl")

make_X_1 = function(x_conts = NA , x_binaries = NA, t_month = NA){
    x_conts = unlist(x_conts)
    x_binaries = unlist(x_binaries)
    
    x_conts_pre = log(x_conts)
    x_conts_sub = t_month * x_conts_pre

    x_binaries_pre = x_binaries
    x_binaries_sub = t_month * x_binaries_pre

    X_1 = c(1, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   
    
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1

    tar_assert_true(length(X_1) == total_length)
    return(X_1)
}
make_X_2 = function(x_conts = NA , x_binaries = NA, t_month = NA, T_month = NA){
    x_conts = unlist(x_conts)
    x_binaries = unlist(x_binaries)

    x_conts_pre = rep(0,length(x_conts))
    x_conts_sub = (t_month-T_month) * log(x_conts)

    x_binaries_pre = rep(0,length(x_binaries))
    x_binaries_sub = (t_month-T_month) * x_binaries

    X_2 = c(0, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   
    
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1

    tar_assert_true(length(X_2) == total_length)
    return(X_2)
}
make_X_3 = function(x_conts = NA, x_star_conts = NA , x_binaries = NA, x_star_binaries = NA, t_month = NA, T_month = NA){
    x_conts = unlist(x_conts)
    x_star_conts = unlist(x_star_conts)
    x_binaries = unlist(x_binaries)
    x_star_binaries = unlist(x_star_binaries)

    x_conts_pre = log(x_star_conts/x_conts)
    x_conts_sub = (t_month* log(x_star_conts)) - (T_month* log(x_conts))

    x_binaries_pre = x_star_binaries - x_binaries
    x_binaries_sub = (t_month* x_star_binaries) - (T_month* x_binaries)

    X_3 = c(0, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   

    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1
    tar_assert_true(length(X_3) == total_length)
    return(X_3)
}

# # make some example data
# # order is hedonic, unchanged rs, changed rs
# x_1 = c(4,5,6) # cont
# x_star_1 = x_1 + c(1,0,1)
# x_2 = c(2,3,4) # cont
# x_star_2 = x_2 + c(1,0,1)
# x_3 = c(1,0,1) # binary
# x_star_3 = x_3 + c(-1,0,-1)

# # hedonic
# V_h_t = 50 
# h_t = 150
# # unchanged_rs
# V_urs_t = 100
# V_urs_T = 180
# urs_t = 200
# urs_T = 210

# # changed_rs
# V_crs_t = 200
# V_crs_T = 150
# crs_t = 180
# crs_T = 185



Y = log(
    c(
        V_h_t, 
        (V_urs_t/V_urs_T), 
        (V_crs_t/V_crs_T)
    )
)
Z = do.call(rbind, list(
    make_X_1(x_conts = list(x_1,x_2), x_binaries = list(x_3), t_month = h_t),
    make_X_2(x_conts = list(x_1,x_2), x_binaries = list(x_3), t_month = urs_t, T_month = urs_T),
    make_X_3(x_conts = list(x_1,x_2), x_star_conts = list(x_star_1,x_star_2), x_binaries = list(x_3), x_star_binaries = list(x_star_3), t_month = crs_t, T_month = crs_T)
)
) #|> as.data.frame(col.names = )
#beta = crossprod(Z)
beta = qr.coef(qr(Z), Y)
# or
#reg = lm(Y ~ Z -1)
#summary(reg)
 return(beta)
}
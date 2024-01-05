make_X_1 = function(hedonic = NA, x_conts = NA , x_binaries = NA, t_month = NA){

    x_conts = full_data[,..x_conts]
    x_binaries = full_data[,..x_binaries]

    x_conts_pre = log(x_conts)
    x_conts_sub = t_month * x_conts_pre 

    x_binaries_pre = x_binaries
    x_binaries_sub = t_month * x_binaries_pre
    
    X_1 = data.table(constant = rep(1,nrow(hedonic)))
    X_1 = cbind(X_1,x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)     
    total_length = sum(length(c(x_conts, x_binaries)) * 2) + 1

    tar_assert_true(length(X_1) == total_length)
    return(X_1)
}
make_X_2 = function(pure = NA, x_conts = NA , x_binaries = NA, t_month = NA, T_month = NA){

    x_conts = pure[,..x_conts]
    x_binaries = pure[,..x_binaries]

    x_conts_pre = matrix(0,length(x_conts),nrow(x_conts)) |> as.data.table()
    x_conts_sub = log((t_month-T_month) * log(xconts))
    
    x_binaries_pre = matrix(0,length(x_binaries),nrow(x_binaries)) |> as.data.table()
    x_binaries_sub = (t_month-T_month) * x_binaries

    X_2 = data.table(constant = rep(0,nrow(pure)))
    X_2 = c(X_2, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   
    
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1

    tar_assert_true(length(X_2) == total_length)
    return(X_2)
}
make_X_3 = function(changed = NA, x_conts = NA, x_binaries = NA, t_month = NA, T_month = NA){

    x_conts = changed[,..x_conts]
    x_star_conts = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_conts][,rs_id := NULL]
    x_binaries = changed[,..x_binaries]
    x_star_binaries = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_binaries][,rs_id := NULL]

    x_conts_pre = log(x_star_conts/x_conts)
    x_conts_sub = (t_month* log(x_star_conts)) - (T_month* log(x_conts))

    x_binaries_pre = x_star_binaries - x_binaries
    x_binaries_sub = (t_month* x_star_binaries) - (T_month* x_binaries)

    X_3 = data.table(constant = rep(0,nrow(changed)))
    X_3 = cbind(X_3, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   

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
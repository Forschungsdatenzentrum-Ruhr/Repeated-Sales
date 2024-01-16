make_X_1 = function(hedonic = NA, x_conts = NA , x_binaries = NA, t_month = NA){

    x_conts = hedonic[,..x_conts]
    x_binaries = hedonic[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]
    x_conts_pre = log(x_conts)
    x_conts_sub = t_month * x_conts_pre 

    x_binaries_pre = x_binaries
    x_binaries_sub = t_month * x_binaries_pre
    
    X_1 = data.table(constant = rep(1,nrow(hedonic)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_1 = cbind(X_1,x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)     
    total_length = sum(length(c(x_conts, x_binaries)) * 2) + 1

    tar_assert_true(length(X_1) == total_length, msg = paste0(length(X_1), " != ", total_length))
    return(X_1)
}
make_X_2 = function(pure = NA, x_conts = NA , x_binaries = NA, t_month = NA, T_month = NA){

    x_conts = pure[,..x_conts]
    x_binaries = pure[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]

    x_conts_pre = matrix(0,nrow(x_conts), length(x_conts)) |> as.data.table()
    setnames(x_conts_pre, names(x_conts))
    x_conts_sub = (t_month-T_month) * log(x_conts)

    x_binaries_pre = matrix(0,nrow(x_binaries), length(x_binaries)) |> as.data.table()
    setnames(x_binaries_pre, names(x_binaries))
    x_binaries_sub = (t_month-T_month) * x_binaries

    X_2 = data.table(constant = rep(0,nrow(pure)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_2 = cbind(X_2, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)    
    
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1

    tar_assert_true(length(X_2) == total_length, msg = paste0(length(X_2), " != ", total_length))
    return(X_2)
}
make_X_3 = function(changed = NA, x_conts = NA, x_binaries = NA, t_month = NA, T_month = NA){

    x_star_conts = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_conts][,rs_id := NULL]
    x_conts = changed[,..x_conts]
    
    x_star_binaries = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_binaries][,rs_id := NULL][,lapply(.SD, function(x){as.numeric(x)-1})]  
    x_binaries = changed[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]

    x_conts_pre = log(x_star_conts/x_conts)
    x_conts_sub = (t_month* log(x_star_conts)) - (T_month* log(x_conts))

    x_binaries_pre = x_star_binaries - x_binaries
    x_binaries_sub = (t_month* x_star_binaries) - (T_month* x_binaries)

    X_3 = data.table(constant = rep(0,nrow(changed)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_3 = cbind(X_3, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub) 

    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1
    tar_assert_true(length(X_3) == total_length, msg = paste0(length(X_3), " != ", total_length))
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
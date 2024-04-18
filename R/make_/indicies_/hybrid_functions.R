# These are helper functions to construct the regression strucutre outlined in Case and Quigley 1991
# generally it decomposes the price indice into three components: X_1, X_2, X_3,
# where each component deals with different data (hedonic, pure repeated sales, changed repeated sales).
# This also entails that the data is transformed in different ways for each component, i.e., dummies/binaries and continous variables are treated differently.

make_X_1 = function(hedonic, x_conts , x_binaries, t_month){
    # Case and Quigley 1991 -> hedonic (X_1)

    # transform variables
    x_conts = hedonic[,..x_conts]
    x_binaries = hedonic[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]

    # make continous
    x_conts_pre = log(x_conts)
    x_conts_sub = t_month * x_conts_pre 

    # make binaries
    x_binaries_pre = x_binaries
    x_binaries_sub = t_month * x_binaries_pre
    
    # combine into X_1
    X_1 = data.table(constant = rep(1,nrow(hedonic)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_1 = cbind(X_1,x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)  

    # Unit test   
    total_length = sum(length(c(x_conts, x_binaries)) * 2) + 1
    tar_assert_true(length(X_1) == total_length, msg = paste0(length(X_1), " != ", total_length))

    #------------------------------------------------
    return(X_1)
}
make_X_2 = function(pure, x_conts , x_binaries, t_month, T_month){
    # Case and Quigley 1991 -> pure repeated sales (X_2)

    # transform variables
    x_conts = pure[,..x_conts]
    x_binaries = pure[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]

    # make continous
    x_conts_pre = matrix(0,nrow(x_conts), length(x_conts)) |> as.data.table()
    setnames(x_conts_pre, names(x_conts))
    x_conts_sub = (t_month-T_month) * log(x_conts)

    # make binaries
    x_binaries_pre = matrix(0,nrow(x_binaries), length(x_binaries)) |> as.data.table()
    setnames(x_binaries_pre, names(x_binaries))
    x_binaries_sub = (t_month-T_month) * x_binaries

    # combine into X_2
    X_2 = data.table(constant = rep(0,nrow(pure)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_2 = cbind(X_2, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)    
    
    # Unit test
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1
    tar_assert_true(length(X_2) == total_length, msg = paste0(length(X_2), " != ", total_length))

    #------------------------------------------------
    return(X_2)
}
make_X_3 = function(changed, x_conts, x_binaries, t_month, T_month){
    # Case and Quigley 1991 -> changed repeated sales (X_3)
    
    # transform variables
    x_star_conts = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_conts][,rs_id := NULL]
    x_conts = changed[,..x_conts]
    x_star_binaries = changed[,lapply(.SD,function(x){lag(x,1)}), by = "rs_id", .SDcols = x_binaries][,rs_id := NULL][,lapply(.SD, function(x){as.numeric(x)-1})]  
    x_binaries = changed[,..x_binaries][,lapply(.SD, function(x){as.numeric(x)-1})]

    # make continous
    x_conts_pre = log(x_star_conts/x_conts)
    x_conts_sub = (t_month* log(x_star_conts)) - (T_month* log(x_conts))

    # make binaries
    x_binaries_pre = x_star_binaries - x_binaries
    x_binaries_sub = (t_month* x_star_binaries) - (T_month* x_binaries)

    # combine into X_3
    X_3 = data.table(constant = rep(0,nrow(changed)))
    setnames(x_conts_pre, paste0("pre_",names(x_conts_pre)))
    setnames(x_binaries_pre, paste0("pre_",names(x_binaries_pre)))
    setnames(x_conts_sub, paste0("sub_",names(x_conts_sub)))
    setnames(x_binaries_sub, paste0("sub_",names(x_binaries_sub)))
    X_3 = cbind(X_3, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub) 

    # Unit test
    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1
    tar_assert_true(length(X_3) == total_length, msg = paste0(length(X_3), " != ", total_length))

    #------------------------------------------------
    return(X_3)
}
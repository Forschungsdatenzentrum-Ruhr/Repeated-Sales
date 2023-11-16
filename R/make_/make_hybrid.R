make_hybrid = function(RED_classified = NA){

# sample 1 pure rs
# sample 2 quality changed rs
# smaple 3 hedonic
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

    x_conts_pre = log(x_star_consts/x_consts))
    x_conts_sub = (t_month* log(x_star_consts)) - (T_month* log(x_consts))

    x_binaries_pre = x_star_consts - x_consts
    x_binaries_sub = (t_month* x_star_consts) - (T_month* x_consts)

    X_3 = c(0, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   
    
    total_length = sum(sapply(append(x_conts,x_binaries),length) * 2) + 1

    tar_assert_true(length(X_3) == total_length)
    return(X_3)
}

# make some example data
x_1 = runif(2) # cont
x_star_1 = x_1 + runif(2)
x_2 = runif(2) # cont
x_star_2 = x_2 + runif(2)
x_3 = c(0,1) # binary
x_star_3 = as.numeric(!x_3)
V_t = c(100,200) # prev_price_var
t = c(200,300) # prev_date
T = c(210,320) # date
V_T = c(200,250) # price_var




}
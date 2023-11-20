make_hybrid = function(RED_classified = NA){
# build by me based on Case and Quigley 1991
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

    x_conts_pre = log(x_star_conts/x_conts)
    x_conts_sub = (t_month* log(x_star_conts)) - (T_month* log(x_conts))

    x_binaries_pre = x_star_binaries - x_binaries
    x_binaries_sub = (t_month* x_star_binaries) - (T_month* x_binaries)

    X_3 = c(0, x_conts_pre, x_binaries_pre, x_conts_sub, x_binaries_sub)   

    total_length = sum(length(c(x_conts,x_binaries)) * 2) + 1
    tar_assert_true(length(X_3) == total_length)
    return(X_3)
}

# make some example data
# order is hedonic, unchanged rs, changed rs
x_1 = c(4,5,6) # cont
x_star_1 = x_1 + c(1,0,1)
x_2 = c(2,3,4) # cont
x_star_2 = x_2 + c(1,0,1)
x_3 = c(1,0,1) # binary
x_star_3 = x_3 + c(-1,0,-1)

# hedonic
V_h_t = 50 
h_t = 150
# unchanged_rs
V_urs_t = 100
V_urs_T = 180
urs_t = 200
urs_T = 210

# changed_rs
V_crs_t = 200
V_crs_T = 150
crs_t = 180
crs_T = 185



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
reg = lm(Y ~ Z -1)
summary(reg)
}
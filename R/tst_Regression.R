# 
# # estimation ----
# makeFormula = function(.data, depvar = dep_var, env = parent.frame()) {
#   avail = names(.data)
#   rhs = c(num_vars, setdiff(cats, fixeffs)) |>
#     intersect(avail) |>
#     paste(collapse = " + ")
#   f = sprintf("%s ~ %s | %s",
#               depvar, rhs, paste(intersect(fixeffs, avail), collapse = "^")
#   )
#   as.formula(f, env = env)
# }
# 
# mod = feols(makeFormula(x), x, combine.quick = FALSE, mem.clean = TRUE)
# 

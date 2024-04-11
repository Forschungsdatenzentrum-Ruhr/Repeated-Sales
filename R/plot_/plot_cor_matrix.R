p_cor_matrix = function(split_index){
  split_index = WK_split_index
  index_names = split_index[, .(index_type)] |> unique()
  tst = data.table::dcast(split_index, "date_quarter + gid2019 ~ index_type", value.var = "mean_index") 
  
  cor_matrix = Hmisc::rcorr(as.matrix(tst[,..index_names]))
}
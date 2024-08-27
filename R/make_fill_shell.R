make_fill_shell = function(conf) {

  row_tb = make_dim_tb(conf[["rows"]], 'row_num')
  col_tb = make_dim_tb(conf[["cols"]], 'col_num')

  fill_shell_seed = tidyr:: crossing(row_num = row_tb$row_num,
                                     col_num = col_tb$col_num)

  fill_shell = dplyr::inner_join(fill_shell_seed, row_tb, by = 'row_num')
  fill_shell = dplyr::inner_join(fill_shell, col_tb, by = 'col_num')

  fill_shell = dplyr::arrange(fill_shell, row_num, col_num)
  dplyr::mutate(fill_shell, shell_row_id = dplyr::row_number())
}


make_dim_tb = function(dims, dim_name) {

  if (is.integer(dims)) {
    cat_tb = data.frame('idx_num' = dims)
  } else {
    cat_tb = make_cat_tb(dims)
    cat_tb = purrr::reduce(cat_tb, dplyr::inner_join, by = 'idx_num')
  }
  update_name(cat_tb, 'idx_num', dim_name)
}

make_cat_tb = function(dims) {
  subcat_tbs = purrr::map(dims, purrr::imap_dfr, make_subcat_tb)
  subcat_tbs = purrr::imap(subcat_tbs, update_name, old_name = 'subcat_val')
  subcat_tbs
}

make_subcat_tb = function(idx_num, subcat_val){

  idx_num = idx_as_int(idx_num)
  tibble::tibble(idx_num = idx_num, subcat_val = subcat_val)

}

update_name = function(x, old_name, new_name) {
  names(x)[names(x) == old_name] = new_name
  x
}

idx_as_int = function(idx_num) {
  idx_num = stringr::str_split(idx_num, " ")
  idx_num = unlist(idx_num, use.names = FALSE)
  idx_num = as.integer(idx_num)
  idx_num
}

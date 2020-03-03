
df <- df_dailyfiles_all

df %>% 
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>%  
  #dplyr::group_by(type) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(
    cases_lag = dplyr::lag(cases),
    diff = cases - cases_lag
  )


df <- df_dailyfiles %>% 
  dplyr::rename(
    type_x = type,
    cases = n
  ) %>% 
  dplyr::mutate(
    type = dplyr::case_when(
      grepl("rawdata_", type_x) ~ "rawdata",
      grepl("snapshot_", type_x) ~ "snapshot",
      TRUE ~ "other"
    )
  )


df %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::filter(type == type_filter) %>%
  dplyr::pull(cases) %>%
  sum(na.rm = TRUE)

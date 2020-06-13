## code to prepare `accident` dataset goes here

accident = list()
for(year in seq(2013, 2015)){
  accident[[as.character(year)]] = fars_read(file.path('data-raw', make_filename(year)))
}

usethis::use_data(accident, overwrite = TRUE)

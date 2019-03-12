
.datatable.aware=TRUE #You have to declare this at https://github.com/tidyverse/dplyr/issues/548

# This function takes in a dataframe with two strings, a and b, and then calculates string distance features on them.

toponym_add_distances_dt <- function(dt, fromscratch=F, nthread=parallel::detectCores()) {
  dt <- data.table::as.data.table(dt)

  print("1 of 24")
  dt[, First_Mistmatch := MeasuringLandscape:::firstmismatch(a, b, verbose = F)]
  print("2 of 24")
  dt[, Jaro := stringdist::stringsim(a, b, "jw", nthread = nthread), ]
  print("3 of 24")
  dt[, Optimal_String_Alignment := stringdist::stringsim(a, b, "osa", nthread = nthread), ]
  print("4 of 24")
  dt[, Levenshtein := stringdist::stringsim(a, b, "lv", nthread = nthread), ]
  print("5 of 24")
  dt[, Damerau_Levenshtein := stringdist::stringsim(a, b, "dl", nthread = nthread), ]
  print("6 of 24")
  dt[, Longest_Common_Substring := stringdist::stringsim(a, b, "lcs", nthread = nthread), ]
  print("7 of 24")
  dt[, q_gram_1 := stringdist::stringsim(a, b, "qgram", nthread = nthread, q = 1), ]
  print("8 of 24")
  dt[, q_gram_2 := stringdist::stringsim(a, b, "qgram", nthread = nthread, q = 2), ]
  print("9 of 24")
  dt[, q_gram_3 := stringdist::stringsim(a, b, "qgram", nthread = nthread, q = 3), ]
  print("10 of 24")
  dt[, q_gram_4 := stringdist::stringsim(a, b, "qgram", nthread = nthread, q = 4), ]
  print("11 of 24")
  dt[, q_gram_5 := stringdist::stringsim(a, b, "qgram", nthread = nthread, q = 5), ]
  print("12 of 24")
  dt[, Cosine_1 := stringdist::stringsim(a, b, "cosine", nthread = nthread, q = 1), ]
  dt[, Cosine_2 := stringdist::stringsim(a, b, "cosine", nthread = nthread, q = 2), ]
  dt[, Cosine_3 := stringdist::stringsim(a, b, "cosine", nthread = nthread, q = 3), ]
  dt[, Cosine_4 := stringdist::stringsim(a, b, "cosine", nthread = nthread, q = 4), ]
  dt[, Cosine_5 := stringdist::stringsim(a, b, "cosine", nthread = nthread, q = 5), ]
  print("13 of 24")
  dt[, Jaccard := stringdist::stringsim(a, b, "jaccard", nthread = nthread), ]
  print("14 of 24")
  dt$a_nchar <- nchar(dt$a)
  print("15 of 24")
  dt$b_nchar <- nchar(dt$b)
  print("16 of 24")
  dt$ab_nchar_diff <- abs(dt$a_nchar - dt$b_nchar)
  print("17 of 24")
  dt[, dJaro := stringdist::stringdist(a, b, "jw", nthread = nthread), ]
  print("18 of 24")
  dt[, dOptimal_String_Alignment := stringdist::stringdist(a, b, "osa", nthread = nthread), ]
  print("19 of 24")
  dt[, dLevenshtein := stringdist::stringdist(a, b, "lv", nthread = nthread), ]
  print("20 of 24")
  dt[, dDamerau_Levenshtein := stringdist::stringdist(a, b, "dl", nthread = nthread), ]
  print("21 of 24")
  dt[, dLongest_Common_Substring := stringdist::stringdist(a, b, "lcs", nthread = nthread), ]
  print("22 of 24")
  dt[, dq_gram := stringdist::stringdist(a, b, "qgram", nthread = nthread), ]
  print("23 of 24")
  dt[, dCosine := stringdist::stringdist(a, b, "cosine", nthread = nthread), ]
  print("24 of 24")
  dt[, dJaccard := stringdist::stringdist(a, b, "jaccard", nthread = nthread), ]


  return(dt)
}

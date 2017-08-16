setWidthTwoColumns(176)


colors.status <- c('failed' = "#d7191c",
                   'poor' = "#fdae61",
                   'acceptable' = '#2b83ba',
                   'good' =   "#abdda4")

commonOrder <- function(d){
  orders <- list("eqa" = c("Instand 100", "Instand 800", "RfB KS",  "RfB GL"),
                 "seGrp" = c("new", "intermediate", "experienced"),
                 "extraEqa" = c("none", "Instand 100", "Instand 800", 
                                "RfB KS", "RfB GL")
  )
  
  for(coln in names(orders)){
    if (coln %in% colnames(d)){
      varRhs <- quo(UQ(sym(coln)))
      varLhs <- quo_name(UQ(sym(coln)))
      
      ord <- intersect(orders[[coln]], levels(d[[coln]]))
      d <- d %>% mutate(UQ(varLhs) := fct_relevel(UQ(varRhs), ord))
    }
  }  
  d
}



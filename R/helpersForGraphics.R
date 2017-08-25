setWidthTwoColumns(176)
setWidthOneColumn(85)


colors.status <- c('failed' = "#d7191c",
                   'poor' = "#fdae61",
                   'acceptable' = '#2b83ba',
                   'good' =   "#abdda4")

commonOrder <- function(d){
  orders <- list("eqa" = c("Instand 100", "RfB KS", "Instand 800", "RfB GL"),
                 "seqGrp" = c("new", "intermediate", "experienced"),
                 "extraEqa" = c("none", "Instand 100", "RfB KS", 
                                "Instand 800", "RfB GL", 'CL',  'POCT'),
                 "status.prev" = c("failed", "poor", "acceptable", "good"),
                 "type" = c("POCT", "central lab")
  )
  
  concatOrders <- function(o){
    paste0(o, orders[[o]])
  }
  
  vOrder <- c(concatOrders("extraEqa"),
              concatOrders("seqGrp"), 
              'seq',
              concatOrders("status.prev")
              )
  
  orders <- append(orders, list("var" = vOrder))
  
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



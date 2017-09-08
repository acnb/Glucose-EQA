## functions ----

library(mice)

oddsLabels <- c('seq' = 'number of previous participations',
                'centralLabcentralLab' = 'has central lab',
                'poct' = 'does POCT',
                'none' = 'no additional EQA',
                'status.prevgood' = '"good" last result', 
                'status.prevfailed' = '"failed" last result',
                'status.prevpoor' = '"poor" last result',
                'status.prevacceptable' = '"acceptable" last result',
                'seqGrpnew' = '"new" participant',
                'seqGrpintermediate' = '"intermediate" experience',
                'seqGrpexperienced' = '"experienced" participant',
                'poctPOCT' = 'has POCT',
                'extraEqanone' = 'no additional EQA',
                'extraEqaPOCT' = 'additional POCT EQA',
                'extraEqaCL' = 'additional CL EQA')

replaceCLNames <- function(str){
  str <- case_when(str_detect(str, 'Hexokinase --') ~ paste(str, '[HK]'),
                   str_detect(str, 'Glucose oxidase/PAP --') ~ 
                     paste(str, '[GO/PAP]'),
                   str_detect(str, 'Glucose oxidase/H2O2-electrode -- ') ~ 
                     paste(str, '[GO/H2O2]'), 
                   TRUE ~ str)
  
  str <- str_replace_all(str, c('Hexokinase -- ' = "", 
                         'Glucose oxidase/PAP -- ' ="",
                         'Glucose oxidase/H2O2-electrode -- ' =""))
  
  str <- ifelse(str_length(str) > 30, 
                          paste(strwrap(str, 30), collapse ="\n"),
                          str)
  str
}


calcOdds <- function(data, x, y){
  data <- as.data.frame(data)
  data <- data[,c(x,y, 'eqa')] 
  data <- data[complete.cases(data),] %>% droplevels()
  o <- ddply(data,
             c('eqa'), function(d){
               form <- as.formula(paste0(y, '~', x))
               fit <- 
                 glm(form, data=d, 
                     family = binomial(link = "logit"))
               odds <- 
                 exp(cbind(odds = coef(fit), confint(fit)))
               
               odds <- as.data.frame(odds)
               odds$var <- row.names(odds)
               
               if (is.factor(d[,x])){
                 counts <- d %>% 
                   group_by_(.dots=as.symbol(x)) %>%
                   summarise(n=n())
                 
                 odds$n <- counts$n
                 odds[1, 1:3] <- 1
                 odds[1, 'var'] <- paste0(x, fit$xlevels[[1]][1])
               }
               else{
                 odds <- odds[-1,]
                 odds$n <- NA
               }
               
               odds
             }) 
  colnames(o) <- make.names(colnames(o))
  o
}

## data ----

eqaAllMulti <- eqaAll %>%
  group_by(id) %>%
  mutate(rd1 = relDiff[1],
         rd2 = relDiff[2]
  ) %>%
  ungroup() %>% 
  dplyr::select(year, eqa, id, sharedDevice, device, eqaRound, pid, round, status,
                rd1, rd2) %>%
  unique()



prevMulti <- eqaAllMulti %>% 
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  transmute(eqa = eqa, status.prev = status, seq = seq+1, pid=pid) %>%
  distinct()  


eqasByYearMulti <- eqaAllMulti %>%
  dplyr::select(year, pid, eqa) %>%
  unique() %>%
  group_by(year, pid) %>%
  mutate(n = n()) %>%
  mutate(extraEqa = ifelse(n == 1, 'none', as.character(eqa))) %>%
  ungroup() %>%
  mutate(extraEqa = factor(extraEqa)) %>%
  mutate(eqa = NULL) %>%
  mutate(n = NULL) %>%
  commonOrder()


byMulti <- eqaAllMulti %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  mutate(seqYear = dense_rank(year), seqSimple = seq) %>% 
  mutate(seq = ifelse(year == 2012 & seq == 1 & round < 3, NA, seq)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  mutate(seq = ifelse(hasFullSeq | seq > 10, seq, NA)) %>%
  mutate(seqGrp = case_when(is.na(seq) ~ NA_character_,
                            seq == 1 ~ 'new', 
                            seq <= 10 ~ 'intermediate',
                            TRUE ~ 'experienced' )) %>%
  mutate(seqGrpSimple = case_when(!hasFullSeq ~ 'experienced',
                                  seq == 1 ~ 'new', 
                                  seq <= 10 ~ 'intermediate',
                                  TRUE ~ 'experienced' )) %>%
  mutate(seqGrp = factor(seqGrp)) %>%
  mutate(seq = ifelse(hasFullSeq, seq, NA)) %>%
  ungroup() %>%
  left_join(eqasByYearMulti, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(as.character(extraEqa) != as.character(eqa)) %>%
  left_join(prevMulti , by=c('eqa' = 'eqa', 'seq' = 'seq', 'pid'='pid')) %>%
  mutate(sharedDevice = as.character(sharedDevice)) %>%
  mutate(sharedDevice = ifelse(is.na(sharedDevice), 
                               "others", sharedDevice)) %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  mutate(status.prev = factor(status.prev, ordered= FALSE)) %>%
  mutate(extraEqaSingle = extraEqa) %>%
  mutate(extraEqa = fct_collapse(extraEqa, 
                                 'CL' =  c('Instand 100', 'RfB KS'),
                                 'POCT' = c('Instand 800', 'RfB GL'))) %>%
  dplyr::select(year, eqa, id, seq, seqGrp, seqYear, seqSimple, extraEqa, 
                status.prev, sharedDevice, device, notFailed, seqGrpSimple,
                good, eqaRound, pid, round, rd1, rd2, status, extraEqaSingle) %>%
  group_by(sharedDevice, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(sharedDevice = ifelse(n < 100 | nlabs < 10, 
                               "others", sharedDevice)) %>%
  group_by(device, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(device = as.character(device)) %>%
  mutate(device = if_else(n < 100, "others", device)) %>%
  mutate(device = if_else(nlabs < 10, "others", device)) %>%
  mutate(device = if_else(str_detect(device, 'Others --'), "others", device)) %>%
  mutate(device = if_else(str_detect(device, "Anderer Hersteller, other producer"),
                          "others", device)) %>%
  rowwise() %>%
  mutate(device = replaceCLNames(device), 
         sharedDevice = replaceCLNames(sharedDevice)) %>%
  ungroup() %>%
  mutate(sharedDevice = factor(sharedDevice), n = NULL) %>%
  mutate(sharedDevice = fct_relevel(sharedDevice, 'others')) %>%
  mutate(device = factor(device)) %>%
  mutate(device = fct_relevel(device, 'others')) %>%
  mutate(seqGrp = fct_relevel(seqGrp, 'new')) %>%
  mutate(status.prev = fct_relevel(status.prev, 'acceptable')) %>%
  mutate(extraEqa = fct_relevel(extraEqa, 'none'))

### combine devices ----

byMultiComplete <- byMulti %>%
  filter(!is.na(seqGrp)) %>%
  mutate_at(vars(device, sharedDevice), as.character) %>%
  group_by(device, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(device = ifelse(n < 50, "others", device)) %>%
  mutate(device = ifelse(nlabs < 10, "others", device)) %>%
  group_by(sharedDevice, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(sharedDevice = ifelse(n < 50 | nlabs < 10,
                               "others", sharedDevice)) %>%
  mutate_at(vars(device, sharedDevice), as.factor) %>%
  mutate(device = fct_relevel(device, 'others')) %>%
  mutate(sharedDevice = fct_relevel(sharedDevice, 'others'))

## percentage plots ----

percPlot <- function(var, xlabel){
  qvar <- quo(UQ(sym(var)))
  
  percData <- byMulti %>%
    filter(!is.na(UQ(qvar))) %>%
    group_by(UQ(qvar), eqa, status) %>%
    summarise(n=n()) %>%
    mutate(p=n/sum(n)) %>%
    ungroup() %>%
    commonOrder()
  
  countData <- byMulti %>%
    filter(!is.na(UQ(qvar))) %>%
    group_by(UQ(qvar), eqa) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    commonOrder()
  
  ggplot() +
    geom_col(data=percData, aes_(x=UQ(qvar), y=~p, fill=~status),
             position = position_stack(reverse = TRUE)) +
    geom_text(data=countData, aes_(x=UQ(qvar), y=1.1, label=~n), size=3) +
    facet_grid(.~eqa, scales = 'free_x') +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
    scale_fill_manual(values=colors.status) +
    xlab(xlabel) +
    ylab('percentage of individual EQA participations') +
    theme(legend.title = element_blank())
}


percPlot('seqGrp', 'number of previous EQAs')
ggpub('bySeqEQA', height= 150)

percPlot('extraEqaSingle', 'additional participation in other EQAs')
ggpub('byParticipate', height= 150)

percPlot('status.prev', 'previous result')
ggpub('byPrevEQA', height= 150)

### by device ----
byDeviceGraph <- byMulti %>%
  filter(eqa != 'Instand 100') %>%
  group_by(eqa, device, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()

byDeviceGraphN <- byMulti%>%
  filter(eqa != 'Instand 100') %>%
  group_by(eqa, device) %>%
  summarise(n=n()) %>%
  ungroup()

for(e in unique(byDeviceGraph$eqa)){
  grData <- byDeviceGraph %>% 
    filter(eqa == e) %>%
    group_by(device) %>%
    mutate(toOrder = p[status == "failed"]) %>%
    ungroup() %>%
    mutate(device = fct_reorder(device, toOrder))
  
  grN <- byDeviceGraphN %>% filter(eqa == e)
  
  devPlot <- ggplot() + 
    geom_col(data = grData, aes(x=device,  y=p, fill=status),
             position = position_stack(reverse = TRUE)) +
    geom_text(data = grN, aes(x=device, y=1.1, label=n),size=3)+ 
    scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
    scale_fill_manual(values=colors.status) +
    xlab(paste0('devices in ', e)) +
    ylab('percentage of individual EQA participations') +
    theme_pub() +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  ggpub(paste0('dev', e), height = 150)
}


## univariate odds ----

### good results ----
oddsSeqGrpGood <- calcOdds(byMulti, 'seqGrp', 'good')
oddsPrevEqaGood <- calcOdds(byMulti, 'status.prev', 'good')
oddsSeqGood <- calcOdds(byMulti, 'seq', 'good')
oddsParticipateGood <- calcOdds(byMulti, 'extraEqa', 'good')

oddsAllGood <- rbind(oddsPrevEqaGood, oddsSeqGood, 
                     oddsSeqGrpGood, oddsParticipateGood) %>%
  mutate(var = factor(var)) %>%
  commonOrder()

ggplot(oddsAllGood,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  geom_hline(yintercept = 1) +
  xlab('') +
  ylab('univariate odds ratios') +
  facet_grid(eqa~.) +
  scale_y_continuous(trans=log10_trans(), limits = c(.1,10)) +
  scale_x_discrete(labels=oddsLabels)+
  theme_pub()

ggpub('oddsAllGood', height= 180)

### not failed results ----
oddsSeqGrpNotFailed <- calcOdds(byMulti, 'seqGrp', 'notFailed')
oddsPrevEqaNotFailed <- calcOdds(byMulti, 'status.prev', 'notFailed')
oddsSeqNotFailed <- calcOdds(byMulti, 'seq', 'notFailed')
oddsParticipateNotFailed <- calcOdds(byMulti, 'extraEqa', 'notFailed')


oddsAllNotFailed <- rbind(oddsPrevEqaNotFailed, oddsSeqGrpNotFailed, 
                          oddsSeqNotFailed, oddsParticipateNotFailed) %>%
  mutate(var = factor(var)) %>%
  commonOrder()

ggplot(oddsAllNotFailed,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  geom_hline(yintercept = 1) +
  xlab('') +
  ylab('univariate odds ratios') +
  facet_grid(eqa~.) +
  scale_y_continuous(trans=log10_trans(), limits = c(.1,10)) +
  scale_x_discrete(label=oddsLabels) +
  theme_pub()

ggpub('oddsAllNotFailed', height = 180)

## multivariate odds ----

multivariatePlot <- function(res){
  oddsConf <- exp(cbind(odds = coef(res), confint(res)))
  
  oddsConf <- as.data.frame(oddsConf)
  
  oddsConf$var <- row.names(oddsConf)
  colnames(oddsConf) <- make.names(colnames(oddsConf))
  
  oddsConf <- oddsConf %>% 
    filter(var != '(Intercept)') %>%
    mutate(var = str_replace(var, 'sharedDevice', 'device')) %>%
    filter(!str_detect(var, 'eqaRound')) %>%
    bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                         var = c('extraEqanone', 'seqGrpnew', 'deviceothers'))) %>%
    mutate(orderForVar = ifelse(str_detect(var, 'device'), 100+odds, 0)) %>%
    mutate(var = str_replace(var, 'device', '')) %>%
    mutate(var = factor(var)) %>%
    mutate(var = fct_reorder(var, orderForVar)) %>%
    commonOrder()
  
  ggplot()+
    geom_point(data = oddsConf, aes(x=var, y=odds))+
    geom_errorbar(data = oddsConf, aes(x=var, ymin=X2.5.., ymax=X97.5..)) + 
    coord_flip() +
    xlab('') +
    ylab('multivariate odds ratios') +
    geom_hline(yintercept = 1) +
    scale_y_continuous(trans=log10_trans(), limits = c(.1, 10)) +
    scale_x_discrete(labels = oddsLabels) +
    theme_pub()
  
}

### POCT ----


resMultiGoodPOCT <- glm(good~seqGrp+extraEqa+sharedDevice+eqaRound, 
                        data = byMultiComplete %>% 
                          filter(eqa=='Instand 800' | eqa == 'RfB GL'), 
                        family = binomial(link = "logit"))

plotMultiGoodPOCT <- multivariatePlot(resMultiGoodPOCT)
ggpub('oddsMultiGoodPOCT', height= 80, plot = plotMultiGoodPOCT)

resMultiNotFailedPOCT <- glm(notFailed~seqGrp+extraEqa+sharedDevice+eqaRound, 
                        data = byMultiComplete %>% 
                          filter(eqa=='Instand 800' | eqa == 'RfB GL'), 
                        family = binomial(link = "logit"))

plotMultiNotFailedPOCT  <- multivariatePlot(resMultiNotFailedPOCT)
ggpub('oddsMultiNotFailedPOCT', height= 80, plot = plotMultiNotFailedPOCT)

### CL ----

resMultiGoodCL <- glm(good~ seqGrp+ extraEqa +  eqaRound + device, 
                      data = byMultiComplete %>% filter(eqa == 'RfB KS'), 
                      family = binomial(link = "logit"))

plotMultiGoodCL <- multivariatePlot(resMultiGoodCL)
ggpub('oddsMultiGoodCL', height = 220, plot = plotMultiGoodCL)

resMultiCLNotFailed <- glm(notFailed~ seqGrp+ extraEqa +  eqaRound + device, 
                      data = byMultiComplete %>% filter(eqa == 'RfB KS'), 
                      family = binomial(link = "logit"))

plotMultiNotFailedCL <- multivariatePlot(resMultiCLNotFailed)
ggpub('oddsMultiNotFailedCL', height = 220, plot = plotMultiNotFailedCL)

## imputation ----

oddsFromImpute <- function(fit, cc){
  pooled <- pool(fit)
  oddsData <- data.frame(odds = exp(pooled$qbar))
  oddsData$var <- row.names(oddsData)
  colnames(oddsData) <- make.names(colnames(oddsData))
  
  oddsData <- oddsData %>%
    filter(var != '(Intercept)') %>%
    filter(!str_detect(var, 'eqaRound')) %>%
    mutate(var = ifelse(str_detect(var, 'sharedDevice'),
                        paste0('sharedDevice', 
                               levels(cc$sharedDevice)[
                                 as.numeric(str_match(var, '\\d+')[,1])]),
                        var)) %>%
    mutate(var = ifelse(str_detect(var, 'device'),
                        paste0('device', 
                               levels(cc$device)[
                                 as.numeric(str_match(var, '\\d+')[,1])]),
                        var)) %>%
    mutate(var = ifelse(str_detect(var, 'seqGrp'),
                        paste0('seqGrp', 
                               levels(cc$seqGrp)[
                                 as.numeric(str_match(var, '\\d+')[,1])]),
                        var)) %>%
    mutate(var = str_replace(var, 'sharedDevice', 'device')) %>%
    bind_rows(data_frame(odds = 1, 
                         var = c('extraEqanone',
                                 'seqGrpnew',
                                 'deviceothers'))) %>%
    mutate(orderForVar = ifelse(str_detect(var, 'device'), 100+odds, 0)) %>%
    mutate(var = str_replace(var, 'device', '')) %>%
    mutate(var = factor(var))
}

#' @seealso https://stats.stackexchange.com/questions/78632/multiple-imputation-for-missing-values
mice.impute.impSeq <- function(y, ry, x, fullData, ...){
  errorEncountered <- TRUE
  n <- 0
  
  while(errorEncountered){
    tryCatch({
      errorEncountered <- FALSE
      fullData <- fullData %>% 
        group_by(eqa, pid) %>%
        mutate(rank = dense_rank(year*100+round)) %>%
        mutate(randomSel = ((1:n()) == base::sample(1:n(), 1))) %>%
        ungroup()
      
      missingFirst <- fullData$rank == 1 & !ry
      notMissingRandomlySelected <- fullData$randomSel & ry
      
      sel <- missingFirst | notMissingRandomlySelected
      
      imputeFirst <- mice.impute.pmm(y[sel], ry[sel], x[sel, ], ...)
    },
    error = function(x){
      print('repeat')
      errorEncountered <<- TRUE
      return(NA)
    },
    finally = {
      
    }
    )
  }
  
  
  fullData[missingFirst, 'seq'] <- imputeFirst
  
  fullData <- fullData %>% 
    group_by(eqa, pid) %>%
    mutate(seq = ifelse(is.na(seq), rank+min(seq, na.rm=TRUE)-1,
                        seq)) %>%
    ungroup()
  
  fullData$seq[!ry]  
  
}

seqGrpFromSeq <- function(seq){
  case_when(seq == 1 ~ 'new', 
            seq <= 10 ~ 'intermediate',
            TRUE ~ 'experienced' )
}

meths <- c('year' = '', 'eqa' = '', 'seq' = 'impSeq', 
           'seqGrp' = '~seqGrpFromSeq(seq)', 'extraEqa'= '',
           'status.prev' = '', 'sharedDevice' = '', 'device' = '',
           'notFailed' = '', 'good' = '', 'eqaRound' = '', 'round' = '',
           'rd1' = '', 'rd2' = '', 'status' = '', 'extraEqaSingle' = '') 

### POCT ----

multiMicePOCT <- mice(byMulti %>% 
                       filter(eqa=='Instand 800' | eqa == 'RfB GL') %>%
                       dplyr::select(-pid, -id,  -nlabs, -seqYear, -seqSimple, 
                                    -seqGrpSimple), 
                    method = meths,     m=5, 
                    fullData = byMulti %>% 
                      filter(eqa=='Instand 800' | eqa == 'RfB GL')
)

ccPOCT <- complete(multiMicePOCT, 1)

fitMultiGoodPOCT <- with(data=multiMicePOCT, 
                 exp=glm(good~seqGrp+extraEqa+sharedDevice+eqaRound,
                         family = binomial(link = "logit")))

oddsMultiGoodPOCTImp <- oddsFromImpute(fitMultiGoodPOCT, ccPOCT)

plotMultiGoodPOCT + 
  geom_point(data = oddsMultiGoodPOCTImp, 
             aes(x=var, y=odds), colour='red', shape=4)

ggpub('oddsMultiGoodImp', height= 220)

fitMultiNotFailedPOCT <- with(data=multiMicePOCT, 
                         exp=glm(notFailed~seqGrp+extraEqa+sharedDevice+eqaRound,
                                 family = binomial(link = "logit")))

oddsMultiNotFailedPOCTImp <- oddsFromImpute(fitMultiNotFailedPOCT, ccPOCT)

plotMultiNotFailedPOCT + 
  geom_point(data = oddsMultiNotFailedPOCTImp, 
             aes(x=var, y=odds), colour='red', shape=4)

ggpub('oddsMultiNotFailedImp', height= 220)

### CL ----

dataMiceCL <- byMulti %>% 
  filter(eqa == 'RfB KS') %>%
  mutate(device = as.character(device)) %>%
  mutate(device = if_else(!device %in% 
                            unique(byMultiComplete$device),
                          "others", device)) %>%
  mutate(device = factor(device)) %>%
  mutate(device = fct_relevel(device, 'others'))

multiMiceCL <- mice(dataMiceCL %>%
                      dplyr::select(-pid, -id,  -nlabs, -seqYear, -seqSimple, 
                                      -seqGrpSimple), 
                      method = meths,     m=5, 
                      fullData = dataMiceCL
)

ccCL <- complete(multiMiceCL, 1)

fitMultiGoodCL <- with(data=multiMiceCL, 
                         exp=glm(good~seqGrp+extraEqa+device+eqaRound,
                                 family = binomial(link = "logit")))

oddsMultiGoodCLImp <- oddsFromImpute(fitMultiGoodCL, ccCL)

plotMultiGoodCL + 
  geom_point(data = oddsMultiGoodCLImp, 
             aes(x=var, y=odds), colour='red', shape=4)

ggpub('oddsMultiCLGoodImp', height= 220)

fitMultiNotFailedCL <- with(data=multiMiceCL, 
                              exp=glm(notFailed~seqGrp+extraEqa+device+eqaRound,
                                      family = binomial(link = "logit")))

oddsMultiNotFailedCLImp <- oddsFromImpute(fitMultiNotFailedCL, ccCL)

plotMultiNotFailedCL + 
  geom_point(data = oddsMultiNotFailedCLImp, 
             aes(x=var, y=odds), colour='red', shape=4)

ggpub('oddsMultiCLNotFailedImp', height= 220)


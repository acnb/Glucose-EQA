## functions ----

library(mice)

strWrapper <- label_wrap_gen(20)


oddsLabels <- c('seq' = 'number of previous participations',
                
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
                'extraEqaCL' = 'additional CL EQA',
                
                'none' = 'no additional EQA',
                'POCT' = 'additional POCT EQA',
                'CL' = 'additional CL EQA',
                
                'new' = '"new" participant',
                'intermediate' = '"intermediate" experience',
                'experienced' = '"experienced" participant')

varLabeller <- as_labeller(c('extraEqa' = "add.\nEQA",
                             'seqGrp' = "experi-\nence",
                             'sharedDevice' = 'device'))



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
  data <- data[,c(x,y, 'eqa', 'type')] 
  data <- data[complete.cases(data),] %>% droplevels()
  o <- ddply(data,
             c('eqa', 'type'), function(d){
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
  o$xvar <- x
  o
}



multivariablePlot <- function(oddsConf, ylab = 'multivariable odds ratios',
                             extraFacetVars = NULL){
  maxLim <- ceiling(max(oddsConf$X97.5..)/10)*10
  
  range <- log(maxLim)-log(0.1)
  textPos <- exp((range*0.1)+log(0.1))
  
  facetForm <- 'xvar~.'
  
  if (!is.null(extraFacetVars)){
    facetForm <- paste(extraFacetVars, facetForm, collapse = '+', sep = '+')
  }
  
  ggplot()+
    geom_rect(data = oddsConf, aes(fill=type), xmin = -Inf, xmax = Inf,
              ymin = -Inf,ymax = Inf) +
    geom_point(data = oddsConf, aes(x=var, y=odds))+
    geom_errorbar(data = oddsConf, aes(x=var, ymin=X2.5.., ymax=X97.5..)) + 
    geom_text(data = oddsConf, aes(x=var, y= textPos, label=n), hjust=1) +
    coord_flip() +
    xlab('') +
    ylab(ylab) +
    geom_hline(yintercept = 1, linetype = 2) +
    facet_grid(as.formula(facetForm), scales ='free_y', 
               space='free_y', labeller = varLabeller) + 
    scale_y_continuous(trans=log10_trans(), limits = c(.1, maxLim),
                       breaks = c(.1, .5, 1, 5, 10, maxLim)) +
    scale_x_discrete(labels = oddsLabels) +
    scale_fill_manual(values= typeColors, guide = "none") +
    theme_pub() +
    theme(strip.text.y = element_text(size = 8))
}


## data ----

eqaAllMulti <- eqaAll %>%
  group_by(id) %>%
  mutate(rd1 = relDiff[1],
         rd2 = relDiff[2]
  ) %>%
  ungroup() %>% 
  dplyr::select(year, eqa, id, sharedDevice, device, eqaRound, pid, round, status,
                rd1, rd2, type) %>%
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
  mutate(seqForGrp = ifelse(year == 2012 & seq == 1 & round < 3, NA, seq)) %>%
  mutate(hasFullSeq = (1 %in% seqForGrp)) %>%
  mutate(seqForGrp = ifelse(hasFullSeq | seqForGrp > 10, seqForGrp, NA)) %>%
  mutate(seqGrp = case_when(is.na(seqForGrp) ~ NA_character_,
                            seq == 1 ~ 'new', 
                            seq <= 10 ~ 'intermediate',
                            TRUE ~ 'experienced' )) %>%
  mutate(seq = if_else(is.na(seqGrp), NA_integer_, seq)) %>%
  ungroup() %>%
  mutate(seqGrp = factor(seqGrp)) %>%
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
                                 'CL' =  c('CL-Instand', 'CL-RfB'),
                                 'POCT' = c('POCT-Instand', 'POCT-RfB'))) %>%
  dplyr::select(year, eqa, id, seq, seqGrp, extraEqa, 
                status.prev, sharedDevice, notFailed, 
                good, eqaRound, pid, round, rd1, rd2, status, 
                extraEqaSingle, type) %>%
  group_by(sharedDevice, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  group_by(sharedDevice) %>%
  mutate(minNLabs = min(nlabs), nSum = sum(n)) %>%
  ungroup() %>%
  mutate(sharedDevice = ifelse(nSum < 100 | minNLabs < 10, 
                               "others", sharedDevice)) %>%
  mutate(minNLabs = NULL, nSum = NULL) %>%
  mutate(sharedDevice = factor(sharedDevice), n = NULL) %>%
  mutate(sharedDevice = fct_relevel(sharedDevice, 'others')) %>%
  mutate(seqGrp = fct_relevel(seqGrp, 'new')) %>%
  mutate(status.prev = fct_relevel(status.prev, 'acceptable')) %>%
  mutate(extraEqa = fct_relevel(extraEqa, 'none'))

### combine devices ----

byMultiComplete <- byMulti %>%
  filter(!is.na(seqGrp)) %>%
  mutate(sharedDevice = as.character(sharedDevice)) %>%
  group_by(sharedDevice, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  group_by(sharedDevice) %>%
  mutate(minNLabs = min(nlabs), totNLabs = n_distinct(pid), nSum = sum(n)) %>%
  ungroup() %>%
  mutate(sharedDevice = ifelse(nSum < 50 | minNLabs < 5 | totNLabs < 10,
                               "others", sharedDevice)) %>%
  mutate(sharedDevice = as.factor(sharedDevice)) %>%
  mutate(sharedDevice = fct_relevel(sharedDevice, 'others'))

## percentage plots ----

percPlot <- function(var, xlabel){
  qvar <- quo(UQ(sym(var)))
  
  percData <- byMulti %>%
    filter(!is.na(UQ(qvar))) %>%
    group_by(UQ(qvar), eqa, status) %>%
    summarise(n=n(), type = type[1]) %>%
    mutate(p=n/sum(n)) %>%
    ungroup() %>%
    commonOrder()
  
  countData <- byMulti %>%
    filter(!is.na(UQ(qvar))) %>%
    group_by(UQ(qvar), eqa) %>%
    summarise(n=n(), type = type[1]) %>%
    group_by(eqa) %>%
    mutate(p = n/sum(n)) %>%
    ungroup() %>%
    mutate(label = paste0(n, "\n[", round(p,2)*100, "%]", sep="")) %>%
    commonOrder()
  
  ggplot() +
    geom_rect(data = percData, aes(fill = type),
              xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
    geom_col(data=percData, aes_(x=UQ(qvar), y=~p, fill=~status),
             position = position_stack(reverse = TRUE)) +
    geom_text(data=countData, aes_(x=UQ(qvar), y=1.13, label=~label), size=3) +
    facet_grid(.~eqa, scales = 'free_x') +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
  #  scale_fill_manual(values = typeColors, guide = "none") + 
    scale_fill_manual(breaks= names(colors.status), 
                      values=c(colors.status, typeColors)) +
    xlab(xlabel) +
    ylab('individual EQA participations') +
    theme(legend.title = element_blank())
}


percPlot('seqGrp', 'experience through previous EQAs')
ggpub('bySeqEQA', height= 150)

percPlot('extraEqaSingle', 'additional participation in other EQAs')
ggpub('byParticipate', height= 150)

percPlot('status.prev', 'previous result')
ggpub('byPrevEQA', height= 150)

### by device ----
byDeviceGraph <- byMulti %>%
  group_by(eqa, sharedDevice, status) %>%
  summarise(n=n(), type = type[1]) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()

byDeviceGraphN <- byMulti%>%
  group_by(eqa, sharedDevice) %>%
  summarise(n=n(), type = type[1]) %>%
  group_by(eqa) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  mutate(label = paste0(n, "\n[", round(p,2)*100, "%]", sep=""))

for(e in unique(byDeviceGraph$eqa)){
  grData <- byDeviceGraph %>% 
    filter(eqa == e) %>%
    group_by(sharedDevice) %>%
    mutate(toOrder = max(c(p[status == "failed"], 0))) %>%
    ungroup() %>%
    mutate(sharedDevice = unlist(strWrapper(as.character(sharedDevice)))) %>%
    mutate(sharedDevice = factor(sharedDevice)) %>%
    mutate(sharedDevice = fct_reorder(sharedDevice, toOrder))
  
  grN <- byDeviceGraphN %>% 
    filter(eqa == e) %>%
    mutate(sharedDevice = unlist(strWrapper(as.character(sharedDevice)))) %>%
    mutate(sharedDevice = factor(sharedDevice))
  
  devPlot <- ggplot() + 
    geom_rect(data = grData, aes(fill = type),
              xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
    geom_col(data = grData, aes(x=sharedDevice,  y=p, fill=status),
             position = position_stack(reverse = TRUE)) +
    geom_text(data = grN, aes(x=sharedDevice, y=1.13, label=label),size=3)+ 
    scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
    scale_fill_manual(breaks= names(colors.status),
                      values=c(colors.status, typeColors)) +
    xlab(paste0('devices in ', e)) +
    ylab('individual EQA participations') +
    theme_pub() +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  ggpub(paste0('dev', e), height = 150)
}


## univariate odds ----

### good results ----
oddsSeqGrpGood <- calcOdds(byMulti, 'seqGrp', 'good')
oddsPrevEqaGood <- calcOdds(byMulti, 'status.prev', 'good') %>%
  mutate(var = factor(var)) %>%
  commonOrder()
oddsSeqGood <- calcOdds(byMulti, 'seq', 'good')
oddsParticipateGood <- calcOdds(byMulti, 'extraEqa', 'good')

oddsDevGoodCL <- calcOdds(byMulti %>% filter(type == 'CL'), 'sharedDevice',
                          'good') %>%
  mutate(var = str_replace(var, 'sharedDevice', '')) %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder2(var,-as.numeric(eqa), -odds))

oddsAllGoodCL <-  rbind(oddsSeqGrpGood, oddsParticipateGood) %>%
  filter(type == 'CL' ) %>%
  mutate(var = factor(var)) %>%
  commonOrder() %>%
  mutate(var = fct_expand(var, levels(oddsDevGoodCL$var)))

oddsAllGoodCL <- rbind(oddsDevGoodCL, oddsAllGoodCL)


multivariablePlot(oddsAllGoodCL, extraFacetVars = 'eqa', 
                 ylab = 'univariate odds ratios')

ggpub('oddsAllGoodCL', height= 200, device = 'pdf')

oddsDevGoodPOCT <- calcOdds(byMulti %>% 
                              filter(type == 'POCT'),
                            'sharedDevice', 'good') %>%
  mutate(var = str_replace(var, 'sharedDevice', ''))%>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder2(var,-as.numeric(eqa), -odds))


oddsAllGoodPOCT <- rbind(oddsSeqGrpGood, oddsParticipateGood) %>%
  filter(type == 'POCT') %>%
  mutate(var = factor(var)) %>%
  commonOrder() %>%
  mutate(var = fct_expand(var, levels(oddsDevGoodPOCT$var)))

oddsAllGoodPOCT <- rbind(oddsDevGoodPOCT, oddsAllGoodPOCT)

multivariablePlot(oddsAllGoodPOCT, extraFacetVars = 'eqa', 
                 ylab = 'univariate odds ratios')

ggpub('oddsAllGoodPOCT', height= 180, device = 'pdf')


ggplot(oddsPrevEqaGood,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5.., fill=type))+
  geom_rect(xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  geom_hline(yintercept = 1, linetype = 2) +
  xlab('') +
  ylab('univariate odds ratios') +
  facet_grid(eqa~.) +
  scale_y_continuous(trans=log10_trans(), limits = c(.01,10),
                     breaks = c(.1, .5, 1, 5, 10)) +
  scale_x_discrete(labels=oddsLabels)+
  scale_fill_manual(values= typeColors, guide = "none") +
  theme_pub()

ggpub('oddsPrevEqaGood', height= 120)

### not failed results ----
oddsSeqGrpNotFailed <- calcOdds(byMulti, 'seqGrp', 'notFailed')
oddsPrevEqaNotFailed <- calcOdds(byMulti, 'status.prev', 'notFailed') %>%
  mutate(var = factor(var)) %>%
  commonOrder()
oddsSeqNotFailed <- calcOdds(byMulti, 'seq', 'notFailed')
oddsParticipateNotFailed <- calcOdds(byMulti, 'extraEqa', 'notFailed')


oddsDevNotFailedCL <- calcOdds(byMulti %>% filter(type=='CL'), 'sharedDevice',
                          'notFailed') %>%
  mutate(var = str_replace(var, 'sharedDevice', ''))%>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder2(var,-as.numeric(eqa), -odds)) 

oddsAllNotFailedCL <- rbind(oddsSeqGrpNotFailed, oddsParticipateNotFailed) %>%
  filter(type == 'CL') %>%
  mutate(var = factor(var)) %>%
  commonOrder() %>%
  mutate(var = fct_expand(var, levels(oddsDevNotFailedCL$var)))

oddsAllNotFailedCL <- rbind(oddsDevNotFailedCL, oddsAllNotFailedCL)

multivariablePlot(oddsAllNotFailedCL, extraFacetVars = 'eqa', 
                 ylab = 'univariate odds ratios')

ggpub('oddsAllNotFailedCL', height= 200, device = 'pdf')

oddsDevNotFailedPOCT <- calcOdds(byMulti %>% 
                              filter(type == 'POCT'),
                            'sharedDevice', 'notFailed') %>%
  mutate(var = str_replace(var, 'sharedDevice', ''))%>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder2(var,-as.numeric(eqa), -odds))


oddsAllNotFailedPOCT <- rbind(oddsSeqGrpNotFailed, oddsParticipateNotFailed) %>%
  filter(type == 'POCT') %>%
  mutate(var = factor(var)) %>%
  commonOrder() %>%
  mutate(var = fct_expand(var, levels(oddsDevNotFailedPOCT$var))) 

oddsAllNotFailedPOCT <- rbind(oddsDevNotFailedPOCT, oddsAllNotFailedPOCT)


multivariablePlot(oddsAllNotFailedPOCT, extraFacetVars = 'eqa', 
                 ylab = 'univariate odds ratios')

ggpub('oddsAllNotFailedPOCT', height= 180, device = 'pdf')


ggplot(oddsPrevEqaNotFailed,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5.., fill=type))+
  geom_rect(xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  geom_hline(yintercept = 1, linetype = 2) +
  xlab('') +
  ylab('univariate odds ratios') +
  facet_grid(eqa~.) +
  scale_y_continuous(trans=log10_trans(), limits = c(.01,10),
                     breaks = c(.1, .5,  1, 5, 10)) +
  scale_x_discrete(labels=oddsLabels)+
  scale_fill_manual(values= typeColors, guide = "none") +
  theme_pub()

ggpub('oddsPrevEqaNotFailed', height= 120)


## multivariable odds ----

multivariableConf <- function(res){
  oddsConf <- exp(cbind(odds = coef(res), confint(res)))
  
  oddsConf <- as.data.frame(oddsConf)
  
  oddsConf$var <- row.names(oddsConf)
  colnames(oddsConf) <- make.names(colnames(oddsConf))
  
  oddsConf <- oddsConf %>% 
    filter(var != '(Intercept)') %>%
    filter(!str_detect(var, 'eqaRound')) %>%
    bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                         var = c('extraEqanone', 'seqGrpnew', 'sharedDeviceothers'))) %>%
    mutate(xvar = case_when(str_detect(var, 'extraEqa') ~ 'extraEqa',
                            str_detect(var, 'seqGrp') ~ 'seqGrp',
                            str_detect(var, 'sharedDevice') ~ 'sharedDevice',
                            TRUE ~ NA_character_)) %>%
    mutate(var = str_replace(var, xvar, '')) %>%
    mutate(var = factor(var)) %>%
    mutate(var = fct_reorder(var, odds))
  
  oddsConf
}

multivariableCount <- function(data){
  counts <- data %>%
    group_by(seqGrp, extraEqa, sharedDevice) %>%
    summarise(n = n()) %>%
    gather(xvar, var, -n) %>%
    group_by(xvar, var) %>%
    summarise(n = sum(n))
}

multiOdds <- function(data, good = TRUE){
  if (good){
    res <- glm(good~seqGrp+extraEqa+sharedDevice+eqaRound, 
        data = data, family = binomial(link = "logit"))
  }
  else{
    res <- glm(notFailed~seqGrp+extraEqa+sharedDevice+eqaRound, 
               data = data, family = binomial(link = "logit"))
  }
  
  odds <- multivariableConf(res)
  counts <- multivariableCount(data)
  
  result <- odds %>%
    left_join(counts, by=c('var' = 'var', 'xvar' = 'xvar')) %>%
    mutate(type = if_else('POCT' %in% data$type, 'POCT', 'CL')) %>%
    mutate(var = factor(var)) %>%
    mutate(var = fct_reorder(var, odds)) %>%
    mutate(xvar = factor(xvar, levels = c('sharedDevice', 'seqGrp', "extraEqa")))
  
  result
}


### POCT ----


oddsMultiGoodPOCT <- multiOdds(byMultiComplete %>% filter(type == 'POCT'),
                               good = TRUE)

plotMultiGoodPOCT <- multivariablePlot(oddsMultiGoodPOCT)
ggpub('oddsMultiGoodPOCT', height= 100, plot = plotMultiGoodPOCT)

oddsMultiNotFailedPOCT <- multiOdds(byMultiComplete %>% filter(type == 'POCT'),
                                    good = FALSE)

plotMultiNotFailedPOCT  <- multivariablePlot(oddsMultiNotFailedPOCT)
ggpub('oddsMultiNotFailedPOCT', height= 100, plot = plotMultiNotFailedPOCT)

### CL ----

oddsMultiGoodCL <- multiOdds(byMultiComplete %>% filter(type == 'CL'),
                               good = TRUE)

plotMultiGoodCL <- multivariablePlot(oddsMultiGoodCL)
ggpub('oddsMultiGoodCL', height= 100, plot = plotMultiGoodCL)

oddsMultiNotFailedCL <- multiOdds(byMultiComplete %>% filter(type == 'CL'),
                                    good = FALSE)

plotMultiNotFailedCL  <- multivariablePlot(oddsMultiNotFailedCL)
ggpub('oddsMultiNotFailedCL', height= 100, plot = plotMultiNotFailedCL)

## imputation ----
set.seed(1)

formatOddsFromImpute <- function(fit, cc){
  pooled <- pool(fit)
  oddsData <- data.frame(odds = exp(pooled$qbar))
  oddsData$var <- row.names(oddsData)
  colnames(oddsData) <- make.names(colnames(oddsData))
  
  oddsData <- oddsData %>%
    filter(var != '(Intercept)') %>%
    filter(!str_detect(var, 'eqaRound')) %>%
    mutate(var = ifelse(str_detect(var, 'seqGrp'),
                        paste0('seqGrp', 
                               levels(cc$seqGrp)[
                                 as.numeric(str_match(var, '\\d+')[,1])]),
                        var)) %>%
    bind_rows(data_frame(odds = 1, 
                         var = c('extraEqanone',
                                 'seqGrpnew',
                                 'sharedDeviceothers'))) %>%
    mutate(xvar = case_when(str_detect(var, 'extraEqa') ~ 'extraEqa',
                            str_detect(var, 'seqGrp') ~ 'seqGrp',
                            str_detect(var, 'sharedDevice') ~ 'sharedDevice',
                            TRUE ~ NA_character_)) %>%
    mutate(var = str_replace(var, xvar, '')) %>%
    mutate(var = factor(var)) %>%
    mutate(var = fct_reorder(var, odds)) %>%
    mutate(xvar = factor(xvar, levels = c('sharedDevice', 'seqGrp', "extraEqa")))
  
  oddsData
}

countFromImpute <- function(data){
  counts <- data %>%
    filter(is.na(seq)) %>%
    group_by(extraEqa, sharedDevice) %>%
    summarise(n = n()) %>%
    gather(xvar, var, -n) %>%
    group_by(xvar, var) %>%
    summarise(n = sum(n))
}

oddsFromImpute <- function(miceData, good = TRUE){
  cc <- mice::complete(miceData, 1)
  
  if(good){
    fit <- with(data=miceData, 
                exp=glm(good~seqGrp+extraEqa+sharedDevice+eqaRound,
                        family = binomial(link = "logit")))
  } 
  else{
    fit <- with(data=miceData, 
                exp=glm(notFailed~seqGrp+extraEqa+sharedDevice+eqaRound,
                        family = binomial(link = "logit")))
  }
  
  
  
  odds <- formatOddsFromImpute(fit, cc)
  odds
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
        ungroup()
      
      missingFirst <- fullData$rank == 1 & !ry
      notMissing <-  ry
      
      sel <- missingFirst | notMissing
      
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

exclDevs <- levels(byMulti$sharedDevice)[!levels(byMulti$sharedDevice) %in% 
                                           levels(byMultiComplete$sharedDevice)]

### POCT ----

dataPOCT <- byMulti %>%
  mutate(sharedDevice = fct_collapse(sharedDevice, 'others' = exclDevs)) %>%
  filter(type == 'POCT')

multiMicePOCT <- mice(dataPOCT %>%
                       dplyr::select(-pid, -id,  -nlabs), 
                    method = meths,     m=5, 
                    fullData = dataPOCT)

oddsMultiGoodPOCTImp <- oddsFromImpute(multiMicePOCT, TRUE)
missingCountsPOCT <- countFromImpute(dataPOCT)

plotMultiGoodPOCT + 
  geom_point(data = oddsMultiGoodPOCTImp, 
             aes(x=var, y=odds), colour='red', shape=4) +
  geom_text(data = missingCountsPOCT,
            aes(x=var, y= 0.35, label=paste0('(+',n,')')), 
            colour = 'red', hjust=1)
 
ggpub('oddsMultiGoodPOCTImp', height= 120)

oddsMultiNotFailedPOCTImp <- oddsFromImpute(multiMicePOCT, FALSE)

plotMultiNotFailedPOCT + 
  geom_point(data = oddsMultiNotFailedPOCTImp, 
             aes(x=var, y=odds), colour='red', shape=4) +
  geom_text(data = missingCountsPOCT,
            aes(x=var, y= 0.4, label=paste0('(+',n,')')), 
            colour = 'red', hjust=1)

ggpub('oddsMultiNotFailedImp', height= 120)

### CL ----

dataMiceCL <- byMulti %>% 
  mutate(sharedDevice = 
           fct_collapse(sharedDevice, 
                        'others' = exclDevs)) %>%
  filter(type == 'CL')

multiMiceCL <- mice(dataMiceCL %>%
                      dplyr::select(-pid, -id,  -nlabs), 
                      method = meths,     m=5, 
                      fullData = dataMiceCL
)


oddsMultiGoodCLImp <- oddsFromImpute(multiMiceCL, TRUE)
missingCountsCL <- countFromImpute(dataMiceCL)

plotMultiGoodCL + 
  geom_point(data = oddsMultiGoodCLImp, 
             aes(x=var, y=odds), colour='red', shape=4) +
  geom_text(data = missingCountsCL,
            aes(x=var, y= 0.4, label=paste0('(+',n,')')), 
            colour = 'red', hjust=1)

ggpub('oddsMultiCLGoodImp', height= 120)

oddsMultiNotFailedCLImp <- oddsFromImpute(multiMiceCL, FALSE)

plotMultiNotFailedCL + 
  geom_point(data = oddsMultiNotFailedCLImp, 
             aes(x=var, y=odds), colour='red', shape=4) +
  geom_text(data = missingCountsCL,
            aes(x=var, y= 0.65, label=paste0('(+',n,')')), 
            colour = 'red', hjust=1)


ggpub('oddsMultiCLNotFailedImp', height= 120)


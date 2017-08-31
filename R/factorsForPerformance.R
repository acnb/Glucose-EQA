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
  
  str_replace_all(str, c('Hexokinase -- ' = "", 
                         'Glucose oxidase/PAP -- ' ="",
                         'Glucose oxidase/H2O2-electrode -- ' =""))
}


calcOdds <- function(data, x, y){
  o <- ddply(data %>% as.data.frame(),
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

replaceCLNames <- function(str){
  str <- case_when(str_detect(str, 'Hexokinase --') ~ paste(str, '[HK]'),
                   str_detect(str, 'Glucose oxidase/PAP --') ~ 
                     paste(str, '[GO/PAP]'),
                   str_detect(str, 'Glucose oxidase/H2O2-electrode -- ') ~ 
                     paste(str, '[GO/H2O2]'), 
                   TRUE ~ str)
  
  str_replace_all(str, c('Hexokinase -- ' = "", 
                         'Glucose oxidase/PAP -- ' ="",
                         'Glucose oxidase/H2O2-electrode -- ' =""))
}


## by number of participations ----

bySeqEQAAll <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  filter(!(year == 2012 & seq == 1 & round < 3)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  filter(hasFullSeq | seq > 8) %>%
  filter(abs(relDiff) < .5) %>%
  mutate(seqGrp = case_when(seq == 1 ~  'new', 
                         seq <= 10 ~ 'intermediate',
                         TRUE ~ 'experienced' )) %>%
  mutate(seqGrp = factor(seqGrp, levels=c('new', 'intermediate', 'experienced', 
                                          ordered = TRUE))) %>%
  ungroup() %>%
  select(eqa, id, seq, seqGrp, status) %>%
  mutate(seqGrp = fct_relevel(seqGrp, 'intermediate')) %>%
  unique()


bySeqEQALog <- bySeqEQAAll %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  select(eqa, id, seq, seqGrp, good, notFailed) %>%
  unique()


oddsSeqGrpEQAGood <- calcOdds(bySeqEQALog, 'seqGrp', 'good')
oddsSeqGrpEQNotFailed <- calcOdds(bySeqEQALog, 'seqGrp', 'notFailed')
oddsSeqEQAGood <- calcOdds(bySeqEQALog, 'seq', 'good')
oddsSeqEQANotFailed <- calcOdds(bySeqEQALog, 'seq', 'notFailed')               

bySeqEQAGraph <- bySeqEQAAll %>%
  group_by(eqa, seqGrp, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup() %>%
  commonOrder()

bySeqEQAGraphN <- bySeqEQAAll %>%
  group_by(eqa, seqGrp) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  commonOrder()

pBySeqEQA <-  ggplot() +
  geom_col(data=bySeqEQAGraph, aes(x=seqGrp, y=p, fill=status),
           position = position_stack(reverse = TRUE)) +
  geom_text(data=bySeqEQAGraphN, aes(x=seqGrp, y=1.1, label=n), size=3) +
  facet_grid(.~eqa) +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
  scale_fill_manual(values=colors.status) +
  xlab('number of previous EQAs') +
  ylab('percentage of individual EQA participations') +
  theme(legend.title = element_blank())

ggpub('bySeqEQA', height= 150)

## by participation in other EQAs ----

eqasByYear <- eqaAll %>%
  dplyr::select(year, pid, eqa) %>%
  unique() %>%
  group_by(year, pid) %>%
  mutate(n = n()) %>%
  mutate(extraEqa = ifelse(n == 1, 'none', as.character(eqa))) %>%
  ungroup() %>%
  mutate(eqa = NULL) %>%
  mutate(n = NULL) %>%
  commonOrder()

byParticipateAll <- eqaAll %>%
  left_join(eqasByYear, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(as.character(extraEqa) != as.character(eqa)) %>%
  select(eqa, id, extraEqa, status) %>%
  commonOrder() %>%
  unique()

byParticipateGraph <- byParticipateAll %>% 
  group_by(eqa, extraEqa, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n))

byParticipateGraphN <- byParticipateAll %>% 
  group_by(eqa, extraEqa) %>%
  summarise(n=n()) 

byParticipateLog <- byParticipateAll %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  dplyr::select(eqa, id, extraEqa, good, notFailed)

oddsParticipateGood <- calcOdds(byParticipateLog, 'extraEqa', 'good')
oddsParticipateNotFailed <- calcOdds(byParticipateLog, 'extraEqa', 'notFailed')


pByParticipate <- ggplot() + 
  geom_col(data = byParticipateGraph, aes(x=extraEqa,  y=p, fill=status),
           position = position_stack(reverse = TRUE)) +
  geom_text(data = byParticipateGraphN, aes(x=extraEqa, y=1.1, label=n),
            size=3)+ 
  facet_grid(.~eqa, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
  scale_fill_manual(values=colors.status) +
  xlab('additional participation in other EQAs') +
  ylab('percentage of individual EQA participations') +
  theme_pub(base_size = 10) +
  theme(legend.title = element_blank())


ggpub('byParticipate', height= 150)

## by result of previous participation ----

prev <- eqaAll %>% 
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  filter(seq > 1) %>%
  transmute(eqa = eqa, status.prev = status, seq = seq-1, pid=pid) %>%
  distinct()

byPrevEQAAll <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  left_join(prev, by=c('eqa' = 'eqa', 'seq' = 'seq', 'pid'='pid')) %>%
  filter(!is.na(status.prev)) %>%
  select(eqa, id, status.prev, status) %>%
  unique() %>%
  commonOrder()

byPrevEQAGraph <- byPrevEQAAll%>%
  group_by(eqa, status.prev, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()

byPrevEQAGraphN <- byPrevEQAAll%>%
  group_by(eqa, status.prev) %>%
  summarise(n=n()) %>%
  ungroup()

byPrevEQALog <- byPrevEQAAll%>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status == 'failed', 0, 1)) %>%
  select(eqa, id, status.prev, good, notFailed) %>%
  unique() %>%
  mutate(status.prev = factor(status.prev, ordered=FALSE)) %>%
  mutate(status.prev = fct_relevel(status.prev, 'acceptable'))

oddsPrevEQAGood <- calcOdds(byPrevEQALog, 'status.prev', 'good')
oddsPrevEQANotFailed <- calcOdds(byPrevEQALog, 'status.prev', 'notFailed')


pByPrevEQA <- ggplot() +
  geom_col(data = byPrevEQAGraph, aes(x=status.prev, y=p, fill=status),
           position = position_stack(reverse = TRUE)) +
  geom_text(data = byPrevEQAGraphN, aes(x=status.prev, y=1.1, label=n),
            size= 3) +
  facet_grid(~eqa) +
  scale_fill_manual(values=colors.status) +
  scale_y_continuous(labels=percent, breaks = c(0,.25, .5, .75, 1)) +
  ylab('percentage of individual EQA participations') +
  xlab('previous result')+
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

ggpub('byPrevEQA', height= 150)

## by device ----

byDevice <- eqaAll %>% 
  filter(eqa != 'Instand 100') %>%
  mutate(device = as.character(device)) %>%
  mutate(device = ifelse(!is.na(sharedDevice), 
                         as.character(sharedDevice), 
                         device)) %>%
  group_by(device, eqa) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  mutate(device = if_else(n < 100, "others", device)) %>%
  mutate(device = if_else(nlabs < 10, "others", device)) %>%
  mutate(device = if_else(str_detect(device, 'Others --'), "others", device)) %>%
  mutate(device = if_else(str_detect(device, "Anderer Hersteller, other producer"),
                          "others", device)) %>%
  select(eqa, device, id, status) %>%
  unique() %>%
  mutate(device = replaceCLNames(device)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(device = ifelse(str_length(device) > 30, 
                         paste(strwrap(device, 30), collapse ="\n"),
                         device)) %>%  
  ungroup() %>%
  mutate(device = as.factor(device))


byDeviceGraph <- byDevice %>%
  group_by(eqa, device, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()

byDeviceGraphN <- byDevice%>%
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

byDeviceLog <- byDevice%>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status == 'failed', 0, 1)) %>%
  select(eqa, id, device, good, notFailed) %>%
  unique() %>%
  mutate(device = fct_relevel(device, 'others'))

oddsDeviceGood <- calcOdds(byDeviceLog, 'device', 'good') %>%
  mutate(var = str_replace(as.character(var), 'device', '')) %>%
  mutate(var = factor(var))
  
oddsDeviceNotFailed <- calcOdds(byDeviceLog, 'device', 'notFailed') %>%
  mutate(var = str_replace(as.character(var), 'device', '')) %>%
  mutate(var = factor(var))

for(e in unique(oddsDeviceGood$eqa)){
  ggplot(oddsDeviceGood %>% 
           filter(eqa == e) %>%
           mutate(var = fct_reorder(var, odds)),
         aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
    geom_point()+
    geom_errorbar() + 
    coord_flip() +
    geom_hline(yintercept = 1) +
    xlab('') +
    ylab('univariate odds ratios') +
    scale_y_continuous(trans=log10_trans(), limits = c(.1,25)) +
    scale_x_discrete(labels=oddsLabels)+
    theme_pub()
  
  ggpub(paste0('oddsDev', e), height = 150)
  
}

## multi odds ---------------


eqaAllMulti <- eqaAll %>%
  filter(abs(relDiff) < .45) %>%
  ungroup()

prevMulti <- eqaAllMulti %>% 
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  filter(seq > 1) %>%
  transmute(eqa = eqa, status.prev = status, seq = seq-1, pid=pid) %>%
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
  mutate(seq = ifelse(year == 2012 & seq == 1 & round < 3, NA, seq)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  mutate(seq = ifelse(hasFullSeq | seq > 8, seq, NA)) %>%
  filter(abs(relDiff) < .45) %>%
  mutate(seqGrp = case_when(seq == 1 ~ 'new', 
                         seq <= 10 ~ 'intermediate',
                         TRUE ~ 'experienced' )) %>%
  mutate(seqGrp = factor(seqGrp)) %>%
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
  mutate(extraEqa = fct_collapse(extraEqa, 
                                 'CL' =  c('Instand 100', 'RfB KS'),
                                 'POCT' = c('Instand 800', 'RfB GL'))) %>%
  dplyr::select(year, eqa, id, seq, seqGrp, extraEqa, 
                status.prev, sharedDevice, device, notFailed, 
                good, eqaRound, pid, round) %>%
  group_by(sharedDevice, eqa) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(sharedDevice = ifelse(n < 100, 
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
  mutate(sharedDevice = factor(sharedDevice), n = NULL) %>%
  mutate(sharedDevice = fct_relevel(sharedDevice, 'others')) %>%
  mutate(device = as.factor(device)) %>%
  mutate(device = fct_relevel(device, 'others')) %>%
  mutate(seqGrp = fct_relevel(seqGrp, 'intermediate')) %>%
  mutate(status.prev = fct_relevel(status.prev, 'acceptable')) %>%
  mutate(extraEqa = fct_relevel(extraEqa, 'none'))

resMultiGoodPOCT <- glm(good~seqGrp+extraEqa+status.prev+sharedDevice+eqaRound, 
                        data = byMulti %>% 
                          filter(eqa=='Instand 800' | eqa == 'RfB GL'), 
                        family = binomial(link = "logit"))

oddsMultiGoodPOCT <- 
  exp(cbind(odds = coef(resMultiGoodPOCT), confint(resMultiGoodPOCT)))

oddsMultiGoodPOCT <- as.data.frame(oddsMultiGoodPOCT)

oddsMultiGoodPOCT$var <- row.names(oddsMultiGoodPOCT)
colnames(oddsMultiGoodPOCT) <- make.names(colnames(oddsMultiGoodPOCT))

oddsMultiGoodPOCT <- oddsMultiGoodPOCT %>% 
  filter(!is.na(X2.5..)) %>% 
  filter(!is.na(X97.5..)) %>%
  filter(var != '(Intercept)') %>%
  filter(!str_detect(var, 'eqaRound')) %>%
  bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                       var = c('extraEqanone',
                               'status.prevacceptable',
                               'seqGrpintermediate',
                               'sharedDeviceothers'))) %>%
  mutate(orderForVar = ifelse(str_detect(var, 'sharedDevice'), 100+odds, 0)) %>%
  mutate(var = str_replace(var, 'sharedDevice', '')) %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder(var, orderForVar)) %>%
  commonOrder()
  
  

ggplot(oddsMultiGoodPOCT,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  xlab('') +
  ylab('multivariate odds ratios') +
  geom_hline(yintercept = 1) +
  scale_y_continuous(trans=log10_trans(), limits = c(.1, 10)) +
  scale_x_discrete(labels = oddsLabels) +
  theme_pub()

ggpub('oddsMultiGoodPOCT', height= 80)


#### multi odds CL ----
resMultiGoodCL <- glm(good~seqGrp+extraEqa+status.prev+eqaRound+device, 
                        data = byMulti %>% 
                          filter(eqa == 'RfB KS'), 
                        family = binomial(link = "logit"))

oddsMultiGoodCL <- 
  exp(cbind(odds = coef(resMultiGoodCL), confint(resMultiGoodCL)))

oddsMultiGoodCL <- as.data.frame(oddsMultiGoodCL)

oddsMultiGoodCL$var <- row.names(oddsMultiGoodCL)
colnames(oddsMultiGoodCL) <- make.names(colnames(oddsMultiGoodCL))

oddsMultiGoodCLX <- oddsMultiGoodCL %>% 
  filter(!is.na(X2.5..)) %>% 
  filter(!is.na(X97.5..)) %>%
  filter(var != '(Intercept)') %>%
  filter(!str_detect(var, 'eqaRound')) %>%
  bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                       var = c('extraEqanone',
                               'status.prevacceptable',
                               'seqGrpintermediate',
                               'deviceothers'))) %>%
  mutate(orderForVar =  ifelse(str_detect(var, 'device'), 100+odds, 0)) %>%
  mutate(var = str_replace_all(var, 'device', "")) %>%
  mutate(var = replaceCLNames(var)) %>%
  rowwise() %>%
  mutate(var = ifelse(str_length(var) > 40, 
                      paste(strwrap(var, 40), collapse ="\n"),
                      var)) %>%  
  ungroup() %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder(var, orderForVar)) %>%
  commonOrder()


ggplot(oddsMultiGoodCLX,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  xlab('') +
  ylab('multivariate odds ratios') +
  geom_hline(yintercept = 1) +
  scale_y_continuous(trans=log10_trans(), limits = c(.1, 10)) +
  scale_x_discrete(labels = oddsLabels) +
  theme_pub() +
  theme(axis.text.x = element_text(size=6))

ggpub('oddsMultiGoodCL', height = 220)

### Imputation ----

#' @seealso https://stats.stackexchange.com/questions/78632/multiple-imputation-for-missing-values
mice.impute.seq <- function(y, ry, x, fullData, ...){
  fullData <- fullData %>% 
    group_by(eqa, pid) %>%
    mutate(rank = dense_rank(year*100+round)) %>%
    ungroup()
  
  first <- fullData$rank == 1
  
  sel <- ry | first
  
  imputeFirst <- mice.impute.pmm(y[sel], ry[sel], x[sel, ], ...)
  
  fullData[!ry & first, 'seq'] <- imputeFirst
  
  fullData <- fullData %>% 
    group_by(eqa, pid) %>%
    mutate(seq = ifelse(is.na(seq), rank+min(seq, na.rm=TRUE)-1,
                        seq)) %>%
    ungroup()
  
  fullData$seq[!ry]  
  
}

meths <- c('eqa' = '', 'seq' = 'seq', 'extraEqa'= '', 'status.prev' = 'polyreg',
           'sharedDevice' = '', 'notFailed' = '', 'good' = '', 'eqaRound' = '')

byMulitMice <- mice(byMulti %>% 
                      filter(eqa=='Instand 800' | eqa == 'RfB GL') %>%
                      select(-year, -pid, -round, -id, -seqGrp,
                             -nlabs, -device), 
                 method = meths,
                 m=5, 
                 fullData = byMulti %>% 
                   filter(eqa=='Instand 800' | eqa == 'RfB GL'))

fitMulti <- with(data=byMulitMice, 
                 exp=glm(good~seq+extraEqa+status.prev+sharedDevice+eqaRound,
                         family = binomial(link = "logit")))

pooledFitMulti <- pool(fitMulti)
oddsMultiGoodImp <- data.frame(odds = exp(pooledFitMulti$qbar))
oddsMultiGoodImp$var <- row.names(oddsMultiGoodImp)
colnames(oddsMultiGoodImp) <- make.names(colnames(oddsMultiGoodImp))
#
filter(!str_detect(var, 'eqaRound')) %>%
  bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                       type = c('extraEqa', 
                                'previous status', 
                                'Experience', 
                                'device'),
                       var = c('extraEqanone',
                               'status.prevacceptable',
                               'seqGrpintermediate',
                               'deviceothers'))) %>%
  mutate(orderForVar =  ifelse(str_detect(var, 'device'), 100+odds, 0)) %>%
  mutate(var = str_replace_all(var, 'device', "")) %>%
  mutate(var = replaceCLNames(var)) %>%
  rowwise() %>%
  mutate(var = ifelse(str_length(var) > 40, 
                      paste(strwrap(var, 40), collapse ="\n"),
                      var)) %>%  
  ungroup() %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder(var, orderForVar)) %>%
  commonOrder()


oddsMultiGoodImpX <- oddsMultiGoodImp %>%
  filter(var != '(Intercept)') %>%
  filter(!str_detect(var, 'eqaRound')) %>%
  mutate(var = ifelse(str_detect(var, 'status.prev'),
                                 paste0('status.prev', 
                                        levels(byMulti$status.prev)[
                                          as.numeric(str_match(var, '\\d+')[,1])]),
                                 var)) %>%
  mutate(var = ifelse(str_detect(var, 'sharedDevice'),
                       paste0('device', 
                      levels(byMulti$sharedDevice)[
                               as.numeric(str_match(var, '\\d+')[,1])]),
                      var)) %>%
  bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                       var = c('extraEqanone',
                               'status.prevacceptable',
                               'seqGrpintermediate',
                               'deviceothers'))) %>%
  mutate(orderForVar =  ifelse(str_detect(var, 'device'), 100+odds, 0)) %>%
  mutate(var = str_replace_all(var, 'device', "")) %>%
  mutate(var = replaceCLNames(var)) %>%
  rowwise() %>%
  mutate(var = ifelse(str_length(var) > 40, 
                      paste(strwrap(var, 40), collapse ="\n"),
                      var)) %>%  
  ungroup() %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_reorder(var, orderForVar)) %>%
  commonOrder()
  
  
  mutate(type = ifelse(str_detect(var, 'sharedDevice'), 'device', NA)) %>%
  mutate(type = ifelse(str_detect(var, 'status.prev'), 'previous status', type)) %>%
  mutate(type = ifelse(str_detect(var, 'extraEqa'), 'additional EQA', type)) %>%
  mutate(type = ifelse(str_detect(var, 'seq'), 'Number of previous EQA', type)) %>%
  mutate(type = ifelse(str_detect(var, 'centralLab'), 'Central Lab', type)) %>%
  mutate(var = str_replace(var, 'sharedDevice', '')) %>%
  mutate(var = str_replace(var, 'status.prev2', 'status.prevfailed')) %>%
  mutate(var = str_replace(var, 'status.prev3', 'status.prevgood')) %>%
  mutate(var = str_replace(var, 'status.prev4', 'status.prevpoor')) %>%
  mutate(var = str_replace(var, 'centralLab2', 'centralLabcentralLab')) %>%
  bind_rows(data_frame(odds = 1,  
                       type = c('Central Lab', 
                                'previous status', 
                                'device'),
                       var = c('has no central lab',
                               'status.prevacceptable',
                               'others'))) %>%
  mutate(var = factor(var)) %>%
  mutate(orderForVar = ifelse(type== 'device', odds+ 10^8,
                              ifelse(type== 'previous status', odds+ 10^6,
                                     ifelse(type== 'additional EQA', odds+ 10^4,
                                            ifelse(type== 'Number of previous EQA', odds+ 10^4, 2 ))))) %>%
  filter(!is.na(type)) %>%
  mutate(var = fct_reorder(var, orderForVar))

ggplot()+
  geom_point(data = oddsMultiGoodPOCT, aes(x=var, y=odds))+
  geom_errorbar(data = oddsMultiGoodPOCT, aes(x=var, ymin=X2.5.., ymax=X97.5..)) + 
  geom_point(data = oddsMultiGoodImp, aes(x=var, y=odds), colour='red', shape=4)+
  coord_flip() +
  xlab('') +
  ylab('multivariate odds ratios') + 
  geom_hline(yintercept = 1) +
  scale_y_continuous(trans=log10_trans(), limits = c(.1, 10)) +
  scale_x_discrete(labels = oddsLabels) +
  theme_pub()

ggpub('oddsMultiGoodImp', height= 80)

## multi notFailed ---------------

resMultiNotFailed <- glm(notFailed~seq+centralLab+status.prev+sharedDevice+eqaRound, 
                    data = byMulti %>% 
                      filter(eqa=='Instand 800' | eqa == 'RfB GL'), 
                    family = binomial(link = "logit"))

oddsMultiNotFailed <- 
  exp(cbind(odds = coef(resMultiNotFailed), confint(resMultiNotFailed)))

oddsMultiNotFailed <- as.data.frame(oddsMultiNotFailed)

oddsMultiNotFailed$var <- row.names(oddsMultiNotFailed)
colnames(oddsMultiNotFailed) <- make.names(colnames(oddsMultiNotFailed))

oddsMultiNotFailed <- oddsMultiNotFailed %>% 
  filter(!is.na(X2.5..)) %>% 
  filter(!is.na(X97.5..)) %>%
  filter(var != '(Intercept)') %>%
  mutate(type = ifelse(str_detect(var, 'sharedDevice'), 'device', NA)) %>%
  mutate(type = ifelse(str_detect(var, 'status.prev'), 'previous status', type)) %>%
  mutate(type = ifelse(str_detect(var, 'extraEqa'), 'additional EQA', type)) %>%
  mutate(type = ifelse(str_detect(var, 'seq'), 'Number of previous EQA', type)) %>%
  mutate(type = ifelse(str_detect(var, 'centralLab'), 'Central Lab', type)) %>%
  mutate(var = str_replace(var, 'sharedDevice', '')) %>%
  bind_rows(data_frame(odds = 1, X2.5.. = 1, X97.5.. = 1, 
                       type = c('Central Lab', 
                                'previous status', 
                                'device'),
                       var = c('has no central lab',
                               'status.prevacceptable',
                               'others'))) %>%
  mutate(var = factor(var)) %>%
  mutate(orderForVar = ifelse(type== 'device', odds+ 10^8,
                              ifelse(type== 'previous status', odds+ 10^6,
                                     ifelse(type== 'additional EQA', odds+ 10^4,
                                            ifelse(type== 'Number of previous EQA', odds+ 10^4, 2 ))))) %>%
  filter(!is.na(type)) %>%
  mutate(var = fct_reorder(var, orderForVar))

ggplot(oddsMultiNotFailed,
       aes(x=var, y=odds, ymin=X2.5.., ymax=X97.5..))+
  geom_point()+
  geom_errorbar() + 
  coord_flip() +
  geom_hline(yintercept = 1) +
  xlab('') +
  scale_y_continuous(trans=log10_trans(), limits = c(.1, 50)) +
  scale_x_discrete(labels = oddsLabels) +
  theme_pub()


ggpub('oddsMultiNotFailed', height= 80)


# all single good ---------------

oddsAllGood <- rbind(oddsPrevEQAGood, oddsSeqEQAGood, 
                     oddsSeqGrpEQAGood, oddsParticipateGood) %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_collapse(var, 
                            'extraEqaCL' =  c('extraEqaInstand 100', 
                                              'extraEqaRfB KS'),
                            'extraEqaPOCT' = c('extraEqaInstand 800', 
                                               'extraEqaRfB GL'))) %>%
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

# all single notFailed ---------------

oddsAllNotFailed <- rbind(oddsPrevEQANotFailed, oddsSeqGrpEQNotFailed, 
                          oddsSeqEQANotFailed, oddsParticipateNotFailed) %>%
  mutate(var = factor(var)) %>%
  mutate(var = fct_collapse(var, 
                            'extraEqaCL' =  c('extraEqaInstand 100', 
                                              'extraEqaRfB KS'),
                            'extraEqaPOCT' = c('extraEqaInstand 800', 
                                               'extraEqaRfB GL'))) %>%
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

ggpub('oddsAllNotFailed', height = 130)

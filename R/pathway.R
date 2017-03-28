replaceNA <- function(x){
  x[is.na(x)] <- ''
  str_replace(x, 'NA', '')
  }

failedYears <- eqaAll %>%
  filter(year < 2015) %>%
  select(pid, eqa, year, status, gid, round) %>%
  unique() %>%
  group_by(pid, eqa, year) %>%
  summarise(failedYear = ifelse(sum(status == 'fail') > 1, TRUE, FALSE),
            p.gid1 = unique(gid[status != 'fail'])[1], 
            p.gid2 = unique(gid[status != 'fail'])[2],
            p.gid3 = unique(gid[status != 'fail'])[3], 
            p.gid4 = unique(gid[status != 'fail'])[4],
            f.gid1 = unique(gid[status == 'fail'])[1],
            f.gid2 = unique(gid[status == 'fail'])[2],
            f.gid3 = unique(gid[status == 'fail'])[3],
            f.gid4 = unique(gid[status == 'fail'])[4]) %>%
  filter(failedYear) %>%
  ungroup() %>%
  mutate(prevYear = year, year=year+1) %>%
  mutate_at(vars(p.gid1, p.gid2, p.gid3, p.gid4, 
                 f.gid1, f.gid2, f.gid3, f.gid4), 
            as.character) %>%
  mutate_at(vars(p.gid1, p.gid2, p.gid3, p.gid4, 
                 f.gid1, f.gid2, f.gid3, f.gid4), 
            replaceNA)


allYears <- eqaAll %>%
  select(pid, eqa, year, status, gid, round, relDiff) %>%
  group_by(pid, eqa, year) %>%
  summarise(nFailure = sum(status == 'fail'),
            gid1 = unique(gid)[1], gid2 = unique(gid)[2],
            gid3 = unique(gid)[3], gid4 = unique(gid)[4],
            maxDiff = max(abs(relDiff), na.rm = T)) %>%
  ungroup() %>%
  mutate_at(vars(gid1, gid2, gid3, gid4), 
            as.character) %>%
  mutate_at(vars(gid1, gid2, gid3, gid4), 
            replaceNA)

actOnFailed <- failedYears %>% 
  left_join(allYears, by=c("pid" = "pid", "year" = "year", 
                           'eqa' = 'eqa')) %>%
  mutate(act = ifelse(is.na(nFailure), 'leftEQA', 
                      ifelse(nFailure == 0, 'cont.Good', 'cont.Fail'))) %>%
  rowwise() %>%
  mutate(devChange = ifelse(
    length(intersect(c(f.gid1, f.gid2, f.gid3, f.gid4), 
                     c(gid1, gid2, gid3, gid4))) == 0, TRUE, FALSE
  )) %>%
  mutate(sumNewDevs = sum(! c(gid1, gid2, gid3, gid4) %in%
                            c(f.gid1, f.gid2, f.gid3, f.gid4, ''))) %>%
  ungroup()

actOnFailed.All <- actOnFailed %>% 
  group_by(act, eqa) %>%
  summarise(n = n()) %>%
  group_by(eqa) %>%
  mutate(p = n/sum(n))

actOnFailed.dry <- actOnFailed %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL') %>%
  filter(act != 'leftEQA') %>% ungroup() %>% as.data.frame()

nextYearDevs <- adply(actOnFailed.dry, 1, function(x){
  fDev <- unique(c(x$f.gid1, x$f.gid2, x$f.gid3, x$f.gid4))
  nDev <- unique(c(x$gid1, x$gid2, x$gid3, x$gid4))
  fDev <- fDev[!is.na(fDev) & fDev != '']
  nDev <- nDev[!is.na(nDev) & nDev != '']
  
  nDev <- nDev[!nDev %in% fDev]
  
  f <- data.frame('type'='f', 'geraet' = fDev)
  if(length(nDev) > 0){
    n <- data.frame('type'='n', 'geraet' = nDev)
  }
  else{
    n <- data.frame()
  }
  res <- rbind(f, n)
  res$pid <- x$pid
  res$eqa <- x$eqa
  res$year <- x$year
  res
  
  
}, .expand =F)


resNextYear <- inner_join(nextYearDevs, eqaAll)

resNextYear <- resNextYear %>%
  mutate(absDiff = abs(relDiff)) %>%
  filter(absDiff < .5)

ggplot(resNextYear, aes(x=type, y=absDiff)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, .5), labels=percent) +
  scale_x_discrete(labels = c('n' = 'new device', 'f' = 'old device')) +
  xlab('')+
  ylab('deviation from assined value') +
  facet_grid(~eqa) +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(base.dir, 'fig/pathDetails.png'), 
       dpi = 600, width = 65, height= 120, units='mm')


ggplot(actOnFailed.All, aes(x=act, y=p, label = n)) +
  geom_col()+
  scale_y_continuous(labels=percent)+
  scale_x_discrete(labels = c(
    'cont.Good' = 'improved',
    'cont.Fail'= 'failed',
    'leftEQA' = "left\nEQA"
  )) +
  facet_grid(.~eqa)+
  xlab("performance after a year with 'failed' EQA") +
  ylab('percentage of participants') +  
  geom_text(
    aes(label = n, y = p + 0.02),
    position = position_dodge(0.9),
    vjust = 0) +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(base.dir, 'fig/pathAll.png'), 
       dpi = 600, width = 110, height= 120, units='mm')
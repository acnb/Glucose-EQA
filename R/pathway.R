star <- function(pval) {
  case_when(
    pval <= 0.001 ~ "***",
    pval <= 0.01 ~ " **",
    pval <= 0.05 ~ "  *",
    TRUE ~ "   "
  )
}


replaceNA <- function(x){
  x[is.na(x)] <- ''
  str_replace(x, 'NA', '')
  }

failedYears <- eqaAll %>%
  filter(year < 2016) %>%
  filter(eqa != 'Instand 100' & eqa != 'RfB KS') %>%
  select(pid, eqa, year, status, device, round) %>%
  unique() %>%
  group_by(pid, eqa, year) %>%
  summarise(failedYear = 'failed' %in% status,
            p.device1 = unique(device[status != 'failed'])[1], 
            p.device2 = unique(device[status != 'failed'])[2],
            p.device3 = unique(device[status != 'failed'])[3], 
            p.device4 = unique(device[status != 'failed'])[4],
            f.device1 = unique(device[status == 'failed'])[1],
            f.device2 = unique(device[status == 'failed'])[2],
            f.device3 = unique(device[status == 'failed'])[3],
            f.device4 = unique(device[status == 'failed'])[4]) %>%
  filter(failedYear) %>%
  ungroup() %>%
  mutate(prevYear = year, year=year+1) %>%
  mutate_at(vars(p.device1, p.device2, p.device3, p.device4, 
                 f.device1, f.device2, f.device3, f.device4), 
            as.character) %>%
  mutate_at(vars(p.device1, p.device2, p.device3, p.device4, 
                 f.device1, f.device2, f.device3, f.device4), 
            replaceNA)


allYears <- eqaAll %>%
  select(pid, eqa, year, status, device, round, relDiff) %>%
  filter(device != "Anderes Gerät" & device != "others" & device != "") %>%
  group_by(pid, eqa, year) %>%
  summarise(nFailure = sum(status == 'failed'),
            device1 = unique(device)[1], device2 = unique(device)[2],
            device3 = unique(device)[3], device4 = unique(device)[4],
            maxDiff = max(abs(relDiff), na.rm = T)) %>%
  ungroup() %>%
  mutate_at(vars(device1, device2, device3, device4), 
            as.character) %>%
  mutate_at(vars(device1, device2, device3, device4), 
            replaceNA)

actOnFailed <- failedYears %>% 
  left_join(allYears, by=c("pid" = "pid", "year" = "year", 
                           'eqa' = 'eqa')) %>%
  mutate(act = ifelse(is.na(nFailure), 'leftEQA', 
                      ifelse(nFailure == 0, 'cont.Good', 'cont.Fail'))) %>%
  ungroup()

actOnFailed.All <- actOnFailed %>% 
  group_by(act, eqa) %>%
  summarise(n = n()) %>%
  group_by(eqa) %>%
  mutate(p = n/sum(n)) %>%
  ungroup() %>%
  commonOrder()

actOnFailed.dry <- actOnFailed %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL') %>%
  filter(act != 'leftEQA') %>% 
  ungroup() %>% 
  as.data.frame()

nextYearDevs <- adply(actOnFailed.dry, 1, function(x){
  failedDev <- unique(c(x$f.device1, x$f.device2, x$f.device3, x$f.device4))
  newyearDev <- unique(c(x$device1, x$device2, x$device3, x$device4))
  failedDev <- failedDev[!is.na(failedDev) & failedDev != '']
  newyearDev <- newyearDev[!is.na(newyearDev) &
                             newyearDev != '' & 
                             newyearDev != "Anderes Gerät" & 
                             newyearDev != "others"]
  
  newDev.notFailed <- newyearDev[!newyearDev %in% failedDev]
  newDev.Failed <- newyearDev[newyearDev %in% failedDev]
  
  
  if(length(newDev.Failed) > 0){
    f <- data.frame('type'='f', 'device' = newDev.Failed)
  }
  else{
    f <- data.frame()
  }
  if(length(newDev.notFailed) > 0){
    n <- data.frame('type'='n', 'device' = newDev.notFailed)
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

resNextYear <- inner_join(nextYearDevs, eqaAll, 
                          by = c("device", "pid", "eqa", "year")) %>%
  mutate(absDiff = abs(relDiff)) %>%
  commonOrder()

pVals <- resNextYear %>%
  filter(device != "Anderes Gerät") %>%
  group_by(eqa) %>%
  summarise(p = 
              wilcox.test(absDiff[type == 'f'], 
                          absDiff[type != 'f'])[['p.value']]) %>%
  mutate(label = star(p))

print(pVals)

countsNextYear <- resNextYear %>%
  group_by(eqa, type) %>%
  summarise(n = n()) %>%
  ungroup()

print(countsNextYear)

resNextYearForGraph <- resNextYear %>%
  filter(abs(relDiff) < .45) %>%
  group_by(eqa, type) %>%
  mutate(class = cut(relDiff, breaks=seq(-.45, .45, .01), 
                     labels = seq(-.45+.01, .45, .01)-(.01/2)), n= n()) %>%
  group_by(eqa, type, class) %>%
  summarise(p = n()/n[1]) %>%
  ungroup() %>%
  mutate(class = as.numeric(as.character(class))) %>%
  commonOrder()

ggplot() +
  geom_col(data = resNextYearForGraph, aes(x=class, y = p, fill=type))+
  scale_x_continuous(limits = c(-.45,.45), labels=percent) +
  theme_pub(base_size = 10) +
  scale_fill_manual(labels=c('n' = 'new device', 'f' = 'old device'),
                    values = c('n' = '#e34234', 'f' = '#87CEEB')) +
  scale_y_continuous(labels=percent) +
  xlab("relative deviation from assigned value") +
  ylab("frequency") +
  theme(legend.title=element_blank()) +
  facet_grid(eqa~.)

ggpub("pathDetails", width = 110, height = 120)


ggplot(actOnFailed.All, aes(x=act, y=p, label = n)) +
  geom_col()+
  scale_y_continuous(labels=percent, limits = c(0, .6))+
  scale_x_discrete(labels = c(
    'cont.Good' = 'improved',
    'cont.Fail'= 'failed',
    'leftEQA' = "left\nEQA"
  )) +
  facet_grid(eqa~.)+
  xlab("performance after a year\n with 'failed' EQA") +
  ylab('percentage of participants') +  
  geom_text(
    aes(label = n, y = p + 0.02),
    position = position_dodge(0.9),
    vjust = 0) +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggpub("pathAll", width = 65, height = 120)


## path for passing ----

deviceStatus <- eqaAll %>%
  filter(eqa != 'Instand 100' & eqa != 'RfB KS') %>%
  select(eqa, year, round, pid, status, device) %>%
  unique() %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  arrange(eqa, pid, seq)

changesDevices <- ddply(deviceStatus, c('eqa', 'pid'), function(x){
  devChanges <- data.frame()
  for (i in 1:nrow(x)){
    year <- x[i, 'year']
    round <- x[i, 'round']
    device <- x[i, 'device']
    seq <- x[i, 'seq']
    status <- x[i, 'status']
    
    roundsAfter <- x[x$seq > seq & (
      ((x$year + 1) < year) & ((x$round) <= round) |
        x$year == year), ]
    
    if (nrow(roundsAfter) >= 3 ){
      devChanges <- rbind(devChanges, 
                          data.frame(status = status,
                                     changed = 
                                       length(unique(c(device, x$device))) != 1))
      
    }
    
  }
  devChanges
})

statsChangesDevices <- changesDevices %>% 
  mutate(failed = (status == 'failed')) %>%
  group_by(eqa, failed) %>%
  summarise(p = sum(changed)/n(), n = n())

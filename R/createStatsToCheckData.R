statsBySample <- eqaAll %>% group_by(eqa, year, round, split, sample) %>%
  summarise(n=n(), mean = mean(value, na.rm=T), 
            median=median(value, na.rm=T), target=target[1],
            p = sum(abs(relDiff) <= .15)/n())

statsByRound.Split <- eqaAll %>% group_by(eqa, year, round, split, pid) %>%
  summarise(status = min(status)) %>%
  group_by(eqa, year, round, split) %>%
  summarise(n = n(), p = sum(status != 'fail')/n())

statsByRound.All <- eqaAll %>% group_by(eqa, year, round, pid) %>%
  summarise(status = min(status)) %>%
  group_by(eqa, year, round) %>%
  summarise(n = n(), p = sum(status != 'fail')/n())

for(e in unique(eqaAll$eqa)){
  rtf<-RTF(paste0(base.dir,'tab/', e, ' - statsBySample.rtf'))
  addTable(rtf,statsBySample %>% filter(eqa == e))
  done(rtf)
  
  rtf<-RTF(paste0(base.dir,'tab/', e, ' - statsByRound.All.rtf'))
  addTable(rtf,statsByRound.All %>% filter(eqa == e))
  done(rtf)
  
  rtf<-RTF(paste0(base.dir,'tab/', e, ' - statsByRound.Split.rtf'))
  addTable(rtf,statsByRound.Split %>% filter(eqa == e))
  done(rtf)
}
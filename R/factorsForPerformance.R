library(forcats)

calcOdds <- function(data, x){
  ddply(data %>% as.data.frame(),
        c('eqa', 'outcome'), function(d){
          form <- as.formula(paste0('realised~', x))
          fit <- 
            glm(form, data=d, 
                family = binomial(link = "logit"))
          odds <- 
            exp(cbind(beta = coef(fit), confint(fit)))
          
          odds <- as.data.frame(odds)
          odds$var <- row.names(odds)

          if (is.factor(d[,x])){
            counts <- d %>% 
              group_by_(.dots=as.symbol(x)) %>%
              summarise(n=n())
            
            odds$n <- counts$n
            odds[1, 1:3] <- 1
            odds[1, 'var'] <- fit$xlevels[[1]][1]
          }
          else{
            odds <- odds[-1,]
            odds$n <- NA
          }
          
          odds
        }) 
}

##############

bySeqEQAAll <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  filter(!(year == 2011 & seq == 1 & round < 3)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  filter(hasFullSeq | seq > 8) %>%
  filter(abs(relDiff) < .5) %>%
  mutate(seqGrp = ifelse(seq == 1, 'new', 
                         ifelse(seq <= 10, 'intermediate', 'experienced' ))) %>%
  mutate(seqGrp = factor(seqGrp, levels=c('new', 'intermediate', 'experienced', 
                                          ordered = TRUE))) %>%
  ungroup() %>%
  select(eqa, id, seq, seqGrp, status) %>%
  unique()


bySeqEQALog <- bySeqEQAAll %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  select(eqa, id, seq, seqGrp, good, notFailed) %>%
  unique() %>%
  gather(outcome, realised, good, notFailed)


oddsSeqEQA <- rbind(calcOdds(bySeqEQALog, 'seqGrp'),
                    calcOdds(bySeqEQALog, 'seq'))

bySeqEQAGraph <- bySeqEQAAll %>%
  group_by(eqa, seqGrp, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup() %>%
  mutate(seqGrp = factor(seqGrp, 
                         levels = c('new', 'intermediate', 'experienced'),
                         ordered = TRUE))

pBySeqEQA <- ggplot(bySeqEQAGraph, aes(x=seqGrp, y=p, fill=status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(.~eqa) +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=colors.status) +
  xlab('number of previous EQAs') +
  ylab('percentage of individual EQA participations')

ggsave(paste0(base.dir, 'fig/bySeqEQA.png'),
       pBySeqEQA,  dpi = 600, width = 176, height= 150, units='mm')

################

eqasByYear <- eqaAll %>%
  select(year, pid, eqa) %>%
  unique() %>%
  group_by(year, pid) %>%
  mutate(n = n()) %>%
  mutate(extraEqa = ifelse(n == 1, 'none', as.character(eqa))) %>%
  ungroup() %>%
  mutate(extraEqa = factor(extraEqa, 
                           levels = c("none",  
                                      "Instand 800", "Instand 111", "Instand 100",
                                      "RfB GL", "RfB KS"))) %>%
  mutate(eqa = NULL) %>%
  mutate(n = NULL) 

byParticipateAll <- eqaAll %>%
  left_join(eqasByYear, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(as.character(extraEqa) != as.character(eqa)) %>%
  select(eqa, id, extraEqa, status) %>%
  unique()

byParticipateGraph <- byParticipateAll %>% 
  group_by(eqa, extraEqa, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n))

byParticipateLog <- byParticipateAll %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  dplyr::select(eqa, id, extraEqa, good, notFailed) %>%
  unique() %>%
  gather(outcome, realised, good, notFailed)

oddsParticipate <- calcOdds(byParticipateLog, extraEqa)


pByParticipate <- ggplot(byParticipateGraph, aes(x=extraEqa, 
                                       y=p, fill=status)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(.~eqa, scales = 'free_x') +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=colors.status) +
  xlab('additional participation in other EQAs') +
  ylab('percentage of individual EQA participations')



ggsave(paste0(base.dir, 'fig/byParticipate.png'),
       pByParticipate,  dpi = 600, width = 176, height= 150, units='mm')

########################

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
  unique()

byPrevEQAGraph <- byPrevEQAAll%>%
  group_by(eqa, status.prev, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()

byPrevEQALog <- byPrevEQAAll%>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(failed = ifelse(status == 'failed', 1, 0)) %>%
  select(eqa, id, status.prev, good, failed) %>%
  unique() %>%
  mutate(status.prev = factor(status.prev, ordered= FALSE)) %>%
  gather(outcome, realised, good, failed)

oddsPrevEQAGood <- calcOdds(byPrevEQALog %>% 
                           filter(outcome == 'good'), 'status.prev')

oddsPrevEQAFail <- calcOdds(byPrevEQALog %>% 
                           filter(outcome == 'failed') %>%
                           mutate(status.prev = relevel(status.prev, 'good')), 
                           'status.prev')


pByPrevEQA <- ggplot(byPrevEQAGraph, aes(x=status.prev, y=p, fill=status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(~eqa) +
  scale_fill_manual(values=colors.status) +
  scale_y_continuous(labels=percent)+
  ylab('percentage of individual EQA participations') +
  xlab('previous result')+
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(paste0(base.dir, 'fig/byPrevEQA.png'),
       pByPrevEQA,  dpi = 600, width = 176, height= 150, units='mm')


####################
byMulti <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  filter(!(year == 2011 & seq == 1 & round < 3)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  filter(hasFullSeq | seq > 8) %>%
  filter(abs(relDiff) < .5) %>%
  mutate(seqGrp = ifelse(seq == 1, 'new', 
                         ifelse(seq <= 10, 'intermediate', 'experienced' ))) %>%
  mutate(seqGrp = factor(seqGrp, levels=c('new', 'intermediate', 'experienced', 
                                          ordered = TRUE))) %>%
  ungroup() %>%
  left_join(eqasByYear, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(as.character(extraEqa) != as.character(eqa)) %>%
  left_join(prev, by=c('eqa' = 'eqa', 'seq' = 'seq', 'pid'='pid')) %>%
  filter(!is.na(status.prev)) %>%
  mutate(sharedDevice = as.character(sharedDevice)) %>%
  mutate(sharedDevice = ifelse(is.na(sharedDevice), device, sharedDevice)) %>%
  mutate(sharedDevice = factor(sharedDevice)) %>%
  mutate(good = ifelse(status == 'good', 1, 0)) %>%
  mutate(notFailed = ifelse(status != 'failed', 1, 0)) %>%
  mutate(status.prev = factor(status.prev, ordered= FALSE)) %>%
  select(eqa, id, seq, seqGrp, extraEqa, status.prev, sharedDevice, pid,
         notFailed, good)
  
res <- glm(good~seq+seqGrp+extraEqa+status.prev+sharedDevice, 
           data = byMulti %>% filter(eqa=='Instand 800'), 
           family = binomial(link = "logit"))

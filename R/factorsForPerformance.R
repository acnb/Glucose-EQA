bySeqEQA <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  filter(!(year == 2011 & seq == 1 & round < 3)) %>%
  mutate(hasFullSeq = (1 %in% seq)) %>%
  filter(hasFullSeq | seq > 8) %>%
  filter(abs(relDiff) < .5) %>%
  mutate(seqGrp = ifelse(seq <= 4, 'new', 
                         ifelse(seq <= 8, 'intermediate', 'experienced' ))) %>% 
  group_by(eqa, seqGrp, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup() %>%
  mutate(seqGrp = factor(seqGrp, 
                         levels = c('new', 'intermediate', 'experienced'),
                         ordered = TRUE))

pBySeqEQA <- ggplot(bySeqEQA, aes(x=seqGrp, y=p, color=status)) + 
  geom_point(size=1) +
  facet_grid(.~eqa) +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels=percent) +
  scale_color_manual(values=colors.status) +
  xlab('number of previous EQAs') +
  ylab('percentage')

################

eqasByYear <- eqaAll %>%
  select(year, pid, eqa) %>%
  unique() %>%
  group_by(year, pid) %>%
  mutate(n = n()) %>%
  mutate(eqa = ifelse(n == 1, 'none', eqa)) %>%
  mutate(n = NULL) 

colnames(eqasByYear)[3] <- 'extraEqa'

byParticipate <- eqaAll %>%
  left_join(eqasByYear, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(extraEqa != eqa) %>%
  group_by(eqa, extraEqa, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n))


pByParticipate <- ggplot(byParticipate, aes(x=extraEqa, 
                                       y=p, fill=status)) + 
  geom_col(position='dodge') +
  facet_grid(.~eqa, scales = 'free_x') +
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=colors.status) +
  xlab('additional participation in other EQAs') +
  ylab('percentage')


########################

prev <- eqaAll %>% 
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  filter(seq > 1) %>%
  transmute(eqa = eqa, status.prev = status, seq = seq-1, pid=pid) %>%
  distinct()


byPrevEQA <- eqaAll %>%
  group_by(pid, eqa) %>%
  mutate(seq = dense_rank(year*100+round)) %>% 
  ungroup() %>%
  left_join(prev, by=c('eqa' = 'eqa', 'seq' = 'seq', 'pid'='pid')) %>%
  filter(!is.na(status.prev)) %>%
  group_by(eqa, status.prev, status) %>%
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()


pByPrevEQA <- ggplot(byPrevEQA, aes(x=status.prev, y=p, fill=status)) +
  geom_col(position = "dodge") +
  facet_grid(~eqa) +
  scale_fill_manual(values=colors.status) +
  scale_y_continuous(labels=percent)+
  ylab('percentage') +
  xlab('previous result')+
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

########################

varLots <- lots %>%
  filter(!is.na(value)) %>%
  group_by(year, round, sample, lot, gid) %>%
  mutate(n= n()) %>%
  group_by(round, sample, gid, year) %>%
  filter(max(n) > 10) %>%
  mutate(sd =  getSfromAlgA(value), n_all = n()) %>%
  group_by(round, sample, lot, gid, year) %>%
  summarise(sd_lot = getSfromAlgA(value), 
            sd_lot_e = getStErrorForS(value),
            n_lot = n(),
            sd_all = sd[1], 
            n_all = n_all[1]) %>%
  mutate(perc = sd_lot/sd_all) %>%
  filter(!is.na(perc))

vl <- varLots %>%
  ungroup() %>%
  filter(!is.na(lot))


############################

frequentLots <- lots %>%
  filter(!is.na(value)) %>%
  filter(!is.na(lot)) %>%
  filter(abs(relDiff) < .5) %>%
  group_by(round, sample, lot, gid, year) %>%
  mutate(n_in_lot = n()) %>%
  filter(n_in_lot > 7) %>%
  group_by(round, sample, gid, year) %>%
  filter(n_distinct(lot) > 1)

diffBetweenLots <- ddply(frequentLots, 
                         c("round", "sample", "gid", "year"),  function(x){
               quantilesFromA(x)
             })

diffBetweenLots <- diffBetweenLots[order(diffBetweenLots$all, decreasing = F),]
diffBetweenLots$over <- ifelse(diffBetweenLots$all > 0.05, 'o', 'u')

diffBetweenLots$id <- as.factor(1:nrow(diffBetweenLots))

save(file=paste0(base.dir, 'generated/diffsInLots.RData'), diffBetweenLots)

pLots <- ggplot(diffBetweenLots, 
                aes(x=id, y=all, ymin=p025, ymax=p975, fill=over))+ 
  geom_pointrange(shape=21, size=.2)  + 
  coord_flip() +
  scale_fill_manual(values=c('o'= 'black', 'u' = 'white')) +
  scale_y_continuous(labels=percent) +
  theme_Publication(base_size = 10) +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") +
  xlab('') +
  ylab("maximum difference between lots\n in same EQA round")


################
library(grid)
library(gridExtra)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

g <- grid_arrange_shared_legend(pBySeqEQA, pByParticipate, pByPrevEQA, pLots)
ggsave(paste0(base.dir, 'fig/factorsForPerformance.png'),
       g,  dpi = 600, width = 176, height= 150, units='mm')


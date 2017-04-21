library(forcats)

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
  mutate(extraEqa = ifelse(n == 1, 'none', as.character(eqa))) %>%
  ungroup() %>%
  mutate(extraEqa = factor(extraEqa, 
                           levels = c("none",  
                                      "Instand 800", "Instand 111", "Instand 100",
                                      "RfB GL", "RfB KS"))) %>%
  mutate(eqa = NULL) %>%
  mutate(n = NULL) 

byParticipate <- eqaAll %>%
  left_join(eqasByYear, by=c('year' = 'year', 'pid' = 'pid')) %>%
  filter(as.character(extraEqa) != as.character(eqa)) %>%
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

####################

byDistFromRMV <- eqaAll %>%
  filter(eqa %in% c('Instand 800', 'RfB GL')) %>%
  filter(target != rmv) %>%
  filter(!(eqa=='RfB GL' & split == 'Anderes Gerät')) %>%
  mutate(dist = (target - rmv)/rmv) %>%
  mutate(absDiff = abs(relDiff)) %>%
  mutate(status.single = ifelse(absDiff > .15 | is.na(absDiff), 'fail',
                         ifelse(absDiff > .1, 'poor', 'good'))) %>%
  mutate(distGrp = ifelse(dist < -.3, '[-Inf, -0.3)', 
                   ifelse(dist < -.1, '[-0.3, -0.1)',
                   ifelse(dist < .1, '[-0.1, 0.1)',
                   ifelse(dist < .3, '[0.1, 0.3)',
                   ifelse(dist < .5, '[0.3, 0.5)', '[0.5, Inf]'))))))%>%
  mutate(distGrp = factor(distGrp, 
                          levels= c('[-Inf, -0.3)', '[-0.3, -0.1)', '[-0.1, 0.1)',
                                    '[0.1, 0.3)', '[0.3, 0.5)', '[0.5, Inf]'),
                          ordered = TRUE)) %>%
  mutate(status.single = factor(status.single, levels=c('fail', 'poor', 'good')))

byDistFromRMVPerc <- byDistFromRMV %>%
  group_by(eqa) %>%
  mutate(total = n()) %>%
  group_by(year, eqa, round, sample, split) %>%
  filter(n() > 8) %>%
  mutate(w = n()/total) %>%
  group_by(year, eqa, round, sample, split, status.single) %>%
  summarise(n=n(), dist=dist[1], w=w[1], distGrp=distGrp[1]) %>%
  group_by(year, eqa, round, sample, split) %>%
  mutate(p=n/sum(n)) %>%
  ungroup()
  

nBySplitAndDistGroup <- byDistFromRMV %>%
  group_by(year, eqa, round, sample, split) %>%
  filter(n() > 8) %>%
  mutate(distSamples = paste(year, eqa, round, sample, split, sep='-')) %>%
  group_by(distGrp, eqa) %>%
  summarise(nSplits = n_distinct(distSamples), n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(n,"/\n",nSplits))


byDistFromRMVPVal <- byDistFromRMV %>%
  group_by(eqa) %>%
  summarise(p = wilcox.test(absDiff[distGrp == '[0,0.1]'], 
                            absDiff[distGrp != '[0,0.1]'])$p.value)



pbyDistFromRMV <-  ggplot() +
  geom_boxplot(data = byDistFromRMVPerc, aes(x=distGrp, y=p, 
                                             color=status.single), 
               outlier.shape = NA) +
  geom_text(data=nBySplitAndDistGroup,
             aes(x=distGrp, y=1.1, label=label), size=1.5) +
  facet_grid(~eqa) +
  scale_color_manual(values=colors.status) +

  scale_y_continuous(labels=percent, 
                     breaks = c(0, .25, .5, .75, 1),
                     limits=c(0,1.2))+
  ylab('percentage') +
  xlab('relative difference between assigned and reference method value')+
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


####################
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

g <- grid_arrange_shared_legend(pBySeqEQA, pByParticipate, pByPrevEQA, pbyDistFromRMV)
ggsave(paste0(base.dir, 'fig/factorsForPerformance.png'),
       g,  dpi = 600, width = 176, height= 150, units='mm')


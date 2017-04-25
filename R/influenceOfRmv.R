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


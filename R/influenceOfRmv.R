byDistFromRMV <- eqaAll %>%
  filter(eqa %in% c('Instand 800', 'RfB GL')) %>%
  filter(target != rmv) %>%
  filter(!(eqa=='RfB GL' & split == 'Anderes Gerät')) %>%
  mutate(dist = (target - rmv)/rmv) %>%
  mutate(absDiff = abs(relDiff)) %>%
  mutate(status.single = ifelse(absDiff > .15 | is.na(absDiff), 'failed',
                         ifelse((abs(value-target)-2)/target > .1, 'poor', 
                         ifelse((abs(value-target)-2)/target > .05, 'acceptable',
                         'good')))) %>%
  mutate(distGrp = ifelse(dist < -.3, '[-Inf, -0.3)',
                   ifelse(dist < -.1, '[-0.3, -0.1)',
                   ifelse(dist < .1, '[-0.1, 0.1)',
                   ifelse(dist < .3, '[0.1, 0.3)',
                   ifelse(dist < .5, '[0.3, 0.5)', '[0.5, Inf]'))))))%>%
  mutate(distGrp = factor(distGrp,
                          levels= c('[-Inf, -0.3)', '[-0.3, -0.1)', '[-0.1, 0.1)',
                                    '[0.1, 0.3)', '[0.3, 0.5)', '[0.5, Inf]'),
                          ordered = TRUE)) %>%
  mutate(status.single = factor(status.single, levels=c('failed', 'poor', 
                                                      'acceptable',  'good')))

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
             aes(x=distGrp, y=1.1, label=label)) +
  facet_grid(~eqa) +
  scale_color_manual(values=colors.status) +

  scale_y_continuous(labels=percent,
                     breaks = c(0, .25, .5, .75, 1),
                     limits=c(0,1.2))+
  ylab('percentage') +
  xlab('relative difference between assigned and reference method value')+
  theme_Publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(base.dir, 'fig/byDistFromRMV.png'),
       pbyDistFromRMV,  dpi = 600, width = 176, height= 150, units='mm')

#####################

singleVsGroup <- eqaAll %>%
  filter(eqa == 'Instand 800') %>%
  filter(split != '90') %>%
  group_by(eqa, year, round, split, sample, device) %>%
  filter(n() > 7) %>%
  group_by(eqa, year, round, split, sample) %>%
  filter(n_distinct(device) > 1) %>%
  mutate(mAll = getMufromAlgA(value), sAll = getSfromAlgA(value)) %>%
  group_by(eqa, year, round, split, sample, device, mAll, sAll) %>%
  summarise(m = getMufromAlgA(value), s = getSfromAlgA(value), n= n()) %>%
  ungroup() %>%
  mutate(p = pnorm(q = abs(mAll-m)/2, mean = 0, s= sAll, lower.tail = FALSE),
         diff = abs(mAll-m)/mAll)

countGrps <- singleVsGroup %>%
  mutate(critical = ifelse(diff > 0.05 & p < .4, TRUE, FALSE)) %>%
  group_by(critical) %>%
  summarise(nGps = n(), min=min(n), max = max(n)) %>%
  ungroup()

ggplot(singleVsGroup, aes(x=p, y=diff, color=n))+
  geom_point() +
  scale_y_continuous(labels=percent)+
  scale_x_continuous(labels=percent)+
  scale_color_continuous(low='#ca0020', high = '#0571b0', trans='sqrt') + 
  theme_Publication(10)+
  theme(legend.position = 'right', legend.direction = "vertical", 
        legend.key.height = unit(1, "cm"), 
        legend.title = element_text()) +
  xlab("Mathematical probability that group\n improves evaluation of single sample")+
  ylab("Difference between device and group mean") +
  labs(color="number of\nsamples\n") 

ggsave(paste0(base.dir, 'fig/singleVsGroup.png'),
       dpi = 600, width = 85, height= 100, units='mm')

#####################

algAvsMedian <- eqaAll %>%
  filter(eqa == 'Instand 800' | eqa == 'RfB GL') %>%
  group_by(eqa, year, round, split, sample) %>%
  mutate(outlier = ifelse(
    value < (1.5*(quantile(value)[2])- (quantile(value)[2])) |
    value > (1.5*(quantile(value)[4])+ (quantile(value)[4])),
                                         TRUE, FALSE)) %>%
  summarise(median = median(value), algA = getMufromAlgA(value), 
            medianO = median(value[!outlier]),
            min=min(value), max=max(value), n=n()) %>%
  ungroup() %>%
  mutate(diff = (median-algA)/((median+algA)/2), 
         diffO=(medianO-algA)/((medianO+algA)/2),
         diffMed = (medianO-median)/((medianO+median)/2))

ggplot(algAvsMedian, aes(x=n,y=diff))+
  theme_Publication(10)+
  scale_y_continuous(limits = c(-.15, .15), labels = percent) +
  geom_point(alpha=.2) +
  xlab('number of samples')+
  ylab('median - algA')

ggsave(paste0(base.dir, 'fig/algAvsMedian.png'),
       dpi = 600, width = 85, height= 100, units='mm')

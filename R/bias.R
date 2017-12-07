bias <- eqaAll %>%
  filter(type == 'CL') %>%
  filter(!is.na(rmv)) %>%
  filter(rmv == target) %>%
  filter(sharedDevice != 'others') %>% 
  filter(abs(relDiff) < .45) %>%
  mutate(sharedDevice = as.character(sharedDevice)) %>%
  mutate(sharedDevice = if_else(sharedDevice == 'ThermoFisher/Microgen./Konelab',
                           'ThermoFisher/ Microgen./Konelab', sharedDevice)) %>%
  group_by(eqa, sharedDevice) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  filter(n > 100) %>%
  filter(nlabs > 10) %>%
  group_by(eqa, sharedDevice, rmv, type) %>%
  filter(n() > 7) %>%
  dplyr::summarise(n = n(), 
          stableMu = getMufromAlgA(value)
          ) %>%
  ungroup() %>%
  dplyr::mutate(bias.abs = rmv - stableMu, bias.rel = (rmv - stableMu)/rmv)

medBias <- bias %>%
  group_by(type, eqa, sharedDevice) %>%
  summarise(medBias = median(bias.rel))


ggplot() +
  geom_rect(data = bias, aes(fill = type),
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_boxplot(data = bias, aes(x=sharedDevice, y=bias.rel))+
  facet_grid(eqa~., scale = 'free_y') +
  theme_pub()+
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values= typeColors, guide = "none") +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab('') +
  coord_flip()+
  ylab('relative bias') +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
ggpub("bias", height = 150)

medBias <- bias %>% 
  group_by(type, sharedDevice, eqa) %>% 
  summarise(m = median (bias.rel))

print(medBias)

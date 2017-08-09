quant <- eqaAll %>%
  filter(!is.na(relDiff)) %>%
  filter(abs(relDiff) < .5) %>%
  group_by(eqa) %>%
  summarise(p025 = quantile(relDiff, .025, names = F),
            p975 = quantile(relDiff, .975, names = F)) %>%
  gather(value=x, key=q, p025, p975)

ymax = .15

poly <- data.frame(x = c(quant$x, quant$x[nrow(quant):1]), 
                   eqa = levels(eqaAll$eqa)[
                     c(quant$eqa, quant$eqa[nrow(quant):1])],
                   y=c(rep.int(0, nrow(quant)),
                       rep.int(ymax, nrow(quant))))

eqaData <- eqaAll %>%
  filter(!is.na(relDiff)) %>%
  filter(abs(relDiff) < .45) %>%
  group_by(eqa) %>%
  mutate(class = cut(relDiff, breaks=seq(-.3, .3, .01), 
                     labels = seq(-.3+.01, .3, .01)-(.01/2))
         , n= n()) %>%
  group_by(eqa, class) %>%
  summarise(p = n()/n[1]) %>%
  ungroup() %>%
  mutate(class = as.numeric(as.character(class)))
  

ggplot() +
  geom_col(data = eqaData, aes(x=class, y = p))+
  scale_x_continuous(limits = c(-.3,.3), labels=percent) +
  geom_polygon(data = poly, aes(x=x, y=y), 
               fill = "red", alpha=.1) +
  theme_Publication(base_size = 10) +
  scale_y_continuous(labels=percent) + 
  xlab("relative deviation from assigned value") +
  ylab("frequency") +
  facet_grid(eqa~.)

ggpub('precision.png', height= 120)

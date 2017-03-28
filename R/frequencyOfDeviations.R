quant <- eqaAll %>%
  filter(!is.na(relDiff)) %>%
  filter(abs(relDiff) < .5) %>%
  group_by(eqa) %>%
  summarise(p025 = quantile(relDiff, .025, names = F),
            p975 = quantile(relDiff, .975, names = F)) %>%
  gather(value=x, key=q, p025, p975)

ymax = .05

poly <- data.frame(x = c(quant$x, quant$x[nrow(quant):1]), 
                   eqa = c(quant$eqa, quant$eqa[nrow(quant):1]),
                   y=c(rep.int(0, nrow(quant)),
                       rep.int(ymax, nrow(quant))))

ggplot(eqaAll %>%
         filter(!is.na(relDiff)) %>%
         filter(abs(relDiff) < .5), aes(x=relDiff)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = .01) +
  scale_x_continuous(limits = c(-.3,.3), labels=percent) +
  geom_polygon(data = poly, aes(x=x, y=y), 
               fill = "red", alpha=.1) +
  theme_Publication(base_size = 10) +
  scale_y_continuous(labels=percent) + 
  xlab("relative deviation from assigned value") +
  ylab("frequency") +
  facet_grid(eqa~.)

ggsave(paste0(base.dir, 'fig/precision.png'), 
       dpi = 600, width = 85, height= 120, units='mm')

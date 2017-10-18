dataCharFunc <- eqaAll %>%
  filter(!is.na(rmv)) %>%
  filter(!str_detect(device, "Andere")) %>%
  filter(!str_detect(device, "andere")) %>%
  mutate(charDev = ifelse(is.na(sharedDevice), 
                          as.character(device),
                          as.character(sharedDevice))) %>%
  filter(charDev != 'Other devices') %>% 
  filter(charDev != 'Roche: other devices') %>% 
  filter(charDev != 'Abbott: other devices') %>% 
  filter(abs(relDiff) < .45) %>%
  mutate(charDev = if_else(charDev == 'ThermoFisher/Microgen./Konelab',
                           'ThermoFisher/ Microgen./Konelab', charDev)) %>%
  group_by(eqa, charDev, rmv, type) %>%
  filter(n() > 7) %>%
  group_by(eqa, charDev) %>%
  filter(n_distinct(rmv) >= 5) %>%
  filter(n_distinct(pid) >= 10) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  mutate(eqa = fct_collapse(eqa, 'RfB' = c('POCT-RfB', 'CL-RfB'),
                            'Instand' = c('POCT-Instand', 'CL-Instand')))

getParamNls <- function(x, idx){
  data <- x %>%
    dplyr::slice(idx) %>%
    group_by(rmv, eqa) %>%
    summarise(
      sd = getSfromAlgA(value),
      sdE = max(.Machine$double.eps, getStErrorForS(value)),
      targetAlgA = getMufromAlgA(value), n=n()) %>%
    mutate(cv = sd/targetAlgA) %>%
    mutate(w = (1/sdE^2)/sum(1/sdE^2))
  
  model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, 
               data=data, weights = x$w,
               control = nls.control(warnOnly = TRUE), start=c(a=.02, b=.02))
  
  c(abs(coef(model)[['a']]), coef(model)[['b']])
}

charFuncConf <- ddply(dataCharFunc, c('type', 'eqa', 'charDev'), 
                         function(x){
                           btrp <- boot(x, statistic = getParamNls, R=2000)
                           ciA <- boot.ci(btrp, index = 1, type = "perc")
                           ciB <- boot.ci(btrp, index = 2, type = "perc")
                           data.frame(
                             a.p025 = ciA$percent[1, 4], 
                             a.p975 = ciA$percent[1, 5],
                             b.p025 = ciB$percent[1, 4],
                             b.p975 = ciB$percent[1, 5]
                           )
                         })

targetsAndImprecision <- dataCharFunc %>%
  group_by(eqa, charDev, rmv, type) %>%
  summarise(
    sd = getSfromAlgA(value),
    sdE = max(.Machine$double.eps, getStErrorForS(value)),
    targetAlgA = getMufromAlgA(value), n=n()) %>%
  mutate(cv = sd/targetAlgA) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  ungroup() %>%
  # filter
  join(charFuncConf %>% select(eqa, charDev), type = "inner") %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))


charFuncConf2 <- charFuncConf

paramsCharFunc <- ddply(targetsAndImprecision, 
                        c('eqa', 'charDev', 'type'), 
                        function(x){
                          model <- NULL
                          try({
                            model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, data=x, weights = x$w, 
                                         control = nls.control(warnOnly = T),  start=c(a=.02, b=.02))
                          })
                          if(!is.null(model)){
                            data.frame(r=resid(model), x=x$targetAlgA, w=x$w, cv = x$cv, 
                                       a = abs(coef(model)[['a']]), b = coef(model)[['b']])
                          }else{
                            data.frame(r=NA, x=x$targetAlgA, w=x$w)
                          }
                        })

resids <- paramsCharFunc %>%
  select(eqa, charDev, type, x, w, r, cv)

paramsCharFunc2 <- paramsCharFunc %>% 
  select(eqa, charDev, type, a, b) %>%
  unique() %>%
  left_join(charFuncConf, by=c('eqa' = 'eqa',
                               'charDev' = 'charDev',
                               'type' =  'type'))


grid <- seq(from=30, to=500, by=.5)

lines.char.func <- ddply(paramsCharFunc2, 
                         c('eqa', 'charDev', 'type'), 
                         function(x){
                           data.frame(
                             x=grid,
                             ymin = ((x[['a.p025']]^2+(x[['b.p025']]*grid)^2)^.5)/grid,
                             y= ((x[['a']]^2+(x[['b']]*grid)^2)^.5)/grid,
                             ymax = ((x[['a.p975']]^2+(x[['b.p975']]*grid)^2)^.5)/grid
                           )
                           })

lines.char.func <- lines.char.func %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))

ggplot() +
  geom_rect(data = resids, aes(fill = type),
               xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = resids, 
             aes(x=x, y=cv, alpha=w, color=eqa)) +
  geom_line(data=lines.char.func, aes(x=x, y=y, color=eqa)) +
  geom_ribbon(data=lines.char.func, aes(x=x, ymin=ymin,
                                        ymax=pmin(ymax, .3), fill=eqa),
              alpha=0.3) +
  facet_wrap(~charDev, labeller = label_wrap_gen(width=19)) +
  theme_pub(base_size = 10) + 
  scale_alpha(guide = "none") +
  scale_color_manual(values = eqaColors, 
                     guide = guide_legend(override.aes = 
                                            list(alpha = 1,
                                                 fill = NA,
                                                 shape = 15,
                                                 linetype = 0))) +
  scale_fill_manual(values = c(typeColors, eqaColors), guide = "none") + 
  scale_x_continuous(sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "consensus value (mmol/L)"), 
                     name='consensus value (mg/dL)') +
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45),
        plot.subtitle = element_text(hjust = .5)) +
  ggtitle('relative imprecision depending on concentration',
          subtitle = 'modeled using the characteristic function') +
  ylab('relative imprecision (spread / consensus value)')

ggpub('charFunc', height = 240)


outliers <- resids %>%
  filter(r > 20 | r < - 15) %>%
  mutate(label = round(r, 1)) %>%
  mutate(r = if_else(r > 20, 20, r)) %>%
  mutate(r = if_else(r < -15, -15, r))

ggplot() +
  geom_rect(data = resids, aes(fill = type), 
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = resids, aes(x=x, y=r, alpha=w, color=eqa))+
  geom_point(data = outliers, aes(x=x, y=r, alpha=w, color=eqa), shape=17)+
  geom_text_repel(data = outliers, aes(x=x, y=r, color=eqa, label = label), 
                  size = 2) +
  geom_smooth(data = resids, aes(x=x, y=r, weight=w, color=eqa), method = 'gam')+
  facet_wrap(~charDev, labeller = label_wrap_gen(width=19)) +
  theme_pub(base_size = 10) + 
  scale_alpha(guide = "none") +
  scale_fill_manual(values = typeColors, guide = "none") + 
  scale_color_manual(values = eqaColors, 
                     guide = guide_legend(override.aes = 
                                            list(alpha = 1,
                                                 fill = NA,
                                                 shape = 15,
                                                 linetype = 0))) +
  scale_x_continuous(sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "assigned value (mmol/L)"), 
                     name='assigned value (mg/dL)') +
  scale_y_continuous(sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "residuals (mmol/L)"), 
                     name='residuals (mg/dL)',
                     limits = c(-15, 20)) +
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45)) +
  ggtitle('residuals of characteristic function fit')

ggpub('residsCharFunc', height = 240)

## table ----

cv.by.device <-  resids %>%
  group_by(type, eqa, charDev) %>%
  summarise(mean.cv.w = weighted.mean(cv, w), mean.cv = mean(cv)) %>%
  ungroup() %>%
  join(paramsCharFunc2, by=c('type' = 'type',
                             'eqa' = 'eqa', 
                             'charDev' = 'charDev')) %>%
  mutate_at(vars(-type, -eqa, -charDev), round, digits=3)

cv.by.device.diff <- cv.by.device %>%
  group_by(charDev) %>%
  filter('Instand' %in% eqa & 'RfB' %in% eqa) %>%
  summarise(diff = abs(mean.cv.w[eqa == 'Instand']
                       - mean.cv.w[eqa == 'RfB']))


cv.by.device.table <- cv.by.device %>%
  group_by(charDev, type) %>%
  summarise(
    eqa = paste0(eqa, collapse = "\n"),
    mean = paste0(mean.cv.w, collapse = "\n"),
    alpha = paste0(a, ' (', a.p025, ' - ', a.p975, ')', collapse = "\n"),
    beta = paste0(b, ' (', b.p025, ' - ', b.p975, ')', collapse = "\n")
    ) %>%
  ungroup() %>%
  transmute(device = charDev, type=type, eqa = eqa, 
            mean=mean, alpha=alpha, beta=beta)

rtf<-RTF(here('tab', 'precision.rtf'))
addTable(rtf,cv.by.device.table)
done(rtf)

params.single.devices <- eqaAll %>%
  filter(!is.na(rmv)) %>%
  filter(!str_detect(device, "Andere")) %>%
  filter(!str_detect(device, "andere")) %>%
  mutate(charDev = ifelse(is.na(sharedDevice), 
                          as.character(device),
                          as.character(sharedDevice))) %>%
  filter(charDev != 'Other devices') %>% 
  filter(abs(relDiff) < .45) %>%
  mutate(charDev = if_else(charDev == 'ThermoFisher/Microgen./Konelab',
                           'ThermoFisher/ Microgen./Konelab', charDev)) %>%
  group_by(eqa, charDev) %>%
  mutate(n = n(), nlabs = n_distinct(pid)) %>%
  ungroup() %>%
  filter(n > 100) %>%
  filter(nlabs > 10) %>%
  group_by(eqa, charDev, rmv, type) %>%
  filter(n() > 7) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = max(.Machine$double.eps, getStErrorForS(value)),
            targetAlgA = getMufromAlgA(value), n=n()) %>%
  mutate(cv = sd/targetAlgA) %>%
  group_by(eqa, charDev) %>%
  filter(n_distinct(rmv) >= 5) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  ungroup() %>%
  filter(sum(n) > 100) %>%
  mutate(cv = sd/targetAlgA) %>%
  ungroup() %>%
  mutate(eqa = fct_collapse(eqa, 'RfB' = c('POCT-RfB', 'CL-RfB'),
                            'Instand' = c('POCT-Instand', 'CL-Instand')))


 param.char.func <- ddply(params.single.devices, c('type', 'eqa', 'charDev'), 
                             function(x){
  model <- NULL
  try({
    model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, 
                 data=x, weights = x$w,
                 control = nls.control(warnOnly = TRUE), start=c(a=.02, b=.02))
  })
  if(!is.null(model)){
    data.frame(a=coef(model)[['a']],
               b=coef(model)[['b']],
               n = sum(x$n))
  }else{
    data.frame(a=NA,
               b=NA,
               n = sum(x$n))
  }
})

params.single.devices <- params.single.devices %>%
  join(param.char.func %>% select(eqa, charDev), type = "inner") %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))


grid <- seq(from=min(params.single.devices$targetAlgA),
            to=max(params.single.devices$targetAlgA), by=.5)

lines.char.func <- ddply(param.char.func, 
                         c('eqa', 'charDev', 'type'), 
                         function(x){
                           data.frame(x=grid, 
                                      y= ((x[['a']]^2+(x[['b']]*grid)^2)^.5)/grid)
                           })

lines.char.func <- lines.char.func %>%
  commonOrder() %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))



eqaColors = c('Instand' = '#f5961e',
              'RfB' = '#4ba1d1')

ggplot() +
  geom_rect(data = params.single.devices, aes(fill = type),
               xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = params.single.devices, 
             aes(x=targetAlgA, y=cv, alpha=w, color=eqa)) +
  geom_line(data=lines.char.func, aes(x=x, y=y, color=eqa)) +
  facet_wrap(~charDev, labeller = label_wrap_gen(width=19)) +
  theme_pub(base_size = 10) + 
  scale_alpha(guide = "none") +
  scale_color_manual(values = eqaColors, 
                     guide = guide_legend(override.aes = 
                                            list(alpha = 1,
                                                 fill = NA,
                                                 shape = 15,
                                                 linetype = 0))) +
  scale_fill_manual(values = typeColors, guide = "none") + 
  scale_x_continuous(sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "assigned value (mmol/l)"), 
                     name='assigned value (mg/dl)') +
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45)) +
  ggtitle('fitted characteristic function') +
  ylab('relative imprecision')

ggpub('charFunc', height = 240)


resids <- ddply(params.single.devices, 
                c('eqa', 'charDev', 'type'), 
                function(x){
  model <- NULL
  try({
    model <- nls(sd ~ (a^2+(b*targetAlgA)^2)^.5, data=x, weights = x$w, 
                 control = nls.control(warnOnly = T),  start=c(a=.02, b=.02))
  })
  if(!is.null(model)){
    data.frame(r=resid(model), x=x$targetAlgA, w=x$w)
  }else{
    data.frame(r=NA, x=x$targetAlgA, w=x$w)
  }
})

resids <- resids %>%
  mutate(charDev = fct_reorder(charDev, as.numeric(type)))

ggplot() +
  geom_rect(data = resids, aes(fill = type), 
            xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
  geom_point(data = resids, aes(x=x, y=r, alpha=w, color=eqa))+
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
                                name = "assigned value (mmol/l)"), 
                     name='assigned value (mg/dl)') +
  scale_y_continuous(sec.axis = 
                       sec_axis(~./mmolConvFactor, 
                                name = "residuals (mmol/l)"), 
                     name='residuals (mg/dl)') +
  theme(strip.text.x = element_text(size = 5), 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45)) +
  ggtitle('residuals of characteristic function fit')

ggpub('residsCharFunc', height = 240)

## table ----

cv.by.device <-  params.single.devices %>%
  group_by(type, eqa, charDev) %>%
  summarise(mean.cv.w = weighted.mean(cv, w), mean.cv = mean(cv)) %>%
  ungroup() %>%
  join(param.char.func, by=c('type' = 'type',
                             'eqa' = 'eqa', 
                             'charDev' = 'charDev')) %>%
  select(charDev, type, eqa, mean.cv.w, mean.cv, a, b) %>%
  mutate_at(vars(mean.cv.w, mean.cv, a, b), round, digits=3)

cv.by.device.table <- cv.by.device %>%
  group_by(charDev, type) %>%
  summarise(
    eqa = paste0(eqa, collapse = "\n"),
    mean = paste0(mean.cv.w, collapse = "\n"),
    alpha = paste0(a, collapse = "\n"),
    beta = paste0(b, collapse = "\n")
    ) %>%
  ungroup() %>%
  transmute(device = charDev, type=type, eqa = eqa, 
            mean=mean, alpha=alpha, beta=beta)

rtf<-RTF(here('tab', 'precision.rtf'))
addTable(rtf,cv.by.device.table)
done(rtf)

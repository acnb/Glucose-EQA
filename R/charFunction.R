 params.single.devices <- eqaAll %>%
  filter(!is.na(sharedDevice)) %>%
  group_by(eqa, sharedDevice, rmv) %>%
  filter(n() > 7) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = getStErrorForS(value),
            targetAlgA = getMufromAlgA(value), n=n()) %>%
  group_by(eqa, sharedDevice) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  ungroup() %>%
  mutate(cv = sd/targetAlgA) %>%
  mutate(uniqueDevice = paste0(sharedDevice, "\n(", eqa, ')'))


param.char.func <- ddply(params.single.devices, c('uniqueDevice'), 
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

param.char.func <- param.char.func %>%
  filter(n > 500) %>%
  filter(!is.na(a)) 

params.single.devices <- params.single.devices %>%
  filter(uniqueDevice %in% param.char.func$uniqueDevice)


grid <- seq(from=min(params.single.devices$targetAlgA),
            to=max(params.single.devices$targetAlgA), by=.5)

lines.char.func <- ddply(param.char.func, 
                         c('uniqueDevice'), function(x){
                           data.frame(x=grid, 
                                      y= ((x[['a']]^2+(x[['b']]*grid)^2)^.5)/grid)
                           })

ggplot() +
  geom_point(data = params.single.devices, 
             aes(x=targetAlgA, y=cv, alpha=w)) +
  geom_line(data=lines.char.func, aes(x=x, y=y)) +
  facet_wrap(~uniqueDevice) +
  theme_Publication(base_size = 10) + 
  theme(legend.position="none", strip.text.x = element_text(size = 6)) +
  ggtitle('characteristic function for device subgroups') +
  xlab('assigned value') +
  ylab('coefficient of variation')

ggsave(paste0(base.dir, 'fig/charFunc.png'), 
       dpi = 600, width = 176, height= 176, units='mm')


resids <- ddply(params.single.devices, c('uniqueDevice'), function(x){
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

ggplot(resids, aes(x=x, y=r, weight=w, alpha=w)) +
  geom_point()+
  geom_smooth()+
  facet_wrap(~uniqueDevice, scales = "free_y") +
  theme_Publication(base_size = 10) + 
  theme(legend.position="none") +
  xlab('assigned value') +
  ggtitle('cleaned') +
  ylab('residuals')

ggsave(paste0(base.dir, 'fig/residsCharFunc.png'), 
       dpi = 600, width = 176, height= 176, units='mm')

cv.by.device <- eqaAll %>%
  filter(!is.na(sharedDevice)) %>%
  group_by(eqa, sharedDevice, rmv) %>%
  filter(n() > 7) %>%
  mutate(uniqueDevice = paste0(sharedDevice, "\n(", eqa, ')')) %>%
  filter(uniqueDevice %in% param.char.func$uniqueDevice) %>%
  summarise(sd = getSfromAlgA(value), 
            sdE = getStErrorForS(value),
            targetAlgA = getMufromAlgA(value),
            uniqueDevice = uniqueDevice[1]) %>%
  mutate(cv = sd/targetAlgA) %>%
  group_by(uniqueDevice) %>%
  mutate(w = (1/sdE^2)/sum(1/sdE^2)) %>%
  summarise(mean.cv.w = weighted.mean(cv, w), mean.cv = mean(cv)) %>%
  ungroup()

cv.by.device <- cv.by.device %>%
  left_join(param.char.func, by=c('uniqueDevice' = 'uniqueDevice')) %>%
  select(uniqueDevice, mean.cv.w, mean.cv, a, b)

rtf<-RTF(paste0(base.dir,'tab/precision.rtf'))
addTable(rtf,cv.by.device)
done(rtf)
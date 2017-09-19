riskpair <- read_csv(here::here("data", "riskpair.csv"))
lkpRiskCat <- read_csv(here::here("data", "lkpRiskCat.csv"))

segCats <- function(measuredValue, referenceValue){
  breaks <- sort(unique(c(lkpRiskCat$ABSLB, lkpRiskCat$ABSUB)))
  risks <- vector(length = length(measuredValue))
  for(i in 1:length(measuredValue)){
    risks[[i]] <- as.numeric(riskpair[
      riskpair$RefVal == round(referenceValue[i]) &
         riskpair$MeasVal == round(measuredValue[i]),
      "RiskFactor"])
  }

  table(cut(abs(risks), breaks = breaks, labels = lkpRiskCat$RiskCatLabel))
}


biasBudgetSEG.CV <- function(cv, sd=3){
  borders <- data.frame()
  for(i in 0:500){
    temp <- riskpair %>%
      dplyr::filter(RefVal == i) %>%
      summarise(
        ref = i,
        lower = max(MeasVal[RiskFactor >= 1.5]),
        upper = min(MeasVal[RiskFactor <= - 1.5]))
    
    borders <- rbind(borders, temp)
  }
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+ref*cv*sd),
           diffLower = lower - (ref-ref*cv*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])

  budget
} 


biasBudgetSEG.CharFunc <- function(a, b, sd=3){
  borders <- data.frame()
  for(i in 0:500){
    temp <- riskpair %>%
      dplyr::filter(RefVal == i) %>%
      summarise(
        ref = i,
        lower = max(MeasVal[RiskFactor >= 1.5]),
        upper = min(MeasVal[RiskFactor <= - 1.5]))
    
    borders <- rbind(borders, temp)
  }
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+sqrt(a^2+(ref*b)^2)*sd),
           diffLower = lower - (ref-sqrt(a^2+(ref*b)^2)*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])
  
  budget
} 


budgetByDevice.CV <- ddply(cv.by.device, c('charDev', 'eqa'),
                        function(x){
                          biasBudgetSEG.CV(x[1, 'mean.cv.w'])
                        })

rtf<-RTF(here('tab', 'budgetByDevice.CV.rtf'))
addTable(rtf,budgetByDevice.CV)
done(rtf)


budgetByDevice.CharFunc <- ddply(cv.by.device, c('charDev', 'eqa'),
                           function(x){
                             biasBudgetSEG.CharFunc(x[1, 'a'], x[1, 'b'])
                           })

rtf<-RTF(here('tab', 'budgetByDevice.CharFunc.rtf'))
addTable(rtf,budgetByDevice.CharFunc)
done(rtf)

budgetByDevice.graph <- budgetByDevice.CV %>%
  mutate(math = 'CV') %>%
  rbind(budgetByDevice.CharFunc %>% 
          mutate(math = "characteristic\nfunction")) %>%
  filter(charDev != 'others') %>%
  mutate(charDev = str_replace(charDev, "\n", " ")) %>%
  mutate(charDev = parse_factor(charDev, levels= unique(charDev)))

ggplot(budgetByDevice.graph ,
       aes(x=charDev, ymin=budgetLower, ymax=budgetUpper, color=eqa))+
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge())+
  theme_pub() +
  ylab('allowed bias (mg/dl) Surveillance Error Grid') +
  xlab('device') +
  coord_flip() +
  facet_grid(.~math) +
  theme(legend.title = element_blank(), axis.text.y = element_text(size=8))

ggpub('allowedBias', height=230)


## bias budget klee simulation ----

biasBudgetSim.CV <- function(cv, sd=3){
  insulinCategories <-  c(80, 90, 110, 130, 150, 175, 200, 250, 300, 350, 400)
  
  borders <- data.frame(ref = insulinCategories,
                        lower = c(-Inf, -Inf, 
                                  insulinCategories[
                                    1:(length(insulinCategories)-2)]),
                        upper = c(insulinCategories[
                          3:(length(insulinCategories))],
                                  Inf, Inf)) 
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+ref*cv*sd),
           diffLower = lower - (ref-ref*cv*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])
  
  budget
}


biasBudgetSim.CharFunc <- function(a, b, sd=3){
  insulinCategories <-  c(80, 90, 110, 130, 150, 175, 200, 250, 300, 350, 400)
  
  borders <- data.frame(ref = insulinCategories,
                        lower = c(-Inf, -Inf, 
                                  insulinCategories[
                                    1:(length(insulinCategories)-2)]),
                        upper = c(insulinCategories[
                          3:(length(insulinCategories))],
                          Inf, Inf)) 
  
  budget <- borders %>%
    mutate(diffUpper = upper - (ref+sqrt(a^2+(ref*b)^2)*sd),
           diffLower = lower - (ref-sqrt(a^2+(ref*b)^2)*sd)) %>%
    summarise(budgetLower = max(diffLower), 
              refLower = ref[which.max(diffLower)],
              budgetUpper = min(diffUpper), 
              refUpper = ref[which.min(diffUpper)])
  
  budget
} 


budgetByDevice.Sim.CV <- ddply(cv.by.device, c('charDev', 'eqa'),
                           function(x){
                             biasBudgetSim.CV(x[1, 'mean.cv.w'])
                           })


rtf<-RTF(here('tab', 'budgetByDevice.Sim.CV.rtf'))
addTable(rtf,budgetByDevice.Sim.CV)
done(rtf)


budgetByDevice.Sim.CharFunc <- ddply(cv.by.device, c('charDev', 'eqa'),
                                 function(x){
                                   biasBudgetSim.CharFunc(x[1, 'a'], x[1, 'b'])
                                 })

rtf<-RTF(here('tab', 'budgetByDevice.Sim.CharFunc.rtf'))
addTable(rtf,budgetByDevice.CharFunc)
done(rtf)


budgetByDevice.Sim.graph <- budgetByDevice.Sim.CV %>%
  mutate(math = 'CV') %>%
  rbind(budgetByDevice.Sim.CharFunc %>% 
          mutate(math = "characteristic\nfunction")) %>%
  filter(charDev != 'others') %>%
  mutate(charDev = str_replace(charDev, "\n", " ")) %>%
  mutate(charDev = parse_factor(charDev, levels= unique(charDev)))

ggplot(budgetByDevice.Sim.graph ,
       aes(x=charDev, ymin=budgetLower, ymax=budgetUpper, color=eqa))+
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge())+
  theme_pub() +
  facet_grid(.~math) +
  ylab('allowed bias (mg/dl) TGC-Simulation') +
  xlab('device') +
  coord_flip() +
  theme(legend.title = element_blank(), axis.text.y = element_text(size=8))

ggpub('allowedBiasSim', height=230)

## summary stats

budgetAll <- rbind(
  budgetByDevice.CV %>%  mutate(math = 'CV', risk = 'SEG'),
  budgetByDevice.CharFunc %>%  mutate(math = 'charFunc', risk = 'SEG'),
  budgetByDevice.Sim.CV %>%  mutate(math = 'CV', risk = 'sim'),
  budgetByDevice.Sim.CharFunc %>% mutate(math = "charFunc", risk = 'sim')
)


statsbudget <- budgetAll %>%
  mutate(budgetLower = budgetLower*-1) %>%
  gather(key = 'type', value = 'budget', budgetLower, budgetUpper) %>%
  group_by(eqa, risk) %>%
  summarise(min = min(budget),
            p25 = quantile(budget, names = FALSE, probs=.25),
            med = median(budget),
            p75 = quantile(budget, names = FALSE, probs=.75),
            max = max(budget))

## bias budget explanation graph ----

borders <- data.frame()
for(i in 0:500){
  temp <- riskpair %>%
    dplyr::filter(RefVal == i) %>%
    summarise(
      ref = i,
      lower = max(MeasVal[RiskFactor >= 1.5]),
      upper = min(MeasVal[RiskFactor <= - 1.5]))
  
    borders <- rbind(borders, temp)
}

borders <- as.data.frame(borders) %>%
  mutate(lower = ifelse(is.infinite(lower), NA, lower)) %>%
  mutate(upper = ifelse(is.infinite(upper), NA, upper))


meanCharFunc <- cv.by.device %>%
  filter(charDev != "others") %>%
  filter(eqa == 'Instand 800' | eqa == "RfB GL") %>%
  summarise(a = median(a), b=median(b)) %>%
  as.list()

charFunc <- function(x){
  sqrt(meanCharFunc[["a"]]^2+(x*meanCharFunc[["b"]])^2)
}

xPos <- borders$ref[which.min(borders$upper-(borders$ref+charFunc(borders$ref)*3))]
xNeg <- borders$ref[which.min((borders$ref-charFunc(borders$ref)*3)-borders$lower)]

budgetPos <- data.frame(
  x = c(xPos, xPos),
  y = c(xPos+3*charFunc(xPos), 
        borders[borders$ref ==xPos, "upper"])
  )

budgetNeg <- data.frame(
  x = c(xNeg, xNeg),
  y = c(xNeg-3*charFunc(xNeg), 
        borders[borders$ref ==xNeg, "lower"])
)

ggplot()+
  geom_point(data= riskpair, aes(x=RefVal, y=MeasVal, color = RiskFactor*2)) +
  geom_line(data = borders, aes(x=ref, y=lower), color='black') + 
  geom_line(data = borders, aes(x=ref, y=upper), color='black') + 
  scale_color_gradientn(
    colours = c("brown", "red", "orange", "yellow", "green",
                "yellow", "orange", "red", 'brown'), 
    values = scales::rescale(c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5))
    ) +
  geom_ribbon(data=data.frame(x=0:300),
              aes(x = x, 
                  ymin=pmax(0, x-charFunc(x)*3), 
                  ymax=pmin(300, x+charFunc(x)*3)), 
              fill = "grey50", color='grey50', alpha=.5) +
  geom_line(data=budgetPos, aes(x=x, y=y), 
            color="blue", size = 1.2) + 
  geom_line(data=budgetNeg, aes(x=x, y=y), 
            color="blue", size = 1.2) + 
  scale_x_continuous(limits = c(0, 300)) +
  scale_y_continuous(limits = c(0, 300)) +
  xlab('true value (mg/dl)') + 
  ylab('measured value (mg/dl)')+
  theme_pub() +
  theme(legend.position = "none")
  
ggpub('biasBudget')

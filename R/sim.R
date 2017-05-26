library(forcats)

funcs <- list('median' = median,
              'huberM' = getMufromAlgA)

set.seed('0304')
res <- ldply(4:100, function(n){
  d <- ldply(seq.int(0.01, 0.1, by=.001), function(cv){
    r <- rdply(200, {
      mean <- runif(1, 1, 50)
      sd <- mean*cv
      nOut <- round(n*runif(1, 0.1, 0.2))
      samples <- rnorm(n, mean=mean, sd=sd)
      samples <- c(samples,
                   runif(nOut, 3, 5)*mean*sd*(round(runif(nOut,0,1))*2-1))
      ldply(funcs, function(f){
        data.frame(diff = (mean-f(samples))/mean)
      }, .id='method')
    }, .id = NULL)
    r$cv <- cv
    r
  })
  d$n <- n
  d
}, .progress = 'text')

resAnalysed <- res %>%
  mutate(e=1.253*round(cv/sqrt(n), 3)) %>%
  group_by(e, method) %>%
  summarise(p975 = quantile(diff, .975),
            p025 = quantile(diff, .025), n=n()) 

ggplot(resAnalysed) +
  geom_ribbon(aes(x=e, ymin=p025, ymax=p975, fill=method),
              alpha=.5) +
  theme_Publication() +
  scale_y_continuous(label=percent) +
  xlab('estimated relative standard error') +
  ylab("relative deviation from true mean\n (central 95%)")

ggsave(paste0(base.dir, 'fig/stdError.png'),
       dpi = 600, width = 85, height= 100, units='mm')

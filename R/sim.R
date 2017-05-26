funcs <- list('median' = median,
              'huberM' = getMufromAlgA)

res <- ldply(4:100, function(n){
  d <- ldply(seq.int(0.01, 0.1, by=.001), function(cv){
    r <- rdply(200, {
      mean <- runif(1, 1, 50)
      sd <- mean*cv
      nOut <- round(n*runif(1, 0.05, 0.15))
      samples <- rnorm(n, mean=mean, sd=sd)
      samples <- c(samples,
                   runif(nOut, 3, 5)*mean*sd*(round(runif(1,0,1))*2-1))
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
  mutate(e=round(cv/sqrt(n), 3)) %>%
  group_by(e, method) %>%
  summarise(p975 = quantile(diff, .975),
            p025 = quantile(diff, .025), n=n())

ggplot(resAnalysed) +
  geom_line(aes(x=e, y=p975, color=method))+
  geom_line(aes(x=e, y=p025, color=method)) +
  theme_Publication()

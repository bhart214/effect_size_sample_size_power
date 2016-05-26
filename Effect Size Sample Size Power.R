
### Effect Size vs. Sample Size Requirement for T-Test (Difference Between Averages) ###
### Brian Hart ###
### March 20, 2016 ###

### This was significantly borrowed from and inspired by Kim Larson's Stitchfix Multithreaded Blog post:
### http://multithreaded.stitchfix.com/blog/2015/05/26/significant-sample/

library(ggplot2)

# replace 0.5 with desired effect size
# effect size = (mu1 - mu2) / pooled std dev
desired_effect_size = .5

# create vector of effect sizes
effect_sizes <- seq(from = 0.2, to = 3.0, by = 0.01)

# initialize data frame for effect size column and sample size column
ES_and_N <- data.frame(matrix(nrow = length(effect_sizes), ncol = 2))

# ES = effect size; ni = sample size requirement for given effect
names(ES_and_N) <- c("ES", "ni")

# compute sample size requirements for vector of effect sizes
# significance level is set to .05 and power is set to .8
for (i in 1:length(effect_sizes)){
  ES_and_N[i, "ES"] <- effect_sizes[i]
  ES_and_N[i, "ni"] <- power.t.test(sig.level = 0.05, type = "one.sample", d = effect_sizes[i], sd = 1, alternative = 'two.sided', power = 0.8)$n  
}

# plot curve of effect size vs. sample size; mark desired effect with red dot
ggplot(data = ES_and_N, aes(x = ES, y = ni)) + 
  geom_line() + xlab("Effect Size") +   ylab("N") +  
  ggtitle("Effect Size vs. Sample Size for Paired T-Test") +
#  ylim(0, 200) +
  scale_x_continuous() +
  geom_point(data = ES_and_N, aes(x = ES_and_N[ES_and_N$ES == desired_effect_size,]$ES, y = ES_and_N[ES_and_N$ES == desired_effect_size,]$ni), colour = "red", size = 5) +
  geom_text(data = ES_and_N, aes(x = ES_and_N[ES_and_N$ES == desired_effect_size,]$ES, y = ES_and_N[ES_and_N$ES == desired_effect_size,]$ni, label = paste(ES_and_N[ES_and_N$ES == desired_effect_size,]$ES, ",", round(ES_and_N[ES_and_N$ES == desired_effect_size,]$ni, 2)), hjust=-.5, vjust=-.5))


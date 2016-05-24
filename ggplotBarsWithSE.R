library(ggplot2); library(dplyr); library(Hmisc)

# Set up fake data
dat = data.frame("vals"=c(rnorm(50) + 0.5, rnorm(50)+2), "cond"=c(rep(TRUE, 50), rep(FALSE, 50)))

#check for desired values
dat %>% group_by(cond) %>% dplyr::summarize(se_plus(vals),
                                            se_minus(vals),
                                            "se_1"=smean.sdl(vals, mult=1/sqrt(length(vals)))[1],
                                            "se_2"=smean.sdl(vals, mult=1/sqrt(length(vals)))[2],
                                            "se_3"=smean.sdl(vals, mult=1/sqrt(length(vals)))[3])


# Wrap this mess up in a new geom...something like geom_barwithse
#Define two functions to get upper and lower bar locations
se_plus <- function(x) mean(x) + sqrt(var(x)/length(x))
se_minus <- function(x) mean(x) - sqrt(var(x)/length(x))

#Yay stat_summary_bin to the rescue!
ggplot(data=dat, aes(cond, y = vals)) + 
  #stat_summary_bin(fun.y = "mean", geom = "bar") +
  stat_summary_bin(fun.y = "smean.cl.boot", geom = "bar") +
  stat_summary_bin(fun.y="mean_cl_boot", geom = "point")
  stat_summary_bin(fun.ymin = "se_minus", fun.ymax="se_plus", geom = "errorbar")



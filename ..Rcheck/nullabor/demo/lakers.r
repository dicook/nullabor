library(nullabor)

threept <- subset(lal, type == "3pt" & !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time", "team", "etype", "player", "points", "result", "x", "y")]

threept <- transform(threept, 
  x = x + runif(length(x), -0.5, 0.5),
  y = y + runif(length(y), -0.5, 0.5))
threept <- transform(threept, 
  r = sqrt((x - 25) ^ 2 + y ^ 2),
  angle = atan2(y, x - 25))

# Focus in on shots in the typical range
threept <- subset(threept, r > 20 & r < 39)

qplot(x, y, data = threept) + coord_equal()

angle_scale <- scale_x_continuous("Angle (degrees)", 
  breaks = c(0, 45, 90, 135, 180), limits = c(0, 180))

qplot(angle * 180 / pi, r, data = threept) + 
  angle_scale 
  
  last_plot() %+% lineup(null_lm(r ~ poly(angle, 2)), threept, n = 9) + 
  facet_wrap(~ .sample)

segment <- function(x, br) (x - br) * (x > br)
qplot(angle * 180 / pi, r, data = threept) + angle_scale +
  geom_smooth(method = lm, formula = y ~ x + segment(x, 90))

last_plot() %+% lineup(null_lm(r ~ angle + segment(angle, pi / 2)), n = 9) +  facet_wrap(~ .sample)

# Look at model residuals directly
#mod <- lm(r ~ poly(angle, 2), data = threept)
#inrange <- threept
#inrange$resid <- resid(mod)
#qplot(angle, resid, data = inrange)
#last_plot() %+% lineup(has_dist("resid", "normal", list(mean = 0, sd = 1))) + facet_wrap(~ .sample)
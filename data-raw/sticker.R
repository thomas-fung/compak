library(tidyverse)
set.seed(1)
a.sample <- c(rpois(10, 1), rpois(5, 7))
table(a.sample)
fit.compak <- compak_fitpmf(a.sample, bandwidth_optim = "KL")

results <- fit.compak$kernel.est

results_flat <- tibble(y = flatten_dbl(results)) %>%
  add_column(
    x = rep(0:max(a.sample), length(unique(a.sample))),
    loc = rep(unique(a.sample), rep(
      max(a.sample) + 1,
      length(unique(a.sample))
    ))
  )

p <- ggplot(results_flat) +
  geom_bar(aes(x = a.sample, y = ..count.. / sum(..count..)), data = tibble(a.sample = a.sample), size = 0.5) +
  # geom_rug(aes(x = loc, colour = factor(loc)))+
  geom_point(aes(x = loc, y = -0.015, colour = factor(loc)), size = 0.5) +
  geom_col(aes(
    x = x, y = y / max(a.sample + 1),
    fill = factor(loc),
    colour = factor(loc),
  ),
  alpha = 0.5, position = "dodge", size = 0.5,
  width = 0.7
  ) +
  geom_pointrange(aes(
    x = x, y = y, ymin = 0,
    ymax = y
  ),
  colour = "red", size = 0.1,
  data = tibble(x = fit.compak$x, y = fit.compak$f.cmp)
  ) +
  theme_void() +
  theme(legend.position = "none")

# library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
# showtext_auto()
library(hexSticker)
sticker(p,
  package = "compak", p_color = "black",
  p_size = 10, s_x = 1, s_y = .8, s_width = 1.7,
  s_height = 0.7,
  filename = "man/figures/compak.svg",
  # filename = "man/figures/compak.png",
  h_color = "black",
  url = "https://github.com/thomas-fung/compak",
  u_size = 0.9, h_fill = "white"
)
